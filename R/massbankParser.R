file <- "inst/spectra/massbank/EA016614.txt"
lines_test <- readLines(file)
schema <- yaml.load_file("inst/schemas/schema_massbank_auto.yaml")
fields <- yaml.load_file("inst/schemas/fields.yaml")


buildSkeleton <- function(schema, fields)
{
  field_names <- unlist(lapply(fields, `[[`, "field"))
  names(fields) <- field_names
  skeleton <- schema
  
}

.massbank.readFile <- function(lines)
{
  buffer <- c()
  record <- list()
  for(line in lines)
  {
    if(substr(line, 1,2) != '  ')
    {
      record <- processBuffer(record, buffer)
      buffer <- c()
    }
    buffer <- c(buffer, line)
  }
  record <- .massbank.processBuffer(record, buffer)
  # recordNames <- names(record)
  # record <- lapply(seq_along(record), function(i)
  #   processBlocks(record[[i]], recordNames[[i]], schema))
  # record <- lapply(seq_along(record), function(i)
  #   processTables(record[[i]], recordNames[[i]], schema))
  # names(record) <- recordNames
  return(record)
}

.massbank.processBuffer <- function(record, lines)
{
  if(length(lines) == 0)
    return(record)
  regex_titleline <- '(.*?): (.*)'
  title <- sub(regex_titleline, '\\1', lines[1])
  line_rest <- sub(regex_titleline, '\\2', lines[1])
  content <- c(line_rest, lines[-1])
  entry <- record[[title]]
  if(is.null(entry))
    entry <- c()
  entry <- c(entry, content)
  record[[title]] <- entry
  return(record)
}

.massbank.rule_read.block <- function(content)
{
  regex_blockline <- '(.*?) (.*)'
  blockData <- lapply(content, function(line) {
    block_entry <- sub(regex_blockline, '\\1', line)
    block_content <- sub(regex_blockline, '\\2', line)
    return(c(block_entry, block_content))
  })
  block <- lapply(blockData, `[[`, 2)
  names(block) <- lapply(blockData, `[[`, 1)
  content <- block
  return(content)
}

.massbank.rule_read.table <- function(content)
{
  content <- gsub("^( +)", "", content)
  content <- read.csv(text=content, sep=' ')
  return(content)
}


parserMassBank <- list(
  render = NULL,
  rules_render = NULL,
  read = .massbank.readFile,
  rules_read = list(
#    "default" = function(content) content,
    "table" = .massbank.rule_read.table,
    "block" = .massbank.rule_read.block
  )
)


.recurse_access <- function(data, trace)
{
  if(length(trace) == 0)
    stop("Trace is absent")
  if(length(trace) == 1)
    return(data[[trace]])
  else
    if(is.list(data[[trace[[1]]]]))
      return(.recurse_access(data[[trace[1]]], trace[-1]))
  else
    return(NULL)
}

applySchema <- function(record, schema, parser)
{
  # go through metadata specification
  # for every element in schema$metadata:
  # if "rule" is NULL, rule <- default
  # parser$rules_read[[rule]](.recurse_access(rawRecord, trace))
  # if "node" is not NULL
  .recurseFields <- function(element, data)
  {
    #trace <- c(trace, element$field)
    #field <- .recurse_access(record, trace)
    field <- data[[element$field]]
    if(!is.null(element$rule))
      field <- parser$rules_read[[element$rule]](field)
    if(!is.null(element$node))
    {
      field <- lapply(element$node, .recurseFields, field)
      names(field) <- unlist(lapply(element$node, `[[`, 'field'))
      field <- field[!unlist(lapply(field, is.null))]
    } 
    return(field)
  }
  schema_root <- schema$metadata
  record <- lapply(schema_root, .recurseFields, record)
  names(record) <- unlist(lapply(schema_root, `[[`, 'field'))
  record <- record[!unlist(lapply(record, is.null))]
  return(record)
}