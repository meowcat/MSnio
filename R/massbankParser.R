# file <- "inst/spectra/massbank/EA016614.txt"
# lines_test <- readLines(file)
# schema <- yaml.load_file("inst/schemas/schema_massbank_auto.yaml")
# fields <- yaml.load_file("inst/schemas/fields.yaml")
# 

#' MassBank parser - line parser
#'
#' @param lines Character vector with the record in line-by-line format.
#'
#' @return List of root-level tag entries (name: tag name, content: unprocessed tag value)
#' @export
#'
#' @examples
.massbank.readFile <- function(lines)
{
  buffer <- c()
  record <- list()
  for(line in lines)
  {
    if(substr(line, 1,2) != '  ')
    {
      record <- .massbank.processBuffer(record, buffer)
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

#' MassBank parser - line buffer processing
#' 
#' This receives a single or multiple line entry
#' (MassBank multiline entries continue the preceding
#' line with starting whitespace, see record spec.)
#' 
#' It parses the multiline buffer into tag name and 
#' vector of lines, without further processing, and updates
#' the record accordingly.
#'
#' @param record 
#' @param lines 
#'
#' @return
#' @export
#'
#' @examples
.massbank.processBuffer <- function(record, lines)
{
  if(length(lines) == 0)
    return(record)
  regex_titleline <- '(.*?): (.*)'
  title <- sub(regex_titleline, '\\1', lines[1])
  line_rest <- sub(regex_titleline, '\\2', lines[1])
  content <- list(c(line_rest, lines[-1]))
  # entry <- record[[title]]
  # if(is.null(entry))
  #   entry <- c()
  # entry <- c(entry, content)
  # record[[title]] <- entry
  names(content) <- title
  record <- c(record, content)
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


.massbank.rule_write.block <- function(content)
{
  paste(names(content), content)
}



parserMassBank <- list(
  render = NULL,
  rules_render = list(
    "block" = .massbank.rule_write.block
  ),
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
  # if "node" is not NULL, recurse
  .recurseFields <- function(element, data)
  {
    #trace <- c(trace, element$field)
    #field <- .recurse_access(record, trace)
    #field <- data[[element$field]]
    field <- unlist(data[names(data) == element$field])
    
    # consider removing this, so names would be downported into the arrays. 
    # Maybe even put the trace here
    names(field) <- NULL
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

renderSchema <- function(record, schema, parser)
{
  # go through metadata specification
  # for every element in schema$metadata:
  # when reading, first apply rule, then recurse nodes,
  # here we are writing, so first recurse nodes, then apply rule
  
  # we need a helper to fix the naming, currently, because I have no better idea
  name_helper <- "&"
  
  .recurseFields <- function(element, data)
  {
    # access the field in the built record (now no multiselection,
    # since it was rendered into one element)
    field <- data[[element$field]]
    if(length(field) == 0)
      return(NULL)

    if(!is.null(element$node))
    {
      field <- lapply(element$node, .recurseFields, field)
      names(field) <- unlist(lapply(element$node, `[[`, 'field'))
      field <- field[!unlist(lapply(field, is.null))]
      field <- do.call(c, c(field, use.names=FALSE))
      #field <- unlist(field)
    } 
    if(!is.null(element$rule))
      field <- unlist(parser$rules_render[[element$rule]](field))
    # the following "default rule" actually reverses the behaviour of 
    # "field <- unlist(data[names(data) == element$field])"
    # in the parser step
    names(field) <- rep(element$field,length(field))
    return(field)
  }
  schema_root <- schema$metadata
  record <- lapply(schema_root, .recurseFields, record)
  names(record) <- unlist(lapply(schema_root, `[[`, 'field'))
  record <- record[!unlist(lapply(record, is.null))]
  record <- as.list(do.call(c, c(record, use.names=FALSE)))
  
  return(record)
}

.massbank.writeLines <- function(record)
{
  buffer <- c()
  for(i in seq_along(record))
  {
    # find tag name and values
    tag <- names(record)[[i]]
    entry <- record[[i]]
    # paste title to first line in multiline entries
    entry[1] <- paste(tag, entry[1], sep=": ")
    buffer <- c(buffer, entry)
  }
  return(buffer)
}

