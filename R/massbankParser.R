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
.massbankParseRecord <- function(lines)
{
  buffer <- c()
  record <- list()
  for(line in lines)
  {
    if(substr(line, 1,2) != '  ')
    {
      record <- .massbankParseProcessBuffer(record, buffer)
      buffer <- c()
    }
    buffer <- c(buffer, line)
  }
  record <- .massbankParseProcessBuffer(record, buffer)
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
.massbankParseProcessBuffer <- function(record, lines)
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


.massbankRuleParseBlock <- function(content)
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

.massbankRuleParseTable <- function(content)
{
  content <- gsub("^( +)", "", content)
  content <- read.csv(text=content, sep=' ')
  return(content)
}


.massbankRuleRenderBlock <- function(content)
{
  paste(names(content), content)
}

.massbankParseSchema <- function(record, schema, parser)
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
      field <- parser$rulesParse[[element$rule]](field)
    if(!is.null(element$node))
    {
      field <- lapply(element$node, .recurseFields, field)
      names(field) <- unlist(lapply(element$node, `[[`, 'field'))
      field <- field[!unlist(lapply(field, is.null))]
    } 
    return(field)
  }
  schemaRoot <- schema$metadata
  record <- lapply(schemaRoot, .recurseFields, record)
  names(record) <- unlist(lapply(schemaRoot, `[[`, 'field'))
  record <- record[!unlist(lapply(record, is.null))]
  return(record)
}

.massbankRenderSchema <- function(record, schema, parser)
{
  # go through metadata specification
  # for every element in schema$metadata:
  # when reading, first apply rule, then recurse nodes,
  # here we are writing, so first recurse nodes, then apply rule
  
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
      field <- unlist(parser$rulesRender[[element$rule]](field))
    # the following "default rule" actually reverses the behaviour of 
    # "field <- unlist(data[names(data) == element$field])"
    # in the parser step
    names(field) <- rep(element$field,length(field))
    return(field)
  }
  schemaRoot <- schema$metadata
  record <- lapply(schemaRoot, .recurseFields, record)
  names(record) <- unlist(lapply(schemaRoot, `[[`, 'field'))
  record <- record[!unlist(lapply(record, is.null))]
  record <- as.list(do.call(c, c(record, use.names=FALSE)))
  
  return(record)
}

.massbankRenderRecord <- function(record)
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



#' Generate a MassBank record parser
#' 
#' @return
#' @export
#'
#' @examples
parserMassBank <- function()
{
  list(
  renderRecord = .massbankRenderRecord,
  renderSchema = .massbankRenderSchema,
  rulesRender = list(
    "block" = .massbankRuleRenderBlock
  ),
  parseRecord = .massbankParseRecord,
  parseSchema = .massbankParseSchema,
  rulesParse = list(
    #    "default" = function(content) content,
    "table" = .massbankRuleParseTable,
    "block" = .massbankRuleParseBlock
  )
)}

#' @title Massbank parser
#'
#' @description
#'
#' Parser for Massbank spectrum files.
#'
#' New objects are supposed to be created with the `MassbankParser` function.
#'
#' @author Michele Stravs, Johannes Rainer
#' 
#' @name MassbankParser
#'
#' @exportClass MassbankParser
#'
#' @examples
#' library(MSnio)
#'
#' ## Create and intialize a parser
#' prs <- MassbankParser()
#'
#' ## Spectrum file
#' fl <- system.file("spectra/massbank/EA016614.txt", package="MSnio")
#' res <- importSpectrum(prs, fl)
setClass("MassbankParser",
         contains = "SpectrumParser")

#' @rdname MassbankParser
setMethod("importSpectrum", "MassbankParser", function(object, file, ...) {
    if (!file.exists(file))
        stop("File ", file, " not found")
    if (length(object@schema) == 0)
        stop("Parser does not have any schema")
    tkns <- .massbankParseRecord(readLines(file))
    ## map to "standard" fields, return a Spectra/Spectrum/whatever
    tkns
})

setValidity("MassbankParser", function(object) {
    msg <- NULL
    ## Would be nice to check the schema for correctness or similar.
    if (length(msg)) msg
    else TRUE
})


#' @rdname MassbankParser
#'
#' @export MassbankParser
MassbankParser <- function() {
    new("MassbankParser",
        schema = yaml.load_file(system.file("schemas/schema_massbank_auto.yaml",
                                            package="MSnio")))
}
