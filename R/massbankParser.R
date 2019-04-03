file <- "C:/Daten/R scripts/MSnio/inst/spectra/massbank/EA016614.txt"
lines_test <- readLines(file)
schema <- yaml.load_file("C:/Daten/R scripts/MSnio/inst/schemas/schema_massbank.yaml")

parserMassBank <- function(lines, schema)
{
  buffer <- c()
  record <- list()
  for(line in lines)
  {
    if(substr(line, 1,2) != '  ')
    {
      record <- processBuffer(record, buffer, schema)
      buffer <- c()
    }
    buffer <- c(buffer, line)
  }
  record <- processBuffer(record, buffer, schema)
  recordNames <- names(record)
  record <- lapply(seq_along(record), function(i)
    processBlocks(record[[i]], recordNames[[i]], schema))
  record <- lapply(seq_along(record), function(i)
    processTables(record[[i]], recordNames[[i]], schema))
  names(record) <- recordNames
  return(record)
}

processBuffer <- function(record, lines, schema)
{
  if(length(lines) == 0)
    return(record)
  regex_titleline <- '(.*?): (.*)'
  regex_blockline <- '(.*?) (.*)'
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

processBlocks <- function(content, title, schema)
{
  # is this a block entry? If yes, retrieve list if it exists
  # or create new one
  if(title %in% schema$parser$blocks)
  {
    blockData <- lapply(content, function(line) {
      block_entry <- sub(regex_blockline, '\\1', line)
      block_content <- sub(regex_blockline, '\\2', line)
      return(c(block_entry, block_content))
    })
    block <- lapply(blockData, `[[`, 2)
    names(block) <- lapply(blockData, `[[`, 1)
    content <- block
  }
  return(content)
}

processTables <- function(content, title, schema)
{
  # is this a block entry? If yes, retrieve list if it exists
  # or create new one
  if(title %in% schema$parser$tables)
  {
    # remove trailing whitespace (which defines the table continuation in MassBank)
    content <- gsub("^( +)", "", content)
    # read the table
    content <- read.csv(text=content, sep=' ')
  }
  return(content)
}