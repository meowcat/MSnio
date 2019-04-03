library(yaml)

schema <- yaml.load_file("schema_test.yaml")


library(MSnbase)
data("itraqdata")

specs <- as.list(assayData(itraqdata))

sp <- specs[[4]]

# todo: metadata<- etc
sp@metadata <- list(
  name = "Compound",
  compound_class = "natural product",
  formula = "C6H12O6",
  cas = c("666-66-6", "7777-77-7", "8888-88-8"),
  not_mapped = "not_mapped"
)

#
schema_export <- function(data, schema, fill = TRUE)
{
  recurse <- function(entry)
  {
    if(is.list(entry))
    {
      l <- lapply(entry, recurse)
      #names(l) <- names(entry)
      # eliminate NULLs
      l <- l[which(!unlist(lapply(l, is.null)))]
      return(l)
    }
    else
    {
      if(!is.null(data[[entry]]))
        return(data[[entry]])
      else if(fill)
        return(NA)
      else
        return(NULL)
    }
  }
  recurse(schema)
}

metadata_identity <- schema_export(sp@metadata, schema)
metadata_identity <- schema_export(sp@metadata, schema, fill=FALSE)

schema_tree <- yaml.load_file("schema_tree.yaml")
metadata_tree <- schema_export(sp@metadata, schema_tree)
metadata_tree <- schema_export(sp@metadata, schema_tree, fill=FALSE)

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

.recurse_access(schema_tree, c('descriptors','formula'))
.recurse_access(schema_tree, c('descriptors','not_there'))
.recurse_access(schema_tree, c('descriptors','formula', 'too_far'))
try(.recurse_access(schema_tree, c()))
.recurse_access(schema_tree, c('not_there'))
.recurse_access(schema_tree, c('not_there', 'not_there_either'))


#
schema_import <- function(data, schema, fill=TRUE)
{
  data_env <- new.env()
  
  recurse <- function(entry, trace = c())
  {
    print(trace)
    if(is.list(entry))
    {
      l <- lapply(
        seq_along(entry),
        function(i)
          recurse(entry[[i]], c(trace, names(entry)[[i]]))
      )
      #names(l) <- names(entry)
      # eliminate NULLs
      # l <- l[which(!unlist(lapply(l, is.null)))]
      # return(l)
    }
    else
    {
      print('!')
      entry_data <- .recurse_access(data, c(trace, names(entry)))
      if(!is.null(entry_data))
        data_env[[entry]] <- entry_data
      else if(fill)
        data_env[[entry]] <- NA
    }
  }
  recurse(schema)
  return(as.list(data_env))
}


metadata_identity_imp <- schema_import(sp@metadata, schema)
metadata_tree_imp <- schema_import(metadata_tree, schema_tree)
