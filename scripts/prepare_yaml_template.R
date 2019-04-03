data <- read.csv("C:\\Daten\\R scripts\\MSnio\\inst\\Spectrum_schemas - librarySchema.tsv", sep="\t",
                 stringsAsFactors = FALSE)

data <- data[,
             c("suggested.common.name.in.R",
               "format",
               "X.1",
               "MSIO",
               "CHEMINF.Ontology.Term",
               "Mass.spectrometry.Ontology.Term",
               "other.ontologies")]

data <- data[4:86,]
colnames(data) <- c("field", "format", "cardinality", "ontology_msio",
                    "ontology_cheminf", "ontology_ms", "ontology_other")

library(plyr)
items <- alply(data, 1, function(row)
  {
  item <- list()
  item$field <- row[["field"]]
  item$format <- row[["format"]]
  item$cardinality <- row[["cardinality"]]
  ont <- row[c("ontology_msio", "ontology_cheminf", "ontology_ms", "ontology_other")]
  ont <- ont[ont != ""]
  ont <- sub("^(.*?) (.*)", "\\1", ont)
  if(length(ont) > 0)
    item$ontology <- ont
  return(item)
})
names(items) <- NULL

yaml::write_yaml(items, "inst/schemas/items.yaml")
