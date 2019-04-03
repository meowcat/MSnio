data <- read.csv("C:\\Daten\\R scripts\\MSnio\\inst\\Spectrum_schemas - librarySchema.tsv", sep="\t",
                 stringsAsFactors = FALSE)

data <- data[,
             c("suggested.common.name.in.R",
               "MassBank")]

data <- data[4:86,]
data <- data[data$MassBank != "",]

colnames(data) <- c("map", "field")

data <- data[,c("field", "map")]
library(plyr)
items <- alply(data, 1, function(row)
  {
  item <- list()
  item$field <- row[["field"]]
  item$map <- row[["map"]]
  return(item)
})
names(items) <- NULL
items <- list(metadata = items)
yaml::write_yaml(items, "inst/schemas/schema_massbank_auto.yaml")
