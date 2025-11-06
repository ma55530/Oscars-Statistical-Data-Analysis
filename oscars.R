data = read.csv("oscars_dataset.csv", sep=";")
var_types <- sapply(data, class)
cat(paste(names(var_types), ": ", var_types, sep = "", collapse = "\n"))