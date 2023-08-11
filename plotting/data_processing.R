library(reshape2)

# https://stackoverflow.com/questions/9564489/read-all-files-in-a-folder-and-apply-a-function-to-each-data-frame
read_from_csv <- function() {
  filenames <-
    list.files("./input/", pattern = "*.csv", full.names = TRUE)
  dataframes <- lapply(filenames, read.csv, sep = ";")
  id_vars <-
    c("first",
      "second",
      "tupleType",
      "plagType",
      "plagTypeGroup",
      "tool")
  melted_dataframes <- lapply(dataframes, melt, id.vars = id_vars)
  
  data_all <- do.call("rbind", melted_dataframes)
  return(data_all)
}

# hacky solution to plot original tuples together with plagiarisms
copy_originals_for_each_plag_type <- function(data) {
  plagTypes <- unique(data$plagType)
  originals <- subset(data, tupleType == "Original-Original")
  
  copied_originals <- data.frame()
  for (plagType in plagTypes[plagTypes != ""])
  {
    copy <- data.frame(originals)
    copy$plagType <- plagType
    plagTypeGroup <- plagType
    copy$plagTypeGroup <- plagTypeGroup
    copied_originals <- rbind(copied_originals, copy)
  }
  data_all <- rbind(data, copied_originals)
  return(data_all)
}

# Add approach column from tool
add_approach_label <- function(data) {
  levels <-
    c("06SAGLAM",
      "07SAGLAM",
      "08SAGLAM",
      "martinez")
  data$approach <- factor(data$tool, levels = levels)
  return(data)
}

# Label the tuple types
add_tuple_type_label <- function(data) {
  tuple_type <- c(
    "Original-Original" = "Unrelated Pairs",
    "Plag-Original" = "Plagiarism-to-Source",
    "Plag-Plag" = "Plagiarism-to-Plagiarism"
  )
  levels <-
    c("Unrelated Pairs",
      "Plagiarism-to-Source",
      "Plagiarism-to-Plagiarism")
  
  data$tupleTypeLabel <- tuple_type[data$tupleType]
  data$tupleTypeLabel <-
    factor(data$tupleTypeLabel, levels = levels)
  return(data)
}
