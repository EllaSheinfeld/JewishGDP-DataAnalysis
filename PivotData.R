normalizeDataFrame = function() {
  data = read.csv("FilteredData.csv", skip = 1, stringsAsFactors = FALSE)
  data = data[,1:89]
  
  #install.packages("reshape2")
  ids = head(names(data), n = 4)
  data2 = reshape2::melt(data, id = ids)
  data2 = tidyr::separate(data2, "variable", c("type", "year"), sep="\\.")
  data2[is.na(data2$year),]$year <- 0
  data2$year = as.numeric(data2$year) + 2000
  
  write.table(data2, file="PivotData.tsv", sep="\t")
}