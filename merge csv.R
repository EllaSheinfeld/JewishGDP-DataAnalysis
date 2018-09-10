setwd("./search results/")
filenames <- list.files(full.names=TRUE)
All <- lapply(filenames,function(i){
  read.csv(i, header=TRUE, sep="\t")
})
df <- do.call(rbind.data.frame, All)
write.table(df,"all_searchresults.tsv", sep ="\t", row.names=FALSE)
setwd("../")