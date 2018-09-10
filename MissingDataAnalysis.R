missingInColumns <- function(){  
  data = read.csv("FilteredData.csv", skip = 1, stringsAsFactors = FALSE)
  data = data[,1:89]
  columnesWithMissing = sapply(data, function(x) {
    sum(is.na(x)) + 
      sum(trimws(x) == "$0", na.rm = TRUE) + 
      sum(trimws(x) == "0", na.rm = TRUE) + 
      sum(trimws(x) == 0, na.rm = TRUE)
    })
  
  missingSummary = data.frame(columnesWithMissing)
  names(missingSummary) <= c("Count")
  renames = row.names(missingSummary)
  
  renames = lapply(renames, function(x) { 
      res = unlist(strsplit(x, "\\."))
      lastPart = res[length(res)]
      numVal = as.numeric(lastPart)
      if(!is.na(numVal)){
        res[length(res)] = as.character(numVal + 2000)
      }
      paste(res, collapse = ".")
    })
  renames = unlist(renames)
  row.names(missingSummary) <- renames
  
  
  write.csv(missingSummary, file = "missing_data_in_columns.csv")
}

missingInOrg = function() {
  data = read.csv("FilteredData.csv", skip = 1, stringsAsFactors = FALSE)
  data = data[,1:89]
  
  data2 = t(data)
  data2 = data.frame(data2, stringsAsFactors = FALSE)
  
  columnesWithMissing = sapply(data2, function(x) {
    sum(is.na(x), na.rm = TRUE) + sum(x == "", na.rm = TRUE)#+ 
      #sum(trimws(x) == "$0", na.rm = TRUE) + 
      #sum(trimws(x) == "0", na.rm = TRUE) 
  })
  
  missingSummary = data.frame(columnesWithMissing)
  row.names(missingSummary) <- data$Organization
  names(missingSummary) <- c("Missing values")
  
  missingSummary
} 


missingRevenueAndExpensesInOrg = function() {
  data = read.csv("FilteredData.csv", skip = 1, stringsAsFactors = FALSE)
  data = data[,1:89]
  
  data = data[, sort(c(1,grep("Revenue", names(data)), grep("Expenses", names(data)) ))]
  
  data2 = t(data)
  data2 = data.frame(data2, stringsAsFactors = FALSE)
  
  columnesWithMissing = sapply(data2, function(x) {
    sum(is.na(x)) #+ 
    #sum(trimws(x) == "$0", na.rm = TRUE) + 
    #sum(trimws(x) == "0", na.rm = TRUE) 
  })
  
  missingSummary = data.frame(columnesWithMissing)
  row.names(missingSummary) <- data$Organization
  names(missingSummary) <- c("Missing values")
  
  missingSummary
} 

