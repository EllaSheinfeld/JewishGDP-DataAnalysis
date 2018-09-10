interpolateData = function(maxgep = 1){
  data = read.csv("FilteredData.csv", skip = 1, stringsAsFactors = FALSE)
  data = data[,1:89]
  
  interpolateRow = function(row){
    zooData = zoo::zoo(as.numeric(row))
    
    zoo::na.approx(zooData,na.rm=FALSE, maxgap = maxgep)
  }
  
  #### Revenue ####
  revenueData = data[,grepl("Revenue", names(data))]
  revenueData[revenueData == ""] = NA
  
  res = apply(revenueData, 1, interpolateRow)
  
  res = as.data.frame(t(res))
  message(paste("Missing revenue values before interpolation:",
                sum(is.na(revenueData)),
                "after interpulation:",
                sum(is.na(res))
                ))
  
  data[,grepl("Revenue", names(data))] <- res
  
  #### Expenses ####
  expensesData = data[,grepl("Expenses", names(data))]
  expensesData[expensesData == ""] = NA
  
  res = apply(expensesData, 1, interpolateRow)
  
  res = as.data.frame(t(res))
  message(paste("Missing Expenses values before interpolation:",
                sum(is.na(expensesData)),
                "after interpulation:",
                sum(is.na(res))
  ))
  
  data[,grepl("Expenses", names(data))] <- res
  
  ####Net Assets####
  netData = data[,grepl("Net", names(data))]
  netData[netData == ""] = NA
  
  res = apply(netData, 1, interpolateRow)
  
  res = as.data.frame(t(res))
  message(paste("Missing Net values before interpolation:",
                sum(is.na(netData)),
                "after interpulation:",
                sum(is.na(res))
  ))
  
  data[,grepl("Net", names(data))] <- res
  
  ####Employees####
  employeesData = data[,grepl("Employees", names(data))]
  employeesData[employeesData == ""] = NA
  
  res = apply(employeesData, 1, interpolateRow)
  
  res = as.data.frame(t(res))
  message(paste("Missing Employees values before interpolation:",
                sum(is.na(employeesData)),
                "after interpulation:",
                sum(is.na(res))
  ))
  
  data[,grepl("Employees", names(data))] <- res
  
  ####Program Services####
  programData = data[,grepl("Program", names(data))]
  programData[programData == ""] = NA

  res = apply(programData, 1, interpolateRow)
  
  res = as.data.frame(t(res))
  message(paste("Missing Program values before interpolation:",
                sum(is.na(programData)),
                "after interpulation:",
                sum(is.na(res))
  ))
  
  data[,grepl("Program", names(data))] <- res
  
  
  #### Write to CSV ####
  
  write.csv(data, file=paste0("FilteredDataOutput_", maxgep,".csv"), na = "")
}
