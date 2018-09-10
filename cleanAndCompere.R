mergedData = read.csv("merged_orgs_before_cleanup.csv", stringsAsFactors = FALSE)
existingOrgs = mergedData[1:738,]$Organization
searchOrgs = mergedData$Org

cleanData = function(data) {
  clean = gsub(", Inc\\.$", "", data)
  clean = gsub(" Inc$", "", clean)
  clean = gsub("^The", "", clean)
  clean = tolower(clean)
  clean = trimws(clean)
  clean
}

cleanExistingOrgs = cleanData(existingOrgs)
cleanSearchOrgs = cleanData(searchOrgs)

res = intersect(cleanExistingOrgs, cleanSearchOrgs)
res2 = pmatch(cleanExistingOrgs, cleanSearchOrgs)
