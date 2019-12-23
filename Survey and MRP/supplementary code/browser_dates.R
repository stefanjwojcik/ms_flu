# looking at the timing of searches in the panel

searches <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/MSWebSearchesSortedByQID2&Date_Wojcik_360_coded.csv", stringsAsFactors = F)
searchdates <- as.Date(searches$serp_capture_time, "%m/%d/%Y")

pages <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/MSWebPagesSortedByQID2&Date_wojcik_coded.csv", stringsAsFactors = F, sep="\t")
pagedates <- as.Date(pages$pagestartlocaltime, "%m/%d/%Y")


alldates <- c(searchdates, pagedates)

min(alldates, na.rm=T)
max(alldates, na.rm=T)
