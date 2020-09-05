# PRE-PROCESSING QUERIES AND PAGES DATA

##
#Queries:
queries <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/Wojcik Coded Flu Files/queries_finalSW_coded.csv", na.strings=c("NA", ""), stringsAsFactors = F)
queries$Carolina <- as.factor(toupper(queries$Carolina))
queries$Isys <- as.factor(queries$Isys)
queries$Final.Code[which(is.na(queries$Final.Code))] <- queries$Carolina[which(is.na(queries$Final.Code))]

#queries$Isys[which(queries$Isys=="?")] = NA
names(queries)[which(names(queries)=="Isys")] = "Coder1"
names(queries)[which(names(queries)=="Carolina")] = "Coder2"
##

##
#Queries
#queries$QID2 <- as.factor(queries$QID2)
#Overall intercoder reliability, excluding NA's
#queries <- na.omit(queries[, c("query", "QID2", "Coder2", "Coder1", "Final.Code")])
##
saveRDS(queries, "~/Documents/ms_flu/Survey and MRP/data/queries.rds")

##
#Pages
pages <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/Wojcik Coded Flu Files/pages_finalSW_coded.csv", na.strings=c("NA", ""), stringsAsFactors = F )
pages$Carolina <- as.factor(toupper(pages$Carolina))

#pages$Isys[which(pages$Isys=="?")] = NA
#pages$Isys <- as.factor(pages$Isys)
pages$Final.Code[which(is.na(pages$Final.Code))] <- pages$Carolina[which(is.na(pages$Final.Code))]
names(pages)[which(names(pages)=="Isys")] = "Coder1"
names(pages)[which(names(pages)=="Carolina")] = "Coder2"
##

##
#PAGES
pages$QID2 <- as.factor(pages$QID2)
#Overall intercoder reliability, excluding NA's
pages <- na.omit(pages[, c("QID2", "Coder2", "Coder1", "Final.Code")])

saveRDS(pages, "~/Documents/ms_flu/Survey and MRP/data/pages.rds")


# EXPANDED QUERIES:::

# initial on NYS
exp_qs = read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Expanded Queries/NYSQueries_to_adjudicate.csv")
names(exp_qs)[which(names(exp_qs)=="castillo")] = "Coder1"
names(exp_qs)[which(names(exp_qs)=="leyla")] = "Coder2"
exp_qs$Coder2 = factor(exp_qs$Coder2, levels = c(levels(exp_qs$Coder2), "b2"))
exp_qs <- na.omit(exp_qs[, c("query", "Coder1", "Coder2")])
saveRDS(exp_qs, "~/Documents/ms_flu/data/expanded_queries.rds")

# final
exp_qs_final = read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Expanded Queries/Expanded_Queries12_FINAL.csv")
saveRDS(exp_qs_final, "~/Documents/ms_flu/data/expanded_queries_final.rds")

# MERGE FULL DATASET FOR ML MODELING
#Queries:
queries <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/Wojcik Coded Flu Files/queries_finalSW_coded.csv", na.strings=c("NA", ""), stringsAsFactors = F)
queries$Carolina <- as.factor(toupper(queries$Carolina))
queries$Isys <- as.factor(queries$Isys)
queries$Final.Code[which(is.na(queries$Final.Code))] <- queries$Carolina[which(is.na(queries$Final.Code))]
# Pages:
pages <- read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Source Data/Coding Sample/Wojcik Coded Flu Files/pages_finalSW_coded.csv", na.strings=c("NA", "") )
pages$Carolina <- as.factor(toupper(pages$Carolina))
pages$Isys <- as.factor(pages$Isys)
pages$Final.Code[which(is.na(pages$Final.Code))] <- pages$Carolina[which(is.na(pages$Final.Code))]
##
#open panel demographics:
dat = read.csv("~/Google Drive/papers/Working Projects/Lazer Lab/Flu/Clean Data/panel_demographics.csv")
# the query data
full = rbind(na.omit(pages[, c("QID2", "Final.Code")]), na.omit(queries[, c("QID2", "Final.Code")]))
##
# Initial merge to match respondents to queries
full = merge(full, dat[, c("RID", "QID2")], by="QID2")
# Aggregate to get the total number of each type of search per individual
c = model.matrix(~0+Final.Code, data=full) # binarizes the flu query code 
full = cbind(c, full) # binds data to full data
agg = aggregate(cbind(Final.CodeA1, Final.CodeA2, Final.CodeB1, Final.CodeB2, Final.CodeC1, Final.CodeD)~RID, sum, data=full) # aggregate number of searches per RID

# Final merge to get respondents to query volumes
full = merge(agg, dat, by="RID")
full$X = NULL

saveRDS(full, "/Users/electron/Documents/ms_flu/Survey and MRP/data/ml_modeling.rds")
