##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsWest = dir("./Western", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DWest <- do.call("readFiles", as.list(filePathsWest)) 
MWest <- convert2df(DWest, dbsource = "scopus", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataWest <- select(MWest, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataWest <- cSplit(mydataWest, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataWest$C1, ";"))
ifelse(count + nrow(mydataWest) == nrow(tidy_dataWest), "No drops", "Warning") 

## Remove non-Western addresses
WestData1 <- tidy_dataWest[grep("UNIVERSITY OF WESTERN ONTARIO", tidy_dataWest$C1), ]
WestData2 <- tidy_dataWest[grep("WESTERN UNIVERSITY", tidy_dataWest$C1), ]
WestData <- rbind(WestData1, WestData2)

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwbFiCTwa2Kd5r62CK04AC7iRfLTDSt18PWe76bO4QSKGqpHkb0I7ZeSesCZN_wGN_NVytVi0x0h1J/pub?gid=164215798&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(WestData$C1, function(x) abs[str_detect(x, abs)])

engData<-cbind(WestData,plyr::ldply(dept_test,rbind)[,1])
names(engData)[6]<-"Abbreviation"
engDeptData <- base::merge(engData, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Western.csv", quote = TRUE, row.names = FALSE)