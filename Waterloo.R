##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsWat = dir("./Waterloo", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DWat <- do.call("readFiles", as.list(filePathsWat)) 
MWat <- convert2df(DWat, dbsource = "scopus", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataWat <- select(MWat, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataWat <- cSplit(mydataWat, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataWat$C1, ";"))
ifelse(count + nrow(mydataWat) == nrow(tidy_dataWat), "No drops", "Warning") 

## Remove non-Waterloo addresses
WatData <- tidy_dataWat[grep("UNIVERSITY OF WATERLOO", tidy_dataWat$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwbFiCTwa2Kd5r62CK04AC7iRfLTDSt18PWe76bO4QSKGqpHkb0I7ZeSesCZN_wGN_NVytVi0x0h1J/pub?gid=985703843&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(WatData$C1, function(x) abs[str_detect(x, abs)])

engData<-cbind(WatData,plyr::ldply(dept_test,rbind)[,1])
names(engData)[6]<-"Abbreviation"
engDeptData <- merge(engData, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Waterloo.csv", quote = TRUE, row.names = FALSE)