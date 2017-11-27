##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsOtt = dir("./Ottawa", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DOtt <- do.call("readFiles", as.list(filePathsOtt)) 
MOtt <- convert2df(DOtt, dbsource = "scopus", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataOtt <- select(MOtt, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataOtt <- cSplit(mydataOtt, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataOtt$C1, ";"))
ifelse(count + nrow(mydataOtt) == nrow(tidy_dataOtt), "No drops", "Warning") 

## Remove non-Ottawa addresses
OttData <- tidy_dataOtt[grep("UNIVERSITY OF OTTAWA", tidy_dataOtt$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwbFiCTwa2Kd5r62CK04AC7iRfLTDSt18PWe76bO4QSKGqpHkb0I7ZeSesCZN_wGN_NVytVi0x0h1J/pub?gid=2091817760&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(OttData$C1, function(x) abs[str_detect(x, abs)])

engData<-cbind(OttData,plyr::ldply(dept_test,rbind)[,1])
names(engData)[6]<-"Abbreviation"
engDeptData <- merge(engData, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Ottawa.csv", quote = TRUE, row.names = FALSE)