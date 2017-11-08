##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsCal = dir("./Calgary", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DCal <- do.call("readFiles", as.list(filePathsCal)) 
MCal <- convert2df(DCal, dbsource = "scopus", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataCal <- select(MCal, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataCal <- cSplit(mydataCal, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataCal$C1, ";"))
ifelse(count + nrow(mydataCal) == nrow(tidy_dataCal), "No drops", "Warning") 

## Remove non-Calgary addresses
CalData <- tidy_dataCal[grep("UNIVERSITY OF CALGARY", tidy_dataCal$C1), ]
engDataCal <- CalData[grep("ENGINEERING", CalData$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwbFiCTwa2Kd5r62CK04AC7iRfLTDSt18PWe76bO4QSKGqpHkb0I7ZeSesCZN_wGN_NVytVi0x0h1J/pub?gid=734988134&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(engDataCal$C1, function(x) abs[str_detect(x, abs)])

engDataCal<-cbind(engDataCal,plyr::ldply(dept_test,rbind)[,1])
names(engDataCal)[6]<-"Abbreviation"
engDeptData <- merge(engDataCal, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Calgary.csv", quote = TRUE, row.names = FALSE)

