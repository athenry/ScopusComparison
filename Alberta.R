##Install and load needed packages

##install.packages("bibliometrix", dependencies = TRUE)
##install.packages("splitstackshape")
##install.packages("tidyverse")

library(bibliometrix)
library(splitstackshape)
library(tidyverse)
library(stringr)

## Read in downloaded files and convert to dataframe
filePathsAB = dir("./Alberta", pattern = "*.bib", recursive = TRUE, full.names = TRUE) 
DAB <- do.call("readFiles", as.list(filePathsAB)) 
MAB <- convert2df(DAB, dbsource = "scopus", format = "bibtex")

## Keep only selected columns: UT, DT, C1, DT, TC, PY
mydataAB <- select(MAB, UT, C1, DT, PY, TC)

## Separate authors into single observations
tidy_dataAB <- cSplit(mydataAB, "C1", sep = ";", direction = "long")

##Test that there were no unintended drops
count <- sum(str_count(mydataAB$C1, ";"))
ifelse(count + nrow(mydataAB) == nrow(tidy_dataAB), "No drops", "Warning") 

## Remove non-Alberta addresses
ABData <- tidy_dataAB[grep("UNIVERSITY OF ALBERTA", tidy_dataAB$C1), ]
engDataAB <- ABData[grep("ENGINEERING", ABData$C1), ]

deptURL <- "https://docs.google.com/spreadsheets/d/e/2PACX-1vSwbFiCTwa2Kd5r62CK04AC7iRfLTDSt18PWe76bO4QSKGqpHkb0I7ZeSesCZN_wGN_NVytVi0x0h1J/pub?gid=0&single=true&output=csv"
depts <- read.csv(deptURL)

abs <- as.character(depts$Abbreviation)
dept_test <- sapply(engDataAB$C1, function(x) abs[str_detect(x, abs)])

engDataAB<-cbind(engDataAB,plyr::ldply(dept_test,rbind)[,1])
names(engDataAB)[6]<-"Abbreviation"
engDeptData <- merge(engDataAB, depts, all.x = TRUE) ##keeps nonmatches and enters NA

## check the "other"s for articles that should be kept
Other <- filter(engDeptData, is.na(Department))
View(Other)

## ##Keep only eng departments and output data to file
finalEngData <- engDeptData[complete.cases(engDeptData), ]
engDataDD <- unique(select(finalEngData, UT, DT, TC, PY, Department))
write.csv(engDataDD, "Alberta.csv", quote = TRUE, row.names = FALSE)

