outcome <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
head(outcome)
names(outcome)
outcome[,11]<- as.numeric(outcome[,11])
hist(outcome[,11])
outcome[,11][1:10]
