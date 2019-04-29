rm(list = ls())
rankall <- function(outcome, num) {
  #to remove
  num <- 5
  outcome <-'heart failure'
  
  #-----------------------------------------
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  illness<- c('heart attack', 'heart failure', 'pneumonia')
  outcome.brief <- outcome.all[,c(2,7,11,17,23)]
  outcome.brief[, 3] <- as.numeric(outcome.brief[,3])
  outcome.brief[, 4] <- as.numeric(outcome.brief[,4])
  outcome.brief[, 5] <- as.numeric(outcome.brief[,5])
  names(outcome.brief) <- c('hospital.name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')
  #---------------------------------  
  if (any(outcome==illness)){
    condition <- sub(' ', '.', outcome)
    outcome.illness.subset <- cbind(outcome.brief[,c(1,2)], outcome.brief[[condition]])
    names(outcome.illness.subset)[3]<- condition
    outcome.illness.subset<-outcome.illness.subset[which(!is.na(outcome.illness.subset[,3])),]
    outcome.illness.subset$state<-factor(outcome.illness.subset$state)
    outcome.sorted <- outcome.illness.subset[with(outcome.illness.subset, order(state, heart.failure, hospital.name)),]
    head(outcome.sorted)
    
    
    aaa<-tapply(outcome.sorted$heart.failure, outcome.sorted$state, unique)
    
    for (i in 1: length(state.abb)){
     
    outcome.sorted$hospital.name[outcome.sorted$state==sort(state.abb)[3] & outcome.sorted$heart.failure==aaa[[3]][20]]
          
          aaa[[1]][1])
    }
    
    if (num=='worst') num<- 
    if (num=='best') num <- 
    if (num>nrow(outcome.rank)) return(NA)
    
    
    hospital.names.ranking <-outcome.state.na.rm[1][outcome.state.na.rm[3]==outcome.rank[,1][num]]
    return(hospital.names.ranking[order(hospital.names.ranking)])
    
    
  
  } else if (!any(outcome==illness)) {
    stop("invalid outcome")
  }
}
