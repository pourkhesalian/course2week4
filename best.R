rm(list= ls())
outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
outcome.all[,11]<- as.numeric(outcome.all[,11])

hist(outcome.all[,11])
min.death.rate.11<-min(outcome.all[,11], na.rm = T)
outcome.all[,2][which(outcome.all[,11]==min.death.rate.11)]
illness<- c('heart attack', 'heart failure', 'pneumonia')
outcome.brief <- outcome.all[,c(2,7,11,17,23)]
outcome.brief[, 3] <- as.numeric(outcome.brief[,3])
outcome.brief[, 4] <- as.numeric(outcome.brief[,4])
outcome.brief[, 5] <- as.numeric(outcome.brief[,5])
names(outcome.brief) <- c('hospital.name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')

best<- function(state, outcome){
  #-----------------------------------------
  outcome.all <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  illness<- c('heart attack', 'heart failure', 'pneumonia')
  outcome.brief <- outcome.all[,c(2,7,11,17,23)]
  outcome.brief[, 3] <- as.numeric(outcome.brief[,3])
  outcome.brief[, 4] <- as.numeric(outcome.brief[,4])
  outcome.brief[, 5] <- as.numeric(outcome.brief[,5])
  names(outcome.brief) <- c('hospital.name', 'state', 'heart.attack', 'heart.failure', 'pneumonia')
  #---------------------------------  
  
  
  
  if (any(state==state.abb) & any(outcome==illness)){
    condition <- sub(' ', '.', outcome)
    outcome.illness.subset <- cbind(outcome.brief[,c(1,2)], outcome.brief[[condition]])
    names(outcome.illness.subset)[3]<- condition
    outcome.state <- outcome.illness.subset[which(outcome.brief$state==state),]
    best.hospitals <- outcome.state[which(outcome.state[[condition]]== min(outcome.state[[condition]],na.rm = T)),][1]
    return( best.hospitals[order(best.hospitals$hospital.name),])
  } else if ( !any(state==state.abb)) {
    stop("invalid state")
  } else if (!any(outcome==illness)) {
    stop("invalid outcome")
  }
  
  
  
}

best('AL', 'heart failure')
best('AcL', 'heart failure')
best('AL', 'hesart failure')
