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

rankhospital<- function(state, outcome, num) {
  #to remove
  #outcome<- 'heart failure'
  #state<- 'TX'
  #num<- 4000
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
    outcome.state <- outcome.illness.subset[which(outcome.brief$state==state),] #filtered state and outcome
    outcome.state.na.rm<-outcome.state[which(!is.na(outcome.state[,3])),] #filtered NAs
    outcome.ordered <- outcome.state.na.rm[order(outcome.state.na.rm[3]),]
    outcome.rank <- unique(outcome.ordered[3])
    if (num=='worst') num<- nrow(outcome.rank)
    if (num=='best') num <- 1
    if (num>nrow(outcome.rank)) return(NA)
    hospital.names.ranking <-outcome.state.na.rm[1][outcome.state.na.rm[3]==outcome.rank[,1][num]]
    return(hospital.names.ranking[order(hospital.names.ranking)])
    
    
  } else if ( !any(state==state.abb)) {
    stop("invalid state")
  } else if (!any(outcome==illness)) {
    stop("invalid outcome")
  }
  
}
rankhospital('AZ', 'heart attack',5)
rankhospital('MD', 'heart attack', 'worst')

rankhospital('TX', 'heart failure', 4)

rankhospital('AZ', 'heart attack',300)
