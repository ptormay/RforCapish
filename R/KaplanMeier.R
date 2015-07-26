#' Runs a Kaplan Meier plot
#' 
#' Takes in a data frame consisting of four columns:
#'  - Patient Key
#'  - category
#'  - event parameter (censoring)
#'  - time
#'  @param KMdata a data frame for calculating survival curve
#'  @return a data frame containing the result of the survival calculation
#'  @export
KaplanMeier <- function(KMdata){
  
  # load necessary libraries
  library(survival)
  
  #input can either be csv file or data   
  KMtest <- if(is.character(KMdata) && file.exists(KMdata)){ 
  read.csv(KMdata) 
    } else { 
  as.data.frame(KMdata) 
  } 
# convert to mumeric columns where necessary
KMtest$Time <- as.numeric(Kmtest$Time)
KMtest$Status <- as.numeric(Kmtest$Status)
  
fit <-survfit(Surv(Time,Status)~x,data=KMtest)

# define custom function to create a survival data.frame
createSurvivalFrame <- function(f.survfit){
  # initialise frame variable
  f.frame <- NULL
  
  # check if more then one strata
  if(length(names(f.survfit$strata)) == 0){
    # create data.frame with data from survfit
    f.frame <- data.frame(time=f.survfit$time, 
                          n.risk=f.survfit$n.risk, 
                          n.event=f.survfit$n.event, 
                          n.censor = f.survfit$n.censor, 
                          surv=f.survfit$surv, 
                          upper=f.survfit$upper, 
                          lower=f.survfit$lower)
    # create first two rows (start at 1)
    f.start <- data.frame(time=c(0, f.frame$time[1]), 
                          n.risk=c(f.survfit$n, f.survfit$n), 
                          n.event=c(0,0), 
                          n.censor=c(0,0), 
                          surv=c(1,1), 
                          upper=c(1,1), 
                          lower=c(1,1)) 
    # add first row to dataset
    f.frame <- rbind(f.start, f.frame)
    
    # add single stratum to dataset (needed for automation purposes)
    f.frame$strata <-paste("x=",KMtest$x[2],sep="")
    
    # remove temporary data
    rm(f.start)
  } 
  else {
    # create vector for strata identification
    f.strata <- NULL
    for(f.i in 1:length(f.survfit$strata)){
      # add vector for one strata according to number of rows of strata
      f.strata <- c(f.strata, rep(names(f.survfit$strata)[f.i], f.survfit$strata[f.i]))
    }
    
    # create data.frame with data from survfit (create column for strata)
    f.frame <- data.frame(time=f.survfit$time, n.risk=f.survfit$n.risk, n.event=f.survfit$n.event, n.censor = f.survfit$n.censor, surv=f.survfit$surv, upper=f.survfit$upper, lower=f.survfit$lower, strata=factor(f.strata))
    
    # remove temporary data
    rm(f.strata)
    
    # create first two rows (start at 1) for each strata
    for(f.i in 1:length(f.survfit$strata)){
      
      # take only subset for this strata from data
      f.subset <- subset(f.frame, strata==names(f.survfit$strata)[f.i])
      
      # create first two rows (time: 0, time of first event)
      f.start <- data.frame(time=c(0, f.subset$time[1]), n.risk=rep(f.survfit[f.i]$n, 2), n.event=c(0,0), n.censor=c(0,0), surv=c(1,1), upper=c(1,1), lower=c(1,1), strata=rep(names(f.survfit$strata)[f.i],2))  
      
      # add first two rows to dataset
      f.frame <- rbind(f.start, f.frame)
      
      # remove temporary data
      rm(f.start, f.subset)
      
    }
    
    # reorder data
    f.frame <- f.frame[order(f.frame$strata, f.frame$time), ]
    
    # rename row.names
    rownames(f.frame) <- NULL   
  }
  # return frame
  return(f.frame)
}

KM.output<-createSurvivalFrame(fit)
return(KM.output)
}