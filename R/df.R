#' Return the dataframe 
#' 
#' Takes in a data frame or csv file consisting of four columns:
#'  - Patient Key
#'  - category
#'  - event parameter (censoring)
#'  - time
#'  @param KMdata a data frame for calculating survival curve
#'  @return a data frame containing the result of the survival calculation
#'  @export
df <- function(KMdata){
  
  # load necessary libraries
  library(survival)
  
  #input can either be csv file or data   
  KMtest <- if(is.character(KMdata) && file.exists(KMdata)){ 
    read.csv(KMdata) 
  } else { 
    as.data.frame(KMdata) 
  } 
  KMtest$Time <- as.numeric(Kmtest$Time)
  KMtest$Status <- as.numeric(Kmtest$Status)
return (KMtest)

}
