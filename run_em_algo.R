# EM algorithm function
#' runEMAlgoritm
#' This function perform the whole EM algorithm
#' @param data
#' @param nbClass
#' @param ITERMAX, 
runEMAlgoritm <- function(data, nbClass, ITERMAX, mode = "random"){
  # Split of data
  # In this step data are split in 2 dataset continous and categorical
  
  # Initialization
  # Creation of objects that will contain parameters of models
  if(mode == "random"){
    ## initialization of random model
    
  } else if(mode == "kmeans"){
    ## initialization of kmeans model
  } else {
   return (print("The selected mode is invalid"))
  }
  
  
  # Beggin of loops
  ITER <- 1
  LIMITE <- 1
  
  while(ITER < ITERMAX){
    # Perform Expectaion step
    
    
    # Perform Maximization step
    # estimation of paramater prop, mu, sigma and alpha
    
    
    # Calcul of loglik of the next interation
    
    
    ITER <- ITER +1
    if(LIMITE <=1e-6){
      return(
        c(
          print(paste("WARNING: we stoped the algorithm because limit is under or equal to)", 1e-6))
        ),
        list())
      break
    }
  }
  
  return()
}