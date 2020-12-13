# EM algorithm function
#' runEMAlgoritm
#' This function perform the whole EM algorithm
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
  
  while(ITER < ITERMAX | LIMITE > 1e-6){
    # Perform Expectaion step
    
    
    # Perform Maximization step
    # estimation of paramater prop, mu, sigma and alpha
    
    
    # Calcul of loglik of the next interation
    
    
    ITER <- ITER +1
  }
  
  return()
}