#' Brute force algorithm 
#' 
#' The brute force algorithm goes through all possible alternatives and returns the maximum value found. It is of complexity O(2^n).
#'
#' @param x data frame with two columns:  v = value and w = weights of each item to put it into the knapsack.
#' 
#' @param  W knapsack size, needs to be a positive value
#' 
#' @param parallel Wether to speed up the code by parallizing (TRUE) or not (FALSE)
#'
#' @return the maximum knapsack value and related elements
#' 
#' @export
#' 
#' @import parallel
#' 
#' @importFrom utils combn
#' 
#' @examples
#' library(Lab6)
#' brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack}


# library(parallel)
# library(utils)

brute_force_knapsack <- function(x, W, parallel=FALSE){
  
  # check input variables
  stopifnot(is.data.frame(x), x>0, length(x)==2, names(x) == c("w","v"))
  stopifnot(is.numeric(W), W>0, length(W)==1)
  
  if(!missing(parallel) && !is.logical(parallel)){
    stop("variable parallel must be logical")
  }
  
  # initialize starting values
  val <- 0
  maxval <- 0
  n <- nrow(x)
  elements <- length(n)
  
  
  if(parallel==TRUE){
    cores <- parallel::detectCores()
    cl <- parallel::makeCluster(cores-1, type = "PSOCK")
    
    parallel::clusterExport(cl, varlist=c("x","W","n","elements","maxval","val"), envir=environment())
    parallel::clusterEvalQ(cl, library(utils))
    
    values<-parallel::parLapply(cl, 1:n, function(i, x,W) {
      
      comb <- utils::combn(n, i) # all the combinations of i from 1 to n
      k <- 1
      while(k <= ncol(comb)){ 
        if (W > sum(x$w[comb[,k]])){
          val <- sum(x$v[comb[,k]])
          if (val > maxval){
            elements <- comb[,k] # save elements of that combination
            maxval <- val # save the max value found
          }
        }
        k <- k+1
      }
      
      return(list(value=round(maxval),elements=elements))
      parallel::stopCluster(cl)
    }, x, W )
    
    
    i=1
    while(values[[i]]["value"]!=0){
      val<-values[[i]]["value"]
      elements<-values[[i]]["elements"]
      i<-i+1
    }
    return(c(val,elements))
    
    
  } else{ # parallel = FALSE (default)
    
    lapply(1:n, function(i){
      comb <- utils::combn(n, i) # all the combinations of i from 1 to n
      k <- 1
      while(k <= ncol(comb)){ 
        if (W > sum(x$w[comb[,k]])){
          val <- sum(x$v[comb[,k]])
          if (val > maxval){
            elements <<- comb[,k] # save elements of that combination
            maxval <<- val # save the max value found
          }
        }
        k <- k+1
      }
    })
    return(list(value = round(maxval), elements=elements))
    }
}


# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 3500)
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 2000)
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000)
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = TRUE)


# # check if parallel is running faster than without - if not something wrong 
# system.time(brute_force_knapsack(x = knapsack_objects[1:18,], W = 3500))
# system.time(brute_force_knapsack(x = knapsack_objects[1:18,], W = 3500, parallel = TRUE))




################ profile to find problem with parallel

# x = knapsack_objects[1:18,]
# W = 3500
# val <- 0
# maxval <- 0
# n <- nrow(x)
# elements <- length(n)
# 
# 
# profvis({
#   
#   cores <- parallel::detectCores()
#   cl <- parallel::makeCluster(cores-1, type = "PSOCK")
#   
#   parallel::clusterExport(cl, varlist=c("x","W","n","elements","maxval","val"), envir=environment())
#   parallel::clusterEvalQ(cl, library(utils))
#   
#   values<-parallel::parLapply(cl, 1:n, function(i, x,W) {
#     
#     comb <- utils::combn(n, i) # all the combinations of i from 1 to n
#     k <- 1
#     while(k <= ncol(comb)){ 
#       if (W > sum(x$w[comb[,k]])){
#         val <- sum(x$v[comb[,k]])
#         if (val > maxval){
#           elements <- comb[,k] # save elements of that combination
#           maxval <- val # save the max value found
#         }
#       }
#       k <- k+1
#     }
#     
#     return(list(value=round(maxval),elements=elements))
#     parallel::stopCluster(cl)
#   }, x, W )
#   
#   
#   i=1
#   while(values[[i]]["value"]!=0){
#     val<-values[[i]]["value"]
#     elements<-values[[i]]["elements"]
#     i<-i+1
#   }
#   return(c(val,elements))
# })
