#' Brute force algorithm goes through all possible alternatives and returns the 
#' maximum value found.
#' 
#' This approach is of complexity O(2^n).
#'
#' @param x data frame with two variables, x = value and w = weight
#' 
#' @param  W knapsack size
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
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack}


# library(Lab6)
# brute_force_knapsack(x = knapsack_objects[1:8,], W = 3500)



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
  
  
  if (parallel==TRUE){
    cores <- parallel::detectCores()
    cl <- makeCluster(cores, type = "PSOCK")
    
    parallel::clusterExport(cl, varlist=c("x","W","n","elements","maxval","val"))
    parallel::clusterEvalQ(cl, library(utils))
    
    values <- parallel::parLapply(cl, 1:n, function(i){
      
      comb <- utils::combn(n, i)
      k <- 1
      while (k <= ncol(comb)){
        if (W > sum(x$w[comb[,k]])){
          val <- sum(x$W[comb[,k]])
          if (val > maxval){
            elements <- comb[,k] # save elements of that combination
            maxval <- val # save the max value found
          }
        }
        k <- k+1
      }
      return(list(value = round(maxval), elements=elements))
      parallel::stopCluster(cl)
    })
    
    i <- 1
    while(values[[i]]["val"]!=0){
      val <- values[[i]]["val"]
      elements <- values[[i]]["elements"]
      i <- i+1
    }
    return(c(val,elements))
    parallel::stopCluster(clust)
    
    
  } else{
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
# 
# brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000, parallel = TRUE)




# 
# ### RUN to check time:
# # devtools::install_github("hadley/lineprof")
# library(lineprof)
# 
# system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500))
# lineprof(knapsack_brute_force(x = knapsack_objects[1:20,], W = 3500))
# 
# 
# # check if parallel is running faster than without - if not we wrong part parallelized
# system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500))
# system.time(brute_force_knapsack(x = knapsack_objects[1:20,], W = 3500, parallel = TRUE))
# 
