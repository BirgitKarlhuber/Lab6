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
#' @examples
#' library(Lab6)
#' knapsack_brute_force(x, W)
#' 
#' @seealso \url{}

# RNGversion("4.3.1")

library(parallel)

knapsack_brute_force <- function(x, W, parallel=FALSE){
  stopifnot(is.data.frame(x), x>0, length(x)==2, names(x) == c("w","v"))
  stopifnot(is.numeric(W), W<0, length(W)==1)
  
  if(is.logical(parallel) && !missing(parallel))
    {stop("variable parallel must be logical")
  }
  
  val <- 0
  maxval <- 0
  n <- nrow(x)
  elements <- length(n)
  
  if (parallel==TRUE){
    cores <- parallel::detectCores()
    clust <- makeCluster(cores, type = "PSOCK") # maybe cores-1
    valori <- parallel::parLapply(clust, 1:n, function(i, x, W){
      comb <- combn(1:n, i)
      k <- 1
      while (k <= ncol(comb)){
        if (W > sum(x$w[comb[,k]])){
          val <- sum(x$W[comb[,k]])
          if (val > maxval){
            elements <- comb[,k]
            maxval <- val
          }
        }
        k <- k+1
      }
      return(valori)
      parallel::stopCluster(clust)
    })
  } else{
    lapply(1:n, function(i){
      comb <- combn(1:n, i) # all the combinations of i from 1 to n
      k <- 1
      while(k <= ncol(comb)){ 
        if (W > sum(x$w[comb[,k]])){
          val <- sum(x$v[comb[,k]])
          if (val > maxval){
            elements <- comb[,k]
            maxval <- val
          }
        }
        k <- k+1
      }
      return(list(maxval, elements))
    }, x, W)
  }
  
  
}



# knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
# knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500)
# knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)
# knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)


### RUN to check time:
# system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 3500))
# lineprof(knapsack_brute_force(x = knapsack_objects[1:16,], W = 3500))




### Data ###

# RNGversion(min(as.character(getRversion()),"3.5.3"))
# 
# ## old sampler used for backward compatibility
# ## suppressWarnings() can be used so that the above warning is not displayed
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <- data.frame(
#   w=sample(1:4000, size = n, replace = TRUE),
#   v=runif(n = n, 0, 10000)
# )
# 
# # usethis::use_data(knapsack_objects, internal = TRUE)
# 
# 
# 


# check if parallel is running faster than without - if not we wrong part parallized
