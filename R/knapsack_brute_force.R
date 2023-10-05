#' Brute force alogrithm ...
#' 
#' xxxx
#'
#' @param x xxxxx
#' 
#' @param  W xxxx
#'
#' @return xxxx
#' 
#' @export
#' 
#' @examples
#' library(Lab6)
#' knapsack_brute_force(x, W)
#' 
#' @seealso \url{}


knapsack_brute_force <- function(x, W){
  # stopifnot()
  
  
  
}

# knapsack_brute_force(x = knapsack_objects[1:8,], W = 3500)
# knapsack_brute_force(x = knapsack_objects[1:12,], W = 3500)
# knapsack_brute_force(x = knapsack_objects[1:8,], W = 2000)
# knapsack_brute_force(x = knapsack_objects[1:12,], W = 2000)





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


