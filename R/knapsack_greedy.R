#' Greedy Algorithm
#' 
#' This algorithm proceeds to insert the items into the knapsack based on their value/weight ratio, inserting the items with higher ratios first. This method gives an approximation of the solution to the knapsack problem.
#'
#' @param x data frame with two columns:  v = value and w = weights of each item to put it into the knapsack.
#' 
#' @param  W knapsack size, needs to be a positive value
#'
#' @return the maximum knapsack value and related elements
#' 
#' @export
#' 
#' @examples
#' library(Lab6)
#' knapsack_greedy(x, W)
#' 
#' @seealso \url{(https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}


knapsack_greedy <- function(x, W){
  
  # check input variables
  stopifnot(is.data.frame(x), x>0, length(x)==2, names(x) == c("w","v"))
  stopifnot(is.numeric(W), W<0, length(W)==1)
  
  # initialize starting values
  n <- nrow(x) # number of items
  val <- 0 # maximum knapsack value
  items <- rep(0,n) # set 
  val_wei <- x$v/x$w      # vector with ratio (value/weight)
  
  pos <- 0   
  
  # we find the max value of the weights, we take the position so calculate 
  # the sum and to save the items we are adding
  while((sum(x$w[items]) + x$w[which.max(val_wei)]) <= W){
    
    val <- val + x$v[which.max(val_wei)] 
    items[pos] <- which.max(val_wei)
    val_wei[which.max(val_wei)] <- 0 
    
    pos <- pos + 1
  }
  
  items <- items[which(items>0)]
  return(list(value=round(val), elements=items))
}  




# knapsack_greedy(x = knapsack_objects[1:800,], W = 3500)
# knapsack_greedy(x = knapsack_objects[1:1200,], W = 2000)

