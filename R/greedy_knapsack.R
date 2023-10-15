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
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}

# library(Lab6)
# greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)



greedy_knapsack <- function(x, W){
  
  # check input variables
  stopifnot(is.data.frame(x), x>0, length(x)==2, names(x) == c("w","v"))
  stopifnot(is.numeric(W), W>0, length(W)==1)
  
  # initialize starting values
  n <- nrow(x) # number of items
  val <- 0 # current knapsack value
  elements <- rep(0,n)
  ratio <- x$v/x$w      # vector with ratio (value/weight)
  
  i <- 0   
  
  # we find the max value of the weights, we take the position so calculate 
  # the sum and to save the items we are adding
  
  while((sum(x$w[elements]) + x$w[which.max(ratio)]) <= W && any(x$w>0)){
    
    val <- val + x$v[which.max(ratio)] 
    elements[i] <- which.max(ratio)
    ratio[which.max(ratio)] <- 0 # x$w[which.max(ratio)] <- 0
    i <- i + 1
  }
  
  elements <- elements[which(elements>0)]
  return(list(value=round(val), elements=elements))
}  

# greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)
# greedy_knapsack(x = knapsack_objects[1:1200,], W = 2000)

