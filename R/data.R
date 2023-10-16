#' Example of an unweighted graph
#'
#' A 'data.frame' with two variables (w and v) that contains the value (v) and the weight (w) of each item to put it into the knapsack.
#'
#' @format ## `knapsack_objects`
#' A data frame with 2000 rows and 2 columns:
#' \describe{
#'   \item{w}{weight to put item into the knapsack}
#'   \item{v}{value of the item}
#' }
#' 
#' @examples
#' library(Lab6)
#' greedy_knapsack(x = knapsack_objects[1:800,], W = 3500)

"knapsack_objects"
