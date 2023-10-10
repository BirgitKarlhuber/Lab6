#' Dynamic programming
#' 
#' xxxx
#'
#' @param x data frame with two variables, x = value and  w= weight
#' 
#' @param  W knapsack size
#'
#' @return the maximum knapsack value and related elements
#' 
#' @export
#' 
#' @examples
#' library(Lab6)
#' knapsack_dynamic(x, W)
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack}


knapsack_dynamic <- function(x, W){
  n <- nrow(x)
  m <- matrix(NA, W+1, n+1) 
  m[1,] <- rep(0, W+1)
  m[,1] <- rep(0, n+1)
  val <- x$v
  wei <- x$w
  for (i in 1:n){    
    for (c in 0:W){ # c = capacity of knapsack
      if (wei[i] > c){
        m[i+1,c+1] <- m[i,c+1]
      }else m[i+1,c+1] <- max(m[i,c+1], m[i,c+1 -wei[i]] + val[i])
    }
  } # this gets us the matrix with all the values of fittable elements
  

}