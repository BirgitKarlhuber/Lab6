#' Dynamic programming
#' 
#' This algorithm tries to solve the knapsack problem, by an dynamic approach. 
#'
#' @param x data frame with two columns:  v = value and w = weights of each item to put it into the knapsack. It is of complexity O(Wn), so it should scale much better than the brute force alogrithm. 
#' 
#' @param  W knapsack size, needs to be a positive value
#'
#' @return the maximum knapsack value and related elements
#' 
#' @export
#' 
#' @examples
#' library(Lab6)
#' dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack}


dynamic_knapsack <- function(x, W){
  
  # check input variables
  stopifnot(is.data.frame(x), x>0, length(x)==2, names(x) == c("w","v"))
  stopifnot(is.numeric(W), W>0, length(W)==1)
  
  
  # initialize starting values
  n <- nrow(x)
  val <- x$v
  wei <- x$w
  
  m <- matrix(nrow = n+1, ncol = W+1)
  m[1,] <- 0
  m[,1] <- 0
  
  for(i in 1:n){    
    for(c in 0:W){ # c = capacity of knapsack
      
      if(wei[i] > c){
        m[i+1,c+1] <- m[i,c+1]
      } else {
        m[i+1,c+1] <- max(m[i,c+1], m[i,c+1 -wei[i]] + val[i])
      }
    }
  } # this gets us the matrix with all the values of fit-able elements
  
  
  c <- c+1  
  i <- which.max(m[,c])
  elements <- length(n)
  k <- 1
  elements[k] <- i-1
  
  while(m[i,c]!=0 && c!=1 && i!=0){
    k <- k+1
    c <- (c-wei[i-1])
    i <- which(m[,c] == m[i-1,c])[1]
    elements[k] <- i-1
  }
  
  value <- round(m[n+1,W+1])
  elements <- sort(elements[which(elements>0)])
  
  return(list(value=value, elements=elements))
}
