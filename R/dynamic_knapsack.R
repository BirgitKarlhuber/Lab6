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
#' 
#' @seealso \url{https://en.wikipedia.org/wiki/Knapsack}

# library(Lab6)
# dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)


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

# dynamic_knapsack(x = knapsack_objects[1:8,], W = 3500)
# dynamic_knapsack(x = knapsack_objects[1:12,], W = 3500)
# dynamic_knapsack(x = knapsack_objects[1:8,], W = 2000)
# dynamic_knapsack(x = knapsack_objects[1:12,], W = 2000)


## Data
# RNGversion(min(as.character(getRversion()),"3.5.3"))
# 
# set.seed(42, kind = "Mersenne-Twister", normal.kind = "Inversion")
# n <- 2000
# knapsack_objects <- data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
# )




###########################

# hashmap - alternative/faster than matrix
# m(nrow(x),w,hashmap(default=-1),x) - where m is the recursive function

# x <- knapsack_objects[1:8,]
# W <- 3500
# 
# n <- nrow(x)
# v <- x$v
# w <- x$w
# 
# 
# value <- matrix(-1, nrow = n + 1, ncol = W + 1)
# 
# # Define function m
# m <- function(i, j){
#   if(i == 0 || j <= 0){
#     value[i, j] <- 0
#     return()
#   }
#   
#   if(value[i-1, j] == -1){
#     m(i-1, j)
#   }
#   
#   if(w[i] > j){
#     value[i, j] <- value[i-1, j]
#   }else{
#     if(value[i-1, j-w[i]] == -1){
#       m(i-1, j-w[i])
#     }
#     value[i, j] <- max(value[i-1, j], value[i-1, j - w[i]] + v[i])
#   }
# }
# 
# knapsack <- function(i, j){
#   
#   if(m(i, j) > m(i-1, j)){
#     return(c(i, knapsack(i-1, j - w[i])))
#   }else{
#     return(knapsack(i-1, j))
#   }
# }
# 
# result <- knapsack(8, 3500)
# 



