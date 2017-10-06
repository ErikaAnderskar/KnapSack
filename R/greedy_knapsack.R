#' greedy_knapsack function
#' 
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack size
#' @return A list of the maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export


greedy_knapsack <- function(x,W){
if (W<0) stop ("W has to be a positive number")

x[,3]<- x[,1]/x[,2]


x$item <- 1:nrow(x)
x<- x[order(x$V3),]


result <- data.frame(weight=0,value=0,density=0,item=0)


totalweight<-0

for (i in 1:nrow(x)){
totalweight <- x[i,1]+totalweight
if(totalweight<W){
  result[i,] <- x[i,]
} 

}

return(list(value=round(sum(result$value)), elements=result$item))

}






