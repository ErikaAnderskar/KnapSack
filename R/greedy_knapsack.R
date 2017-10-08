#' greedy_knapsack_c function
#' 
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack size
#' @param fast =FALSE set to TRUE if you want c++ code
#' @return A list of the maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export


greedy_knapsack <- function(x,W,fast=FALSE){
  if (W<0) stop ("W has to be a positive number")
  x[,3]<- x[,1]/x[,2]
  x$item <- 1:nrow(x)
  x<- x[order(x$V3),]
  
  if (fast==TRUE){
    item <- x[,4]
    weight <-x[,1]
    
    
    cppFunction('NumericVector iteminknapsack(NumericVector item, NumericVector weight, int W){
                
                int n=item.size();
                NumericVector out(n);
                double total = 0;
                for ( int i=0; i < n; ++i){
                total += weight[i];
                if (total < W){
                out[i] = item[i];
                }
                }
                
                return out;
                
  }')

    
    
    elements <- iteminknapsack(item,weight,W)
    value <- sum(x[elements==x[,4],2])
    elements <- elements[elements!=0]
    
    
    
    return(list(value=value,elements=elements))
} else {
  
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
  }


