#' greedy_knapsack_c function
#' 
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack size
#' @return A list of the maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export


greedy_knapsack_C <- function(x,W){
  if (W<0) stop ("W has to be a positive number")
  x[,3]<- x[,1]/x[,2]
  x$item <- 1:nrow(x)
  x<- x[order(x$V3),]

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
  
}



