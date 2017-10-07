#' knapsack_dynamic function
#' 
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack size
#' @return A list of the maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export


knapsack_dynamic<- function(x,W){

if (W<0) stop ("W has to be a positive number")

test <- data.frame(x[,2],x[,1],1:length(x[,1]))
test <- t(test)

test <- test[,order(-test[2,])]

test <- test[,test[2,]<W]



m <- matrix(ncol=W+1, nrow=ncol(test)+1)

m[1,] <- rep(0,W+1)

m[,1] <- rep(0,ncol(test)+1)




N <- ncol(test)



for (i in 2:(N+1)){
  for (w in 1:W){ 
    
    if (test[2,i-1] > w){
      m[i, w+1] <- m[i-1,w+1]
      
    } else if (test[2,i-1] <= w){
      m[i, w+1] <- max(m[i-1,w+1],test[1,i-1]+m[i-1,(w+1-test[2,i-1])])
      
    }
  }
}



value <- round(max(m))


i <- N+1
w <- W+1

elements <- rep(0, length(test[1,]))

while(i & w >0){
  if (i==1) break
  if (m[i,w]!=m[i-1,w]){
    elements[i] <- test[3,i-1]
    w <- w-test[2,i-1]
    i <- i-1
   
  } else {
    i <- i-1
   
  }
 
}


elements <- elements[elements!=0]

return(list(value=value, elements=elements))

}


