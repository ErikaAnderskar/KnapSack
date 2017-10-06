

greedy_knapsack <- function(x,W){



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

}

greedy_knapsack(x=knapsack_objects[1:1200,],W=2000)


