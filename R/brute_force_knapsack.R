#' Brute force search function
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack weight
#' @return Returns the maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export
#' 

brute_force_knapsack<-function(x,W){
  stopifnot(is.data.frame(x) & ncol(x) == 2 & "v" %in% names(x) & "w" %in% names(x) & x[,1:2] > 0 & W>0)
  nrowx<-nrow(x)
  mat<-matrix(0,ncol=nrowx,nrow=2^nrowx)
  mat_row<-rep(0,nrowx)

  for (i in 1:nrow(mat)){
    j<-nrowx
    while(j>0){
      if (mat_row[j]==0) break
      else{
        mat_row[j]<-0
        j<-j-1
      }
    }
    mat_row[j]<-1
    mat[i,]<-mat_row
  }
  vikt<-numeric()
  varde<-numeric()
  w<-x$w
  v<-x$v
  for (i in 1:nrow(mat)){
    vikt[i]<-sum(mat[i,]*w)
    varde[i]<-sum(mat[i,]*v)
  }
  df<-data.frame(vikt,varde,mat)
  names(df)<-c("weight","value",1:nrowx)
  df_<-df[df$weight<W,]
  max_val<-df_[which.max(df_[df_$weight<W,2]),]
  
  res<-list()
  res$value<-round(max_val$value)
  res$elements<-as.numeric(colnames(max_val[,max_val==1]))
  
  return(res)
}

#lineprof(brute_force_knapsack(x = knapsack_objects[1:12,], W = 2000))

system.time(brute_force_knapsack(x = knapsack_objects[1:16,], W = 2000))
# set.seed(42)
# n <- 2000
# knapsack_objects <-
#   data.frame(
#     w=sample(1:4000, size = n, replace = TRUE),
#     v=runif(n = n, 0, 10000)
#   )
# x <- knapsack_objects[1:8,]
# W <- 3500
