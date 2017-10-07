#' Brute force search function
#' 
#' @param x A data.frame with two variables v (values) and w (weights)
#' @param W The maximum knapsack weight
#' @return The maximum value in the knapsack and the elements choosen to be in the knapsack
#' 
#' @export
#' 

brute_force_knapsack<-function(x,W){
  stopifnot(is.data.frame(x) & ncol(x) == 2 & "v" %in% names(x) & "w" %in% names(x) & x[,1:2] > 0 & W>0)
  mat<-matrix(0,ncol=nrow(x),nrow=2^nrow(x))
  mat_row<-rep(0,nrow(x))
  for (i in 1:nrow(mat)){
    j<-nrow(x)
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
  df<-  data.frame(mat)
  vikt<-numeric()
  varde<-numeric()
  for (i in 1:nrow(mat)){
    vikt[i]<-sum(mat[i,]*x$w)
    varde[i]<-sum(mat[i,]*x$v)
  }
  df<-data.frame(vikt,varde,mat)
  names(df)<-c("weight","value",1:nrow(x))
  df_<-df[df$weight<W,]
  max_value<-max(df[df$weight<W,2])
  max_val<-df_[which.max(df_[df_$weight<W,2]),]
  
  res<-list()
  res$value<-round(max_val$value)
  res$elements<-as.numeric(colnames(max_val[,max_val==1]))
  
  return(res)
}
