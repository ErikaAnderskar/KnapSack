## ----KS, include=FALSE---------------------------------------------------
require(microbenchmark)
require(KnapSack)
require(Rcpp)

## ----KS1a----------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <- data.frame(v=sample(1:4000,size=n,replace=TRUE),
                               w=runif(n=n,0,10000)
                               )
brute_force_knapsack(knapsack_objects[1:8,],3500)


## ----KS1b----------------------------------------------------------------
sys_time<-system.time(brute_force_knapsack(knapsack_objects[1:16,],3500))
sys_time



## ----KS2-----------------------------------------------------------------
set.seed(42)
n <- 2000
knapsack_objects <-
  data.frame(
    v=sample(1:4000,size=n,replace=TRUE),
    w=runif(n=n,0,10000)
  )


knapsack_dynamic(knapsack_objects[1:8,],3500)


## ----KS3-----------------------------------------------------------------
sys_time<- system.time(knapsack_dynamic(knapsack_objects[1:500,],3500))
sys_time


## ----KS4b----------------------------------------------------------------
set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    v=sample(1:4000,size=n,replace=TRUE),
    w=runif(n=n,0,10000)
  )
system.time(
  greedy_knapsack(knapsack_objects,2000)
  
)

## ----KS4-----------------------------------------------------------------
set.seed(42)
n <- 1000000
knapsack_objects <-
  data.frame(
    v=sample(1:4000,size=n,replace=TRUE),
    w=runif(n=n,0,10000)
  )
system.time(
  greedy_knapsack(knapsack_objects,2000)
  
)

## ----KS5-----------------------------------------------------------------
sys_time_<-system.time(greedy_knapsack(x=knapsack_objects[1:1000,], W= 2000))
sys_time_


