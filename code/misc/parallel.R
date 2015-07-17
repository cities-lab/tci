# load required packages
library(doMC)
library(foreach)

# detect the number of cores 
require(parallel)
detectCores() # there are 24 cores

# set the number of multicore parallel backend
registerDoMC(24)
getDoParWorkers()
getDoParName()


# example of foreach for nested loop

x <-
  foreach(b=c(1:2), .combine='cbind') %:%
  foreach(a=c(1:4), .combine='c') %dopar% {
    a*10+b
  }
x
y <- as.matrix(x)

y[1,]

# test format

df1 <- data.frame(c(1:10))
df2 <- data.frame(c(11,20))
df3 <- data.frame(c(21,30))
df4 <- data.frame(c(31,40))
df <- data.frame(df1,df2,df3,df4)
colnames(df) <- c(11,12,21,22)
df[,"11"]

# matrix 
x <-
  foreach(a=c(1:2), .combine='rbind') %:%
  foreach(b=c(1:2), .combine='c') %dopar% {
    c <- as.character(a*10+b)
    summary(df[,c])
  }
x

x[1,]
x[1,1]

# list

x <-
  foreach(a=c(1:2)) %:%
  foreach(b=c(1:2)) %dopar% {
    
    c <- as.character(a*10+b)
    summary(df[,c])
  }
x

names(x)
x[[1]][[1]]
x[1]
str(x)


# work for three layers of loop 
x <-
  foreach(a=c(1:2)) %:%
  foreach(d=c(1:2)) %:%
  foreach(b=c(1:2)) %dopar% {
    c <- as.character(a*10+b)
    summary(df[,c])
  }
x


# how for formate the result correctively 


x <-
  foreach(a=c(1:2)) %:%
  foreach(b=c(1:2)) %dopar% {
    
    c <- as.character(a*10+b)
    data.summary <- summary(df[,c])
    data.summary.df <- data.frame(data.summary)
    
  }
x

# 

# test dependent packages
stargazer(df1, type = "text", title="title.name", 
          summary.stat = c("n","min", "p25", "median", "mean","sd", "p75", "max"), digits=4)

x <-
  foreach(a=c(1:2), .packages="stargazer") %:%
  foreach(b=c(1:2)) %dopar% {
    
    c <- as.character(a*10+b)
    
    data.summary <- stargazer(df[,c], type = "text", title="title.name", 
                              summary.stat = c("n","min", "p25", "median", "mean","sd", "p75", "max"), digits=4)
    data.frame(data.summary)
    
  }
x