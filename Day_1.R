#Commenting intentionally Left Out

setwd("../Desktop")
nums <- as.numeric(readLines(con = "./Day 1.txt"))

changes <- function(x){
  out <- c(paste("There are",length(which(x>0)),"positive measurements."),
              paste("There are",length(which(x<0)),"negative measurements."),
              paste("There are",length(which(x==0)),"unchanging measurements."))
  return(out)
}

high_low <- function(x){
  out <- c()
  for(i in 2:length(x)){
    if(x[i]<x[i-1]){
      out <- c(out,-1)
    }else if(x[i]>x[i-1]){
      out <- c(out,1)
    }else{
      out <- c(out,0)
    }
  }
  return(out)
}

changes(high_low(nums))

sum_3 <- function(x){
  out <- c()
  for(i in 3:length(x)){
    out <- c(out,sum(x[(i-2):i]))
  }
  return(out)
}

changes(high_low(sum_3(nums)))

peaks <- function(x){
  xx <- high_low(x)
  out <- c()
  for(i in 2:length(xx)){
    if(xx[i]==-1 && xx[i-1]==1){
      out <- c(out,1)
    }else if(xx[i]==1 && xx[i-1]==-1){
      out <- c(out,-1)
    }else{
      out <- c(out,0)
    }
  }
  return(out)
}

changes(peaks(nums))
