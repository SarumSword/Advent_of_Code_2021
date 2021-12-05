#Commenting intentionally left out.

setwd("../Desktop")
dat <- readLines(con = "./Day 2.txt")


xyz_convert <- function(dat){
  require(stringr)
  dat_use <- str_split_fixed(dat, " ", 2)
  directions <- c("left","backward","down","stop","up","forward","right")
  x <- y <- z <- vector(mode = "numeric",length=nrow(dat_use))
  out <- data.frame(x,y,z)
  xyzconvert <- sapply(X = dat_use[,1], FUN = function(x){return(which(directions %in% x)-4)})
  for(i in 1:nrow(dat_use)){
    if(xyzconvert[i]<0){
      out[i,abs(xyzconvert[i])] <- -as.numeric(dat_use[i,2])
    }else{
      out[i,abs(xyzconvert[i])] <- as.numeric(dat_use[i,2])
    }
  }
  return(out)
}

dat_xyz <- xyz_convert(dat)

out <- c()
for(i in 1:ncol(dat_xyz)){
  out <- c(out,sum(dat_xyz[,i]))
}

cat("The final direction is",abs(out[1]*out[2]))

shooting <- function(dat_xyz){
  aim <- horiz <- dep <- vector(mode = "numeric",length=nrow(dat_xyz))
  out <- data.frame(aim,horiz,dep)
  for(i in 1:nrow(dat_xyz)){
    out$aim[i] <- -dat_xyz$x[i]
    out$horiz[i] <- dat_xyz$y[i]
    out$dep[i] <- sum(out$aim)*dat_xyz$y[i]
  }
  return(out)
}

# dat_test <- c("forward 5",
# "down 5",
# "forward 8",
# "up 3",
# "down 8",
# "forward 2")
# 
# dat_xyz <- xyz_convert(dat_test)


dat_aim_horiz_dep <- shooting(dat_xyz)

cat("The final direction is",sum(dat_aim_horiz_dep$horiz)*sum(dat_aim_horiz_dep$dep))
