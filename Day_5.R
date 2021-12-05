setwd("../Desktop")
#dat <- readLines(con = "Day 5_test.txt")
dat <- readLines(con = "Day 5.txt")

require(stringr)
dat_split <- str_split_fixed(gsub(pattern = " -> ",
                                  replacement = ",",x = dat), ",", 4)
dat_split <- matrix(data = as.numeric(dat_split),ncol = 4,byrow = FALSE)
colnames(dat_split) <- c("x1","y1","x2","y2")

plotter <- function(dat,grd=TRUE){
  mx <- max(c(dat[1,],dat[3,]))
  my <- max(c(dat[2,],dat[4,]))
  a <- min(c(dat[1,],dat[3,]))
  b <- min(c(dat[2,],dat[4,]))
  
  par(mar = c(2,2,0.5,0.5),lab = c(mx, my, 7))
  plot(x=c(dat[,1],dat[,3]),y=c(dat[,2],dat[,4]),
       xaxt = "n",yaxt = "n",
       xlim=c(a,mx),
       ylim=c(my,b))
  axis(1,at=c(0:mx))
  axis(2,at=c(0:my))
  for(i in 1:nrow(dat)){
    lines(x=c(dat[i,1],
              dat[i,3]),
          y=c(dat[i,2],
              dat[i,4]),
          col = i)
  }
  if(grd){
    grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted",equilogs = TRUE)
  }
}

make_line <- function(x,y){
  out <- lm(y~x)
  if(is.na(out$coefficients[2])){
    out <- lm(x~y)
    y_hat <- y[1]:y[2]
    x_hat <- y_hat*out$coefficients[2]+out$coefficients[1]
  }else{
    x_hat <- x[1]:x[2]
    y_hat <- x_hat*out$coefficients[2]+out$coefficients[1]
  }
  return(data.frame(x_hat,y_hat))
}

mat_plotter <- function(dat_split){
  dat_adjusted <- dat_split
  mx <- max(c(dat_adjusted[,1],dat_adjusted[,3]))
  my <- max(c(dat_adjusted[,2],dat_adjusted[,4]))
  a <- min(c(dat_adjusted[,1],dat_adjusted[,3]))
  b <- min(c(dat_adjusted[,2],dat_adjusted[,4]))
  if(a<=0){
    offset_a = 1-a
  }else{
    offset_a = 0 
  }
  if(b<=0){
    offset_b = 1-b
  }else{
    offset_b = 0 
  }
  
  out <- matrix(rep(0,(mx+offset_a)*(my+offset_b)),nrow = my+offset_b,ncol = mx+offset_a)
  
  for(i in 1:nrow(dat_adjusted)){
    
    lin <- make_line(x=c(dat_adjusted[i,1],dat_adjusted[i,3]),
              y=c(dat_adjusted[i,2],dat_adjusted[i,4]))
    
    for(j in 1:nrow(lin)){
      clm <- as.integer(round(lin$x_hat[j]))+offset_a
      rw <- as.integer(round(lin$y_hat[j]))+offset_b
      out[rw,clm] <- (1+out[rw,clm])
    }
  }
  return(out)
}

#test <- 1:5

dat_new <- dat_split[dat_split[,1]==dat_split[,3] | dat_split[,2]==dat_split[,4],]#[test,]

plotter(dat_new,grd=FALSE)

o <- mat_plotter(dat_new)

cat("The hot zones from horizontal and vertical overlaps count to",length(which(o>1)))

plotter(dat_split,grd = FALSE)

o <- mat_plotter(dat_split)

cat("The hot zones from all line overlaps count to",length(which(o>1)))

