setwd("../Desktop")

#https://spartanideas.msu.edu/2019/02/21/binary-integer-conversion-in-r/
# Convert an ID (integer) to a binary vector of appropriate length. Note that the vector is reversed so that the lowest order bit (corresponding to the first decision) comes first.
fromID <- function(id,n=5) {
  as.integer(head(intToBits(id), n))
}
# Convert a binary vector of appropriate length to an ID value (integer).
toID_rev <- function(vec) {
  n <- length(vec)
  # Create a vector of powers of 2 (for use in conversions from binary vectors to integers).
  powers.of.two <- 2^(0:(n - 1))
  as.integer(vec %*% powers.of.two)
}
# Convert a binary vector of appropriate length to an ID value (integer).
toID <- function(vec) {
  n <- length(vec)
  # Create a vector of powers of 2 (for use in conversions from binary sets to integers).
  powers.of.two <- 2^((n - 1):0)
  as.integer(vec %*% powers.of.two)
}

#dat <- readLines(con = "Day 3_test.txt")
dat <- readLines(con = "Day 3.txt")

require(stringr)
dat_split <- str_split_fixed(dat, "", nchar(dat[1]))

half <- nrow(dat_split)/2
hold <- gam <- vector(mode = "numeric",length = ncol(dat_split))

for(i in 1:length(hold)){
  hold[i] <- sum(as.numeric(dat_split[,i]))
  if(hold[i]>half){
    gam[i] <- 1
  }
}

eps <- abs(gam-1)

g <- toID(gam)
e <- toID(eps)

cat("The gamma is",paste0(gam),"=",g,
    "\nThe epsilon is",paste0(eps),"=",e,
    "\nThe 'power consumption' is",g*e)

rating <- function(dat_split,common = TRUE){
  hold <- rat <- vector(mode = "numeric",length = ncol(dat_split))
  temp <- dat_split
  for(i in 1:length(hold)){
    half <- dim(temp)[1]/2
    hold[i] <- sum(as.numeric(temp[,i]))
    
    if(common){
      if(hold[i]>half){
        rat[i] <- 1
      }else if(hold[i]==half){
        rat[i] <- 1
      }
    }else{
      if(hold[i]<half){
        rat[i] <- 1
      }else if(hold[i]==half){
        rat[i] <- 0
      }
    }

    
    temp <- temp[temp[,i]==rat[i],]
    a <- dim(temp)[1]
    if(is.null(a) || a ==1){
      return(as.numeric(temp))
    }
    
    i=i+1;i;a;rat
    
  }
  return(rat)
}

o_g_r <- rating(dat_split,common = TRUE)
co2_scrub <- rating(dat_split,common = FALSE)

g <- toID(o_g_r)
e <- toID(co2_scrub)

cat("The oxygen generator rating is",paste0(o_g_r),"=",g,
    "\nThe CO2 scrubber rating is",paste0(co2_scrub),"=",e,
    "\nThe life support rating is",g*e)
