

isPowerOf2 <- function(x) {
  n1s <- sum(as.numeric(intToBits(x)))
  if (n1s == 1) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

findClosestLowerPowerOf2 <- function(n) {
  for (i in n:1 )
  {
    if (isPowerOf2(i)) 
    { 
      res = i; 
      break; 
    } 
    i<- i - 1
  }
  
  return(res)
}

