
##### Piece-Wise ######
library(segmented)


piece <- function(x,y){
  
  # just in case this comes with column names or something
  y <- as.numeric(y)
  
  
  
  # create the lm object
  lm.obj <- lm(y~x)
  # Try to fit piecewise and return the coefficients
  seg1 <- try(segmented(lm.obj,seg.Z=~x))
  
  # See if it fails once, get out using linear
  
  if("try-error" %in% class(seg1)) {
    
    # Print that you are using a linear regression and not the piece-wise
    print("Using linear Regression")
    
    # Call helper function
    result <- lin.reg(x,y)
    
    # Get out of the error/function
    return(result)  
    
  }
  
  # We do not trust segmented working reliably
  # It can estimate badly but not fail or fail randomly
  # We will mark a flag if the coefficients are too small (1e-10)
  
  print(abs(broom::tidy(seg1)$estimate[2:3]))
  
  flag <- as.numeric(abs(broom::tidy(seg1)$estimate[2:3])>1e-10)
  
  sum.flag <- sum(flag)
  
  diff.flag <- diff(as.numeric(abs(broom::tidy(seg1)$estimate[2:3])))
  
  
  print(paste("summ.flag", sum.flag))
  
  print(paste("diff.flag", diff.flag))
  
  
  print(paste("first try class:",class(seg1)))  
  
  # inspiration
  # seg1 <- try(sum(as.numeric(abs(coefficients(segmented(lm.obj,seg.Z = ~x))[2:3]) > 10e-15)))
  
  # If second group of flags worrisome, get out with linear 
  
  if(sum.flag < 2 | diff.flag < 1) {
    
    # Print that you are using a linear regression and not the piece-wise
    print("Using linear Regression")
    
    # Call helper function
    result <- lin.reg(x,y)
    
    # Get out of the error/function
    return(result)
  } else {
    
    # Use the piece-wise
    #result <- predict(segmented::segmented(lm.obj,seg.Z=~x),
    #                  newdata = data.frame(x,y))
    
    result <- predict(seg1,
                      newdata=data.frame(x,y))
    
    print("Using piece-wise regression")
    
    print(plot(x,y, pch=19))
    print(lines(result))
    
    
  }
  
  
  return(result)
  
}
