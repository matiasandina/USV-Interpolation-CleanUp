Interpolate_Me <- function(dataframe, column.vector, mymethod){
  
  subseted.frame <- dataframe[,column.vector]
  
  x.length <- length(subseted.frame)
  
  out <- apply(subseted.frame, 1, function(y) Fit_Row(x=1:x.length,
                                                      y,
                                                      inspect=FALSE,
                                                      mymethod=mymethod))
  
  # Get the Out in shape
  
  out <- data.frame(t(out))
  
  names(out) <- paste0("f",1:x.length)
  
  return(out)
  
}