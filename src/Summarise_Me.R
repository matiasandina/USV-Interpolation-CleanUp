Summarise_Me <- function(dataframe, my.margin, ...) {
  
  # This will get a dataframe with just frequency values
  # It will create a summary of columns by row that you can append to other stuff
  
  if(!my.margin %in% c(1,2)) {
    stop ("My.margin has to be 1 (rows) or 2 (colums)")
    }
  
  if(sum(is.na(dataframe))>0){
    warning("There's NAs in the data, you might want to add na.rm=T to function arguments")
  }
  
  
  mean_f <- apply(dataframe, my.margin,
                  function(y) mean(y, ...))
  
  var_f <- apply(dataframe, my.margin,
                 function(y) var(y, ...))
  
  diff.range <- apply(dataframe, my.margin,
                      function(y) diff(range(y)))
  
  # Make x vector for linear regression depending on the margin
  # If you apply to rows the length of x should be the columns
  if(my.margin==1){
    x <- 1:ncol(dataframe)  
  } else {
    x <- 1:nrow(dataframe)
  }
  
  # get the slope of a lm (second coeff) 
  
  trend <- apply(dataframe, my.margin,
               function(y) coefficients(lm(as.numeric(y)~x))[2])
  
  # we use this helper to return the value of the max diff
  # Keep in mind that if there is ties, positive wins
  
  absmax <- function(x) { x[which.max( abs(x) )]}
  
  max.jump <- apply(dataframe, my.margin,
                    function(y) absmax(diff(y)))
  
  
  ## Auto-correlation and exponential fit
  
  my.corr <- Auto_Corr(dataframe, my.margin)
  
  
  exp_mod <- function(x,y){
    
    # fitting exponential needs parameters and shit we don't want to deal with
    # we will take log and fit linear
    
    # We know that y might be in [-1,1] range and logs don't like negative
    # Thus, we add 10 to all y values to keep shape but get out of negative zone
    
    y <- log(y + 10)
  
    decay <- coefficients(lm(y ~ x))[2]
  
    return(decay)
    }
  
corr.decay <- apply(my.corr, my.margin,
                      function(y) exp_mod(x,y))


### Number of points above the mean

 above_mean <- rowSums(dataframe > mean_f)


 out <- data.frame(mean_f = mean_f,
                    var_f = var_f,
                    diff.range = diff.range,
                    trend = trend,
                    max.jump = max.jump,
                   corr.decay = corr.decay,
                   above_mean = above_mean)
  return(out)
}