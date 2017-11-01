Auto_Corr <- function(dataframe, my.margin){

  
 if(!my.margin %in% c(1,2)) {
    stop ("My.margin has to be 1 (rows) or 2 (colums)")
  }

if(sum(is.na(dataframe))>0){
  warning("There's NAs in the data, autocorrelation is using na.pass.")
}
  
  
out <- t(apply(dataframe,
                my.margin,
                function(x) as.numeric(acf(x, 
                                lag.max = 50,
                                plot = F,
                                na.action =  na.pass)$acf)))
                

if (sum(is.na(out))>0){warning("There are constant signals. Replacing with corr=1 for all time points.")}

out[is.na(out)] <- 1

                
return(out)
}
