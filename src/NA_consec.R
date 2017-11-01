NA_consec <- function(dataframe, column.vector){
  
  
  
  # we get NA values
  pru <- is.na(dataframe[,column.vector])
  
  out <- list()
  
  for(i in 1:nrow(pru)){
    # Find the first not NA 
    first.value <- match(0, pru[i,])
    
    # Find the last not NA and convert it to numeric
    last.value <- as.numeric(tail(which(pru[i,]==0), n=1))
    
    out[[i]] <- data.frame(first.value=first.value,
                  last.value = last.value)
    
    

  }
  
  # plot in tile plot. NA
  
  mm <- reshape2::melt(pru)
  names(mm) <- c("Call.Row", "Frequency.Point", "Empty")
  
  tile <- ggplot(mm,
         aes(x = Frequency.Point, y = Call.Row, fill = Empty)) +
    geom_tile() +
    scale_fill_manual(values = c("green", "black")) +
    theme_bw() 
  
  print(tile)
  
  out <- bind_rows(out)
  
  return(out)
    
}

