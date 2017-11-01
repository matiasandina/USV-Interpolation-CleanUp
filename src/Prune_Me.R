Prune_Me <- function(non.interpolated,
                     interpolated,
                     sample.rows,
                     show.plot=FALSE){
  
  
  
  # non interpolated data is needed to calculate mean + 3sd boundaries
  # also needed to plot in show.plot=T
  
  interp <-  interpolated 
  
  # Get x vector from data
  x<-1:length(interp)
  
  # names of columns are going to be "f1.....fn"
  
  .columns <- paste0("f", x)
  
  
  columns.to.subset <- paste(first(.columns),last(.columns), sep=":")
  
  # Get the subset of the desired columns, mind the underscore
  non.interp <- select_(non.interpolated, columns.to.subset)
  
  
  # General rules ######
  
  # 1) Anything below 15000 should go to NA
  
  interp[interp < 15000] <- NA
  
  # 2) Anything above 120000 should go to NA
  
  interp[interp > 120000] <- NA
  
  
  # Take the mean within call
  mean.vector <- rowMeans(non.interp, na.rm=TRUE)
  # Take the sd within call
  sd.vector <- apply(non.interp, 1, function(x) sd(x, na.rm=T))
  
  # Create top margins
  
  top.value <- mean.vector + 3 * sd.vector
  bottom.value <- mean.vector - 3 * sd.vector
  
  # If interpolated data goes outside margins they should go to NA
  # We create logical matrices to subset from there
  
  above.value <- interp > top.value 
  below.value <- interp < bottom.value
  
  interp[above.value] <- NA
  interp[below.value] <- NA
  
  # If you want to analyze the NAs you added and extreme cases
  # Some of them are in the boundary condition
  # (i.e, 28 NAs out of 50 meaning 55% NAs allowed in Filter_Me)
  
  # hist(as.numeric(apply(interp,1, function(q) sum(is.na(q)))))
  #  myflag <- interp[which(apply(interp,1, function(q) sum(is.na(q)))>25),]
  # return(myflag)
  
  
  # Re-interpolation of Missing points ######
  
  # We will try to do a piecewise linear regression
      
  # copy interp because we will modify it
  
  out <- interp
  
  for(i in 1:nrow(out)){
    
    print(i)
    
    y <- out[i,]
    
    # Get predicted values for piece-wise regression
    fit <- piece(x,y)
    
    out[i,is.na(y)] <- fit[is.na(y)]
    
  }
  
  ##### Plot the pruning #####
  
  sampled.rows <- sample(x = nrow(interp),
                         size = sample.rows)
  
  
  # 3 datasets: 
  # 1 interpolated + pruned with NAs,
  # 1 non interpolated, 
  # 1 pruned and reinterpolated
    
    new.interp <- interp[sampled.rows,]
    new.non.interp <- non.interp[sampled.rows,]
    new.out <- out[sampled.rows,]

  # Label for calls
  mylabel <- as.character(non.interpolated$label[sampled.rows])
  
  
  sampled.top <- top.value[sampled.rows]
  sampled.bottom <- bottom.value[sampled.rows]
  
  # print(cbind(new.interp,sampled.top,sampled.bottom))
 
  # If show.plot==T it will show, else it will not and it will run faster 
  if(show.plot){
    for(i in c(1:nrow(new.interp))){
      
      
      # print(i)
      
      # Obtain the frequency values for the datasets
      
      interp.y <- as.numeric(new.interp[i,])
      non.interp.y <- as.numeric(new.non.interp[i,])
      out.y <- as.numeric(new.out[i,])
      
      # print(interp.y)
      
      p1 <- ggplot(data.frame(x,non.interp.y, interp.y, out.y),
                   aes(x, non.interp.y))+
        geom_point(aes(x, non.interp.y), color='black')+
        geom_point(aes(x, interp.y), color='red', size=4, shape=4)+
        geom_point(aes(x, out.y), color='green', size=4, shape=2)+
        theme_bw() + scale_x_continuous(minor_breaks = x, breaks= x)+
        geom_hline(yintercept = c(sampled.top[i],sampled.bottom[i]), lty=2)+
        ggtitle(paste(mylabel[i], "interpolation"))
      
      p2 <- p1 + ylim(c(22000, 110000))
      
      TTT <- cowplot::plot_grid(p1,p2)
      
      print(TTT)
      
    }
  } 
  
  
  
  
  
  
  
#####  OLD OPTION DIFF ######
  
  # Dataframe here should be already interpolated, ONLY FREQUENCY VALUES
  # We aim to find the extreme values that have to be pruned for cases
  # 1) interpolation error at the borders
  # 2) Measurement error (e.g, harmonics too loud)
  
  # We start by making a copy of the dataframe to fill the NAs without messing with the original
  
  # out <- dataframe
  
  ## Next steps
  
  # Rowwise diff() and transpose to get correct order (calls in rows)
  # diff.matrix <- t(apply(dataframe, 1, function(q) diff(q)))
  
  # for(i in 1:nrow(out)){
    
  #  y <- as.numeric(out[i,])
  #  y.diff <- diff.matrix[i, ]
  #  above <- which(abs(y.diff) > diff.threshold)
    
  #  print(y)
  #  print(y.diff)
  #  print(above)
    
    # In the case there's no such value above threshold, go to next iteration
  #  if(is.integer(above) && length(above) == 0L){ next }
    
    # Make the extreme values into NA.
    # We use above + 1 because we lost 1 in the diff call
    
  #  y[(above + 1)] <- NA
    
    # fit a line to interpolate again those values
    
  #  pred.y <- lin.reg(x.values,y)
    
    # Fill NA values with the linear regression 
      
  #  out[i, (above + 1)] <- pred.y[above + 1]
    
    
  # }
  
  
  ## General Rules Still apply after the re-interpolation
  
  # 1) Anything below 15000 should go to NA
  
  out[out < 15000] <- NA
  
  # 2) Anything above 120000 should go to NA
  
  out[out > 120000] <- NA
  
  
 return(out)

    
}


# We have to check for boundaries

# interp[above.value] <- NA but with the out object
# interp[below.value] <- NA but with the out object

# we have to get this into the function !!!

# QQ <- data.frame(t(apply(qq, 1, function(y) copy_NA(y))))