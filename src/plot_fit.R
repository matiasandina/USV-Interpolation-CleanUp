plot_fit <- function(dataframe,
                     sample.rows,
                     column.vector,
                     x.length){
 
 dataframe$FakeID <- 1:nrow(dataframe)  
 
 # Sampled rows will allow us to:
 #  1) sample 
 #  2) keep the original rownumbers
 
 
 sampled.rows <-sample(x = nrow(dataframe),
                       size = sample.rows)
 
 # Perform subset by sampled.rows and provided column.vector
 new.dat<-dataframe[sampled.rows, column.vector]
  
 # keep labels from the sampled rows
 # we can use them as titles and to summarise later
 mylabel<-as.character(dataframe$label[sampled.rows])
 

 # We will use vectors that are of constant increase up to the desired lenght
  x <- 1:x.length
  
 # Allocation of inspection list
    
  
  lista <- list()
  
  for(i in 1:nrow(new.dat)){
    
    print(i)
    
    # Columns have names, we want to get rid of them for lm
    # hence, we use as.numeric call
    
    y <- as.numeric(new.dat[i,])
    
    
    if(length(x)!=length(y)){stop("X and Y vectors lengths differ. Check column.vector and x.length")}
    
    # Get fits from Fit_Row.R, since it's for inspecting, we use inspect=T
    # This produces a data.frame with fits in columns and 1 x column
    
    myfit <- bind_rows(Fit_Row(x=x, y=y, inspect=TRUE)) %>% mutate(x=x)
    
    # We will inspect in 2 scales (detail and 22000 to 110000)
    
    # First plot, free axis
    p1<-ggplot(data.frame(x,y), aes(x, y))+
    geom_point(data=data.frame(x,y), aes(x,y), color='black')+  
    geom_line(data=reshape2::melt(myfit, id.vars='x'),
              aes(x,value, colour = variable))+
    #geom_point(data=reshape2::melt(myfit, id.vars='x'),
    #            aes(x,value, colour = variable))+  
      ggtitle(paste(mylabel[i], "fit")) + theme_bw() +
      scale_x_continuous(minor_breaks = x, breaks= x)
    
    
    # Second plot is just a rescaling of first plot
    p2 <- p1 + ylim(c(22000, 110000))
    
    TTT <- cowplot::plot_grid(p1,p2)
    
    print(TTT)
    
    # Ask which one do you like
    
    category.allowed = FALSE
    
    while(category.allowed==FALSE) {
    
      ask <- readline(prompt= "What fit do you like the most? (1,2,3,4)>> ")
      
      # This will convert into TRUE the user input and exit the while loop 
      category.allowed <- as.numeric(ask) %in% 1:(ncol(myfit)-1)
        
      
      call.type <- mylabel[i]
      best.fit <- ask
      
      lista[[i]] <- data.frame(call.type = call.type,
                               best.fit = best.fit)
        
    }
      
    
    readline(prompt="Hit return to see next plot>> ")
    
  }
  
  
  # We bind the rows of the lista and return:
  #  1) the call label
  #  2) the best fit
  #  3) the row in the original df
  
  out <- bind_rows(lista) %>% mutate(sampled.rows=sampled.rows)
    
  return(out)
}

