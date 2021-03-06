Fit_Row <- function(x, y, inspect=TRUE, mymethod=NULL){
  
  
  ## if you run the function just to inspect
  # the idea is that you are looping over rows from 
  # another function
  
  
  if(inspect==TRUE){
  
  A <- lin.reg(x,y)    
  B <- poly.four(x,y)
  C <- poly.five(x,y)
  D <- zoo::na.spline(y)
  
  return(list(lin.reg=A,
              poly.four=B,
              poly.five=C,
              spline=D))
  
  }
  
  if(inspect==FALSE){
    if(is.null(mymethod)){
      
      A <- lin.reg(x,y)    
      B <- poly.four(x,y)
      C <- poly.five(x,y)
      D <- zoo::na.spline(y)
      
      # return the binded list of all fits if no method specified
      
      return(bind_rows(
             list(lin.reg=A,
                  poly.four=B,
                  poly.five=C,
                  spline=D))
             )
      
      
    } else {
     
    # Switch/case structure by method
    
      switch(mymethod, 
             linear={
               # linear regression here
               A <- lin.reg(x,y)
               return(A)
             },
             poly.four={
               # polynomic, order four
               B <- poly.four(x,y)
               return(B)
             },
             poly.five={
               # polynomic, order five
               C <- poly.five(x,y)
               return(C)
             },
             splines={
               # polynomic, order four
               D <- zoo::na.spline(y)
               return(D)
             }
      )
        
    }
  }
  

}


# How to call this function

# pru must be the subset with the selected columns
# Interpolated_data <- data.frame(t(apply(filtered_50[,c(3:52)], 1,
#                        FUN = function(y) Fit_Row(x=1:50,
#                                  y,
#                                  inspect=FALSE,
#                                  mymethod = "splines"))))

# names(Interpolated_data) <- paste0("f",1:50)


# HELPER FUNCTIONS


###### POLINOMIAL FIT: ORDER 1 ######

lin.reg <- function(x,y){
  
  lala <- lm(y~x)
  y.lm <- predict(lala, newdata=data.frame(y, x))

  return(lin.values=y.lm)
    
}

###### POLINOMIAL FIT: ORDER n ######

  
# General polynomial, MODIFY HERE!!!
poly.reg <- function(x,y,degree){
  
  lala <- lm(y~poly(x = x, degree=degree))
  y.poly <- predict(lala, newdata=data.frame(y, x))
  return(y.poly=y.poly)
}

# Wrappers that we will use
poly.five <- function(x,y){
  poly.reg(x,y,5)
}

poly.four <- function(x,y) {
  poly.reg(x,y,4)
}

##### Splines ######

#zoo::na.spline(y)


## Plot Ideas ########
  
#plot(x,y)
#points(x=x,y=predict(lala, newdata=data.frame(y, x)), col="red", pch=19)
  
  
#if ( plot == TRUE ) {
  
#  plot.frame <- data.frame(
#    y=rep(y,2)/7,
#    x=rep(1:length(y),2),
#    inter.values=c(interpol.linear.final, interpol.spline.final)/7,
#    method=c(rep("Linear", length(y)), rep("Spline", length(y)))
#  )
  
#  p <- ggplot(data=plot.frame, aes(x=x)) +
#    geom_point(aes(y=y, x=x), size=4) +
#    geom_line(aes(y=inter.values, color=method), size=1) +
#    ylab("y") +
#    xlab("x") +
#    theme(axis.title.y =element_text(vjust=0.4, size=20, angle=90)) +
#    theme(axis.title.x =element_text(vjust=0, size=20, angle=0)) +
#    theme(axis.text.x =element_text(size=15, colour = "black")) +
#    theme(axis.text.y =element_text(size=17, colour = "black")) +
#    theme(panel.background =  element_rect(fill = "grey85", colour = NA),
#          panel.grid.major =  element_line(colour = "white"),
#          panel.grid.minor =  element_line(colour = "grey90", size = 0.25))+
#    scale_color_manual(values=c("#377EB8", "#E41A1C"), 
#                       name="Interpolation method",
#                       breaks=c("Linear", "Spline"),
#                       labels=c("Linear", "Spline")) +
#    theme(legend.position="none") +
#    theme(strip.text.x = element_text(size=16)) +
#    facet_wrap(~ method)
  
#  suppressWarnings(print(p))
  
#}

# More info about splines here https://stats.stackexchange.com/questions/59418/interpolation-of-influenza-data-that-conserves-weekly-mean/63004

