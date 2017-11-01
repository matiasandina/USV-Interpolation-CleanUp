fix_labels <- function(y_column){
  
  
  Y_matrix <- data.frame()
  
  Y_matrix<-tidyr::separate(y_column,
                            remove=FALSE,into=c("main","secondary"),by='-')
  to.nnet$main.label<-as.numeric(as.factor(to.nnet$main))
  
  
  Y_matrix$label <- y_column
  
  # create as.numeric as factor
  Y_matrix$num.label <- as.numeric(as.factor(dataframe$y_column))
  
  
  
  
}