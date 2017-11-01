shape_plot <- function(dataframe,columns,tag,sample.rows=NULL,...){
  
  if(!is.null(sample.rows) & nrow(dataframe)>=sample.rows){
    print(paste0("sampling ", sample.rows, " from data"))
    dataframe <- dataframe[sample(nrow(dataframe),size = sample.rows,
                                  replace = F),]  
    
  }
  

  
  data2 <- select_(dataframe,
                   tag,columns)
  

  data2$fakeID = c(1:nrow(dataframe))
  
  # Get IDs as numeric values (first and last columns for how was coded)
  ID <- c(1, ncol(data2))
  
  to.plot <- reshape2::melt(data2,
                            id.vars=ID)
  
  p <- ggplot(to.plot, aes(variable, value, group=fakeID, color=get(tag)))+
    geom_line(...)
  
  p  

  }

