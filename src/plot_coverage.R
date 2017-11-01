
plot_coverage <- function(dataframe, tag, column.vector){
  
  na.frame <- data.frame(label=dataframe$label,
                         duration=dataframe$duration,
                         is.na(dataframe[,column.vector]))
  
  
  Q <- reshape2::melt(na.frame, id.vars=c('label', 'duration'))
  Q$value <- as.numeric(Q$value)
  
  
  # We subset TRUE empty cases (value==1)
  QQ <- Q[Q$value==1,]
  
  g <- ggplot(QQ,
              aes(variable, fill=label, color=label)) +
    facet_wrap(~get(tag), scales = "free_y")+
    geom_histogram(stat="count") + theme_bw()
  
  
  g  
  
}

