Filter_Me <- function(dataframe,
                      columns,
                      end.positions, 
                      please.filter = FALSE,
                      na.threshold = 55){
  
  
  ## Fix Human error in duration #####
  
  # General Impossible Values
  
  # Duration cannont be less than 2 ms (or 0.002 sec)
  # Duration cannot be more than 300 ms 
  # We want to keep x between 0.002 & 0.4
  
  dataframe <- filter(dataframe, duration>=0.002 & duration<0.4)
  
  # This is only useful if you need to subset numeric columns 
  # aka duration
  
  fix_human <- function(x, lower.bound, upper.bound){
    
    boolean <- logical(length = length(x))
    
    boolean <- ifelse((is.na(x) | (x>lower.bound & x<upper.bound)),
                      FALSE,
                      TRUE)
    
    
    myfix <- ifelse(boolean==T, NA, x)
    
    return(myfix)
     
  }
  
  # 1) Anything below 15000 should go to NA
  # 2) Anything above 120000 should go to NA
  
  dataframe <- dataframe %>% mutate_at(.cols = columns,
                                      .funs=function(q) fix_human(q,15000,120000))
  

  # These old calls are way easier but have troubles
  
  # dataframe[dataframe < 15000] <- NA  THIS LINE WILL GET RID OF DURATION AND WE DONT WANT THAT
  # dataframe[dataframe > 120000] <- NA  THIS LINE WILL GET RID OF DURATION AND WE DONT WANT THAT
  
  
  
  
  huge.lista <- list()
  huge.counter <- 1
    
for(myend in 1:length(end.positions)){
 
 print(paste0("Trying end.positions value equals ", myend))  
  
  # Subset the signal
  signal <- dataframe %>%
    mutate(ID=1:nrow(dataframe)) %>%
    group_by(ID) %>%
    select_(columns)
  
  # We add one to account for ID column in position 1 of data.frame signal
  start.chunk <- c(1:myend) + 1
  
  # We subtract the end.positions - 1 from the length to have the last end.positions value from it
  # e.g, 51 - end.positions when end.positions=2 will give 49 and 49:51 will give one extra
  # we do 51 - (end.positions - 1) so that it will give 50:51 
  
  end.chunk <- c((length(signal) - (myend - 1)):length(signal))
  
  ends.to.remove <- c(start.chunk, end.chunk)
  
  # Remove ends
  signal <- signal %>% select(-ends.to.remove) 
  
  
  na.frame <- data.frame(is.na(signal))
  
  # Empty list to store the curve
  lista <- list()
  j <- 1 
  
  for(i in seq(10,100,5)){
    
    i <- as.numeric(i)
    
    print(i)
    
    temp <- na.frame %>% mutate(miss = rowSums(.[2:length(na.frame)]),
                                total= ncol(na.frame)-1,
                                prop = round(miss/total,3)*100,
                                accepted = ifelse(prop <= i, "accepted", "rejected"))
    
    temp
    
    number.of.calls <- temp %>% count(accepted) %>% mutate(na.max=i)
    
    
    lista[[j]] <- number.of.calls
    j <- j + 1
    
    print("loop is over, next j")
  }
  
  A <- bind_rows(lista) %>% mutate(End.Trim = myend)
  

  # Source helper function
  # if(!exists("plot_coverage", mode="function")) source("plot_coverage.R")
  
  huge.lista[[huge.counter]] <- A
  
  huge.counter <- huge.counter + 1 
  
  print("Huge loop over, be happy :)")
    
}  
  
  HUGE <- bind_rows(huge.lista)
  
  
  p <- ggplot(HUGE, aes(na.max, n, group=End.Trim, color=End.Trim)) + geom_line() + facet_wrap(~accepted)

  print(p)
  
  # Make summary 
  
  print("Here's a summary of the coverage")
  
  summ.cov <- HUGE %>% 
    group_by(na.max, End.Trim) %>%
    mutate(TOTAL=sum(n)) %>%
    mutate(percent=n/TOTAL*100) %>%
    filter(accepted=="accepted") %>%
    mutate(penalty=percent/(na.max*End.Trim)) %>%
    arrange(desc(penalty)) # penalty penalizes you for increasing na.max and End.Trim to gain percent
  
  print(summ.cov)
  
  if(please.filter){
  
    if(length(end.positions)>1){
      stop("choose only 1 value of end.positions if you want to filter")
      }
    
    # Make a table with proportion of accepted, the end.positions that yield that proportion
    # and ask to filter
    # alberto
    
    print(paste("na.threshold is", na.threshold, ". Do you want to change it?" ))
    
    change.it <- toupper(readline("Y/N>>  "))
    
    if(change.it=="Y"){
      ask <- readline("Choose your percent (0-100): >")
      
      # If ask is outside boundaries, function has to break
      if(as.numeric(ask)<=0 | as.numeric(ask)>=100){
        stop("Input has to be a number between 0 and 100. Run function again.")
      } else {
          na.threshold <- as.numeric(ask)
        }
      }
    
    
    # We kinda settled for na.max=55 and End.Trim=1
    # run the temp call with the value assigned
    
    temp <- na.frame %>% mutate(miss = rowSums(.[2:length(na.frame)]),
                                total= ncol(na.frame)-1,
                                prop = round(miss/total,3)*100,
                                accepted = ifelse(prop <= na.threshold, "accepted", "rejected"))
    
    # Create ID to store the original information of the your's original data order
    temp$ID <- 1:nrow(dataframe)
    
    filtered.data <- dataframe[which(temp$accepted=="accepted"),]
      
    return(filtered.data)
    
  }
  
  

  return(HUGE)
  
}



