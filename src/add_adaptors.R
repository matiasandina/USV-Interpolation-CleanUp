### HELPER TO MOVE ALL NAs to last ####

#' Move All \code{NA} Values to the End of the Rows or Columns of a Matrix
#' 
#' Moves all of the \code{NA} values in the rows or columns of a matrix to the
#' end of the respective rows or columns.
#' 
#' 
#' @param inmat The input matrix.
#' @param by Should be either \code{"row"} or \code{"col"}, depending on if you
#' want to shift non-\code{NA} values left or up.
#' @param outList Logical. Do you just want a \code{list} of the non-\code{NA}
#' values? Defaults to \code{FALSE}.
#' @param fill While you're at it, do you want to replace \code{NA} with some
#' other value?
#' @return Either a \code{matrix} with the same dimensions as the input matrix
#' or a \code{list} with the same number of rows or columns as the input matrix
#' (depending on the choice made in \code{by}).
#' @author Ananda Mahto
#' @references \url{http://stackoverflow.com/q/23008142/1270695}
#' @examples
#' 
#' set.seed(1)
#' m <- matrix(sample(25, 20, TRUE), ncol = 4,
#'             dimnames = list(letters[1:5], LETTERS[1:4]))
#' m[sample(prod(dim(m)), prod(dim(m)) * .6)] <- NA
#' 
#' m
#' 
#' naLast(m, by = "row")
#' naLast(m, by = "col")
#' naLast(m, by = "col", outList = TRUE)
#' 
#' @export naLast
naLast <- function(inmat, by = "row", outList = FALSE, fill = NA) {
  A <- dim(inmat)
  M <- matrix(fill, nrow = A[1], ncol = A[2])
  dimnames(M) <- dimnames(inmat)
  switch(by, 
         row = {
           myFun1 <- function(x) { y <- inmat[x, ]; y[!is.na(y)] }
           B <- sequence(A[1])
         },
         col = {
           myFun1 <- function(x) { y <- inmat[, x]; y[!is.na(y)] }
           B <- sequence(A[2])
         },
         stop("'by' must be either 'row' or 'col'"))
  
  myList <- lapply(B, myFun1)
  if (isTRUE(outList)) {
    myList
  } else {
    Len <- vapply(myList, length, 1L)
    switch(by,
           row = {
             IJ <- cbind(rep(sequence(A[1]), Len), sequence(Len))
           },
           col = {
             IJ <- cbind(sequence(Len), rep(sequence(A[2]), Len))
           },
           stop("'by' must be either 'row' or 'col'"))
    M[IJ] <- unlist(myList, use.names=FALSE)
    M
  }
}


### Actual adaptors function ####



add_adaptors <- function(dataframe,
                         colums.to.subset,
                         adaptor.value,
                         adaptor.size=10,
                         na.placeholder=0,
                         z.score=FALSE,
                         empty.data=FALSE,
                         limit.rows=NULL){
  
  # ------------- Debug Only ----------
  # We need limit.rows only for debug purposes when we want to run for less than the whole data.frame
  # The problem with subseting in the argument as in dataframe[1:10, ] is that there is a call to min(dataframe)
  # Since the dataframe the function sees is the only the subset, the min will be incorrect and placement will be incorrect
  
  # Subset
  
  data.to.play <- select_(dataframe, colums.to.subset)
  
  
  # If zscore = T >>> zscore the rows
  
  if(z.score==T){
    
    # Helper function that can deal with NAs
    scale_this <- function(x){
      (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
    }
    
    # Scale, row-wise, na.rm during math
    # Actual NAs keep being there because NA/real = NA 
    # Note we apply row-wise but need to transpose afterwards
    
    data.to.play <- t(apply(data.to.play, 1, scale_this))  
    
    
  }
  
  # naLast Helper Function for data with NAs #####
  # Put signal first, NAs last, do not sort
  # Change NA values to placeholder.value, zero by default
  
  # We convert the data.frame to matrix and back to data.frame again
  
  if(empty.data==TRUE){
    data.to.play <- data.frame(naLast(inmat = as.matrix(data.to.play), 
                                      by = 'row',
                                      fill = na.placeholder))
    
  }
  
  
  # Duration Proportional Shifting ######
  
  # Rescale duration by min duration
  # Round to the neraest integer
  
  scaling.factor <- round(dataframe$duration/min(dataframe$duration),
                          digits=0)
  
  # we need to allocate the max amount of space
  # That is the ratio + the length of the call
  
  myvalue <- max(round(filtered_50$duration/min(filtered_50$duration),0))
  
  myvalue <- myvalue + length(data.to.play[1,])
  
  # This will create a fairly big matrix with na.placeholder as values
  max.space <- data.frame(matrix(nrow = nrow(dataframe),
                                 ncol = myvalue,
                                 data = na.placeholder))
  
  # print(scaling.factor)
  
  # print(paste("Max space is", max.space))
  
  for(i in 1:min(limit.rows,nrow(dataframe))){
    
    # call length in points (i.e, 50 measurments=50)
    # if call comes with NAs they will get ignored regardless of the position
    # ideally, calls should be interpolated (no NAs)
    # Alternatively, if a call had NAs,
    # they should have been put at the back by naLast (see above)
    
    call.length <- length(data.to.play[i,])
    
    # we get the place where the call should start (index column)
    call.start <- scaling.factor[i]
    # we get the place where the call should end (index column)
    # The -1 here is because we are adding call.length to the position
    # (hence counting the start twice)
    call.end <- scaling.factor[i] + call.length - 1
    
    # we create a vector of columns to fill to index  
    columns.to.fill <- c(call.start:call.end)
    
    #print(paste("column starts at:", call.start))
    #print(paste("column ends at:", call.end))
    
    # print(length(columns.to.fill))
    # print(length(data.to.play[i,]))
    
    # print(columns.to.fill)
    # print(data.to.play[i,])
    
    
    # we fill it with the call values
    
    max.space[i,columns.to.fill] <- data.to.play[i,]
    

      
  }
  
  # Add adaptors #####
  
  matrix.rows <- nrow(dataframe)
  
  adaptor1 <- data.frame(matrix(ncol = adaptor.size,
                                nrow = matrix.rows,
                                data = adaptor.value))
  
  # Duplicate adaptor
  adaptor2 <- adaptor1
  
  # Rename with "meaningful" names
  names(adaptor1) <- paste0("start", 1:adaptor.size)
  names(adaptor2) <- paste0("end", 1:adaptor.size) 
  
  # knit everything together
  
  out <- data.frame(duration=dataframe$duration,
                    adaptor1,
                    max.space,
                    adaptor2)
  
  # Return modified dataframe
  
  return(out)  
  
}



