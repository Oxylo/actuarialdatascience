# ==========
# fv.R
#
# Code to support article on www.actuarialdatascience.com 
# (Should retirement planning start at birth)
#
# Objective: calculating future value for any investment stream.
# ========== 


fv <- function(deposits, timescale=NULL, endpoint=NULL, rate=0.04, breaks=FALSE, verbose=FALSE) {
  # Returns the future value for any investment stream.
  #
  # Args:
  #   deposits: Required. Vector containing deposits.
  #   timescale: Optional. Vector containing time points when deposits take place.
  #              Default is deposits taking place at the beginning of each period. 
  #              Deposits and timescale must have same length. 
  #   endpoint:  Optional. Future value endpoint on timescale. If NULL, endpoint = highest timepoint in timecale,
  #              rounded up to the nearest integer. 
  #   rate: Optional. Either scalar indicating fixed return rate per unit of time or a
  #         function indicating cumulative return for implementing returns changing over time.
  #         Default is  0.04, indicating a fixed return of 4 percent per unit of time.
  #   breaks: Optional. If TRUE additional records are included in detailed calculation showing balance 
  #           just before each deposit is made. Default is FALSE.
  #   verbose: Optional. Boolean indicating whether calculation details should be returned. Default is FALSE.
  #
  # Returns:
  #   If verbose=TRUE, returns Future value at given endpoint. 
  #   If verbose=FALSE, returns dataframe with calculation details.
  #
  n <- length(deposits)
  frate <- rate
  # parameter checking
  if (is.null(timescale))
    timescale <- 0:(n-1)
  
  if (length(timescale) != n) {
    stop("Arguments deposits and timescale have different lengths: ",
         n, " and ", length(timescale), ".")
  }
  
  m <- max(timescale)
  if (is.null(endpoint))
    endpoint <- ifelse(ceiling(m) == m, m+1, ceiling(m))
  
  if (class(rate) != "function")
    frate <- (function(t) (1 + rate)^t)

  # main program
  timescale <- timescale[timescale <= endpoint]
  dep <- as.data.frame(list(t=timescale, deposit=deposits))
  lookup <- function(t) {
    index <- which(dep$t == t)
    ifelse(length(index), dep$deposit[index], 0)
  }
  regular.scale <- 0:floor(endpoint)
  merged.scale <- sort(unique(c(regular.scale, timescale, endpoint)))
  df <- as.data.frame(list(t=merged.scale))
  df$deposit <- sapply(df$t, lookup)
  
  if (breaks) {
    dep$deposit = 0
    df <- rbind(df, dep)
    df <- df[with(df, order(t, abs(deposit))),]
    rownames(df) <- 1:nrow(df)
  }
  
  df$rate <- sapply(df$t, frate)
  df$units <- df$deposit / df$rate
  df$cum.units <- cumsum(df$units)
  df$balance <- df$rate * df$cum.units
  if(verbose)
    return(df)
  else
    return(tail(df$balance, 1))
}



