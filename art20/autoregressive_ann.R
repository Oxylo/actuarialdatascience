#
# Train a simple autoregressive ANN in a few lines of code
#
library(forecast)

##################
# 1. Read data
##################
data <- read.csv('eneco.data')
myts <- ts(data$gas, start=c(2007, 11), end=c(2019, 6), frequency=12)
autoplot(myts, main='gas consumption', xlab='year', ylab='m3')

##################################################
# 2a. Train neural net & forecast 12 months ahead
##################################################
ann <- nnetar(myts, lambda=0)
fc <- forecast(ann, h=12)
autoplot(fc, main='gas consumption', xlab='year', ylab='m3')

######################################################################
# 2b. Again, but with simulated prediction intevals around the forcast
######################################################################
ann <- nnetar(myts, lambda=0)
fc <- forecast(ann, h=12, PI=TRUE)
autoplot(fc, main='gas consumption', xlab='year', ylab='m3')

############################
# 2c. Walk-forwrd validation
############################

fcf <- function(y, h)
  #' User defined forecast function
  #' 
  #' @param y : time series
  #' @param h : prediction horizon
{
  fit <- nnetar(y, repeats=20, p=1, P=1, size=2, lambda=0)
  fc <- forecast(fit, h)
}

err <- tsCV(myts, fcf, h=1)
fitted <- myts - lag(err, -1)
y.fitted <- cbind(myts, fitted)
par(mfcol=c(2,1)) 

plot1 <- (autoplot(y.fitted, xlab='year', ylab='y and predicted') 
         + theme(legend.position = 'bottemright') 
         + scale_x_continuous(breaks = seq(2008, 2020, 2))
         + scale_color_manual(values=c('black', 'orange'))
         )
plot2 <- autoplot(lag(err, -1), xlab='year', ylab='y minus predicted', color='red')
grid.arrange(plot1, plot2, nrow=2)
