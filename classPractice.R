library(xts)
library(PerformanceAnalytics)

data("sample_matrix")
mydata <- sample_matrix

head(mydata, 4)
str(mydata)

# demo xts - get monthly data
mydata_mthly <- apply.monthly(mydata,tail,1 )

# import dataframe and transfer to ts object
ts <- read.table("data/ts.csv",header=TRUE,sep=",",as.is=TRUE)
head(ts,2)

ts_xts <- xts(ts[,-1], 
              order.by = as.Date(ts[,1], format= "%Y-%m-%d"))
head(ts_xts)
class(ts_xts)
index(ts_xts)

# calculate liner return
ts.ret.lin <- sample_matrix[-1,]/sample_matrix[-nrow(sample_matrix),] -1

# calculate log return
ts.ret.log <- diff(log(sample_matrix))

# Simulate return series
my.returns <- rnorm(252,mean=0.1/252,sd=.16/sqrt(252))
plot(my.returns,type="l",col="blue",ylab="Daily Return",
      xlab="Day",main="Daily Returns")

# load edhec data
data(edhec)
colnames(edhec)
colnames(edhec) = c("CA","CTA","DS","EM","EMN","ED","FIA",
                     "GM","LS","MA","RV","SS","FoF")

cor(edhec)[1:4,1:4]

# summary
coreData <- coredata(edhec[,1:3])
summary(coreData)

#
# regression example
#
library(quantmod)
getSymbols("^GSPC", src = "yahoo",from="1996-12-31",to="2009-08-31")

# get ret
spx.dat = apply.monthly(GSPC[,6],tail,1)
spx.ret = (exp(diff(log(spx.dat)))-1)[-1,]

# bind data and transfer to ts
my.df = cbind(coredata(spx.ret), coredata(edhec))
my.data.xdets = xts(my.df,order.by=as.Date(index(spx.ret)))

regression <- lm(my.data.xts[,2] ~ my.data.xts[,1])
beta <- regression$coefficients[2]

# loop beta
betas <- rep(0,ncol(edhec))
 for (i in 1:ncol(edhec)) {
  betas[i] = lm(my.data.xts[,(i+1)] ~
                    + my.data.xts[,1])$coef[[2]]
 }

# try tseries package
library(tseries)
GSPC <- get.hist.quote("^GSPC","2007-12-31","2014-12-31",quote = "Close",
                        provider="yahoo",retclass="zoo")


# test Garch model
library(tseries)
garchFit <- garch(ts.ret.log[,1], trace = FALSE)
coef(garchFit)

# load stock data
stk_data <- read.csv(file = "data/stock_data.csv",
                     stringsAsFactors = FALSE)

stk_data_xts <- xts(stk_data[,-1], as.Date(stk_data[,1],
                                           format = "%Y-%m-%d"))
plot.zoo(stk_data_xts, type = "l", col = 1:5)

# objects
die <- c(1:6)
text <- c("R", "Workshop")
logicals <- c(TRUE, FALSE,FALSE)

mtx <- matrix(die, nrow = 2)
ary <- array(mtx, dim=c(2,3,3))

list <- list(die, mtx, ary)

df <- data.frame(face = c("ace","king","queen"),
                 suit = c("heart","spades","diamonds"),
                 value = c(1,13,12))

allB <- c("B","BB","BB")
barCase1 <- c("B","BB","BB")
barCase2 <- c("DD","0","B")

n <- 1000
myPrizeHist <- vector()
for(i in 1:n){
  myPrizeHist[i] <- play()
}

# load tidyverse
library(tidyverse)
library(dplyr)

load("data_df.RData")
