library(quantmod)

#Problem 1----

#1.1: Attempt:?

m<- c(2,3,4,1,7,8)
TTR::SMA(m, n=2)
sma <-NULL
SMAFunction <- function(inputVector){
  for (i in 1:base::length(inputVector)){
  sma <- c(sma, base::mean(inputVector[i:(i-2)]))
  }
  return(sma)
}

SMAFunction(m)

#I do not know how to do it

#1.2:
v <- c(4,8,8,2,3,4)
cor <- stats::cor(v,m)
#correlation coefficient:the ratio between the covariance of two variables 
#and the product of their standard deviations;value between ???1 and 1.

CorFunction <- function(inputVector1, inputVector2){
     First = base::sum(inputVector1 - base::mean(inputVector1))
     Second = base::sum(inputVector2 - base::mean(inputVector2))
     Nominator = base::mean(First*Second)
     Denominator = stats::sd(inputVector1)*stats::sd(inputVector2)
     Final = Nominator/Denominator
     return(Final)
}
  
CorFunction(v,m)

#I tried to write the function, however the results from the inbuilt function and 
#from my function are different
#Maybe I have misunderstood the formula

#Problem 2----

#Prime numbers less then 100: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
#A prime number is a whole number greater than 1 with only two factors – themselves and 1.
#A prime number cannot be divided by any other positive integers without leaving a remainder, decimal or fraction.

n <- 1:100
Prime_num <- for (i in n){
  
}



#again i was not able to solve it/found a lot of things in the internet tho/

#Problem 3----

library(tidyquant)
library(tidyverse)

data<-tidyquant::tq_get("AAPL")%>%
  dplyr::mutate(EMA26 = TTR::EMA(adjusted, n = 26),
                EMA12 = TTR::EMA(adjusted, n = 12),
                MACDline = EMA12 - EMA26,
                SignalLine = TTR::EMA(MACDline, n=9))%>%
  dplyr::filter(!is.na(EMA26 & SignalLine)) %>%
  dplyr::mutate(signal = dplyr::case_when(dplyr::lag(MACDline) > dplyr::lag(SignalLine) & MACDline < SignalLine ~ "sell",
                                          TRUE ~"buy"))

data2 <- data %>%dplyr::mutate(BenchmarkMoney = 100,
                      sss = adjusted/ lag(adjusted),
                      sss= ifelse(is.na(sss), 1, sss),
                      BenchmarkMoney1 = cumprod(sss),
                      StrategyMoney = 100)
                      

options(scripen = 999)

#still not complete
                