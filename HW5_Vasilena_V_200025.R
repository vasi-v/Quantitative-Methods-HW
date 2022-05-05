library(quantmod)

#Problem 1----

#1.1: Attempt:?

m<- c(2,3,4,1,7,8)
TTR::SMA(m, n=2)

SMAFunction <- function(inputVector){
  Denominator = base::length(inputVector)
  #?Nominator = sum(inputVector-lag(inputVector))
  Result = Nominator/Denominator
  return(Result)
}

SMAFunction(m)

#I think that we need the lag() function 

#1.2:
v <- c(4,8,8,2,3,4)
cor <- stats::cor(m,v)
#correlation coefficient:the ratio between the covariance of two variables 
#and the product of their standard deviations;value between ???1 and 1.

#I will need further explanations the correlation and covariance/sorry/


#Problem 2----

#Prime numbers less then 100: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
#A prime number is a whole number greater than 1 with only two factors – themselves and 1.
#A prime number cannot be divided by any other positive integers without leaving a remainder, decimal or fraction.

Prime_num <- for (i in 2:n){
    
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
  dplyr::mutate(signal = dplyr::case_when(dplyr::lag(MACDline) > dplyr::lag(SignalLine) & dplyr::lag(MACDline) < dplyr::lag(SignalLine) ~ "sell",
                                          dplyr::lag(MACDline) < dplyr::lag(SignalLine) & dplyr::lag(MACDline) > dplyr::lag(SignalLine) ~ "buy",
                                         TRUE ~ "hold"))

#Again as in HW4 SMA task - The function does not find any values
#for which the TRUE conditions are implemented
#Everything is "hold"

                