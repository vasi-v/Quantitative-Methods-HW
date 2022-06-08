library(quantmod)

#Problem 1----

#1.1:

m<- c(2,3,4,1,7,8)
TTR::SMA(m, n=2)
sma <-NULL
n <- 2
SMAFunction <- function(inputVector,n){
  for (i in n:base::length(inputVector)){
  sma <- c(sma, base::mean(inputVector[i:(i-n+1)]))
  }
  return(sma)
}

SMAFunction(m,n)

#works, but does not return NA

#1.2:
v <- c(4,8,8,2,3,4)
cor <- stats::cor(v,m)
#correlation coefficient:the ratio between the covariance of two variables 
#and the product of their standard deviations;value between -1 and 1.

CorFunction <- function(inputVector1, inputVector2){
     First = inputVector1 - base::mean(inputVector1)
     Second = inputVector2 - base::mean(inputVector2)
     Nominator = base::sum(First*Second)/(base::length(inputVector1) - 1)
     Denominator = stats::sd(inputVector1)*stats::sd(inputVector2)
     Final = Nominator/Denominator
     return(Final)
}
  
CorFunction(v,m)


#Problem 2----

#Prime numbers less then 100: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97
#A prime number is a whole number greater than 1 with only two factors – themselves and 1.
#A prime number cannot be divided by any other positive integers without leaving a remainder, decimal or fraction.

num <- 1:100
Prime_num <- for(k in num){
  for(denominator in 1:k){
    if(k%%denominator == 0) {
      print(paste(k, denominator))
    }
  }
}


#Despite your efforts, i am still unsure how to "filter" the numbers
#sorry

#Problem 3----

library(tidyquant)
library(tidyverse)

data <- tidyquant::tq_get("AAPL",from = lubridate::ymd("2012-01-03"),
                                to = lubridate::ymd("2022-05-16"))%>%
        dplyr::select(symbol, date, adjusted)

dates2 <- base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd("2012-01-03"),
                                                         to = lubridate::ymd("2022-05-16"),
                                                         by = "day")),
                                             Symbol = c(base::rep("AAPL",3787)))
Final <- dates2 %>%
  dplyr::left_join(data, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")                      
  
signals <- Final %>%
  dplyr::mutate(EMA26 = TTR::EMA(adjusted, n = 26),
                EMA12 = TTR::EMA(adjusted, n = 12),
                MACDline = EMA12 - EMA26,
                SignalLine = TTR::EMA(MACDline, n=9))%>%
  dplyr::filter(!is.na(EMA26 & SignalLine)) %>%
  dplyr::mutate(signal = dplyr::case_when(dplyr::lag(MACDline) > dplyr::lag(SignalLine) & MACDline < SignalLine ~ "sell",
                                          TRUE ~"buy"))

data2 <- signals %>% dplyr::mutate(BenchmarkMoney = 100,
                      sss = adjusted/ lag(adjusted),
                      sss= ifelse(is.na(sss), 1, sss),
                      BenchmarkMoney1 = cumprod(sss))

data3 <- data2 %>% dplyr::mutate(StrategyMoney = 100,
                      sss1 = dplyr::case_when(signal == "sell" ~ 1,
                                              signal == "buy" ~ sss))
                      

options(scripen = 999)

#do not know if this is correct
                
