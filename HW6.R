#Problem 1-----
library(tidyquant)
library(tidyverse)

info <-tidyquant::tq_get("GOOG",from = lubridate::ymd("2012-01-03"),
                         to = lubridate::ymd("2021-10-26")) %>%
       dplyr::select(symbol, date, adjusted)

dates <- base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd("2012-01-03"),
                                                            to = lubridate::ymd("2021-10-26"),
                                                            by = "day")),
                           Symbol = c(base::rep("GOOG",3585)))

DATA <- dates %>%
  dplyr::left_join(info, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")


SMA <- DATA %>%
  dplyr::mutate(sma20 =TTR::SMA(adjusted, n = 20)) %>%
  dplyr::filter(!is.na(sma20))%>%
  dplyr::mutate(upperBound = sma20 + 2*stats::sd(dplyr::lag(sma20))) %>%
  dplyr::mutate(lowerBound = sma20 - 2*stats::sd(dplyr::lag(sma20)))

#define upper and
#lower bounds around it which are equal to SMA +-2 standard deviation
#the past observations used to calculate the SMA.
#I did not understand how to do the upper and lower bounds

#Problem 2----

#I am not very sure how to calculate the RSI formula 
#i suppose that we should not use the inbuilt function 
#like i have done below

RSI <- DATA %>%
  dplyr::mutate(rsi=TTR::RSI(adjusted, n=20))

  
