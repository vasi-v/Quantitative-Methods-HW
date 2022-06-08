#Problem 1-----
library(tidyquant)
library(tidyverse)

FromDate <- "2012-01-03"
ToDate <- "2021-10-26"

info <-tidyquant::tq_get("GOOG",from = lubridate::ymd(FromDate),
                         to = lubridate::ymd(ToDate)) %>%
       dplyr::select(symbol, date, adjusted)

dates <- base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd(FromDate),
                                                            to = lubridate::ymd(ToDate),
                                                            by = "day")),
                           Symbol = c(base::rep("GOOG",base::as.numeric(lubridate::ymd(ToDate) - lubridate::ymd(FromDate))+ 1)))

DATA <- dates %>%
  dplyr::left_join(info, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")


SMA <- DATA %>%
  dplyr::mutate(sma20 =TTR::SMA(adjusted, n = 20)) %>%
  dplyr::mutate(sd20 = RcppRoll::roll_sd(adjusted, n =20, align = "right", fill = NA),
                UpperBound = sma20 + 2*sd20,
                LowerBound = sma20 - 2*sd20)%>%
  dplyr::filter(!is.na(sma20)) %>%
  dplyr::mutate(signal = dplyr::case_when(dplyr::lag(adjusted) < dplyr::lag(UpperBound) & adjusted > UpperBound ~ "sell",
                                          TRUE ~"buy"))



#Problem 2----

#Calculate the RSI using the instruction about the formula from here:
  # https://www.investopedia.com/terms/r/rsi.asp
  # Employ the following strategy and compare to a baseline strategy of buy and hold:
  # If the RSI above 65 - sell.
  # If the price goes below 35 - buy.

#RSI <- DATA %>%
  #dplyr::mutate(rsi=TTR::RSI(adjusted, n=20))

#still not solved

  