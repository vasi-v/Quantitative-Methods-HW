library(tidyquant)
library(tidyverse)

FromDate <- "2020-01-03"
ToDate <- "2021-10-26"

stockNames <- c("GOOG", "FB", "AMZN", "AAPL", "NFLX")
stocks <-tidyquant::tq_get(stockNames,
                         from = lubridate::ymd(FromDate),
                         to = lubridate::ymd(ToDate)) %>%
         dplyr::select(symbol, date, adjusted)

#dates <- base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd(FromDate),
                                                           #to = lubridate::ymd(ToDate),
                                                           #by = "day"), times = 5)),
                          #Symbol = c(base::rep(stockNames,base::as.numeric(lubridate::ymd(FromDate) - lubridate::ymd(ToDate))+ 1),5))

#i do not know why 
#but when i try do download the dates and replicate it for each stock 
#i get a mistake
#i suppose my code is incorrect


SPY <- tidyquant::tq_index("SP500") %>%
       dplyr::filter(symbol %in% stockNames) %>%
       dplyr::select(symbol, weight, shares_held) 

#not sure if that is how i should download the SPY

returns <- stocks %>%
  dplyr::mutate(returns = adjusted - dplyr::lag(adjusted))

#I am not sure that i will be able
#to do this HW
#when you explain it in class i get the idea
#but when i am at home...it is a "blank canvass"