library(tidyquant)
library(tidyverse)

#Problem 1----

#1:

data<-tidyquant::tq_get(c("AMZN","FB", "NFLX"),
             from= "2019-01-01 ",
             to="2021-04-01") %>%
      dplyr::select(symbol, date, adjusted)
#2:

dates<-base::data.frame(Dates = base::rep(base::seq.Date(from = lubridate::ymd("2019-01-01"),
                                    to = lubridate::ymd("2021-04-01"),
                                    by = "day"),3),
                        Symbol = c(base::rep("AMZN",822), base::rep("NFLX",822), base::rep("FB",822)))


Final <- dates %>%
  dplyr::left_join(data, by = c("Dates" = "date", "Symbol" = "symbol"))%>%
  dplyr::group_by(Symbol)%>%
  tidyr::fill(adjusted, .direction = "downup")
 
#3:


    data_new<-base::data.frame(tidyquant::tq_get(c("AMZN","FB"),
             from = "2019-01-01",
             to ="2019-07-01"))
      

    data_new2<-base::data.frame(tidyquant::tq_get(c("AMZN","FB"), 
            from = "2020-04-01",
            to = "2020-07-01"))
    
    ResultM <- data_new %>%
       dplyr::full_join(data_new2, by = NULL) %>%
       dplyr::arrange(symbol)%>%
       dplyr::arrange(desc(date))
    
#4:
    
select <- ResultM %>%
  dplyr::group_by(symbol)%>%
  dplyr::slice(c(1, n()))
    
#5:I don't know how to do it

#select2 <- ResultM %>%
  #dplyr::mutate ?

#Problem 2----
 SMA <- Final %>%
  dplyr::mutate(sma10 =TTR::SMA(adjusted, n = 10),
                sma25 =TTR::SMA(adjusted, n = 25)) %>%
  dplyr::filter(!is.na(sma10 & sma25))

#Understood the SMA logic, do not know how to illustrate the crossing points

#Buy signal - when a short-run SMA crosses from below to above a long-run SMA.
#Sell signal - when a short-run SMA crosses from above to above a long-run SMA.