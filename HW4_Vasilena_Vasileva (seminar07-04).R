library(tidyquant)
library(tidyverse)

#Problem 1----

#1:

data<-tidyquant::tq_get(c("AMZN","FB","NFLX"),
             from = "2019-01-01",
             to = "2021-04-01") %>%
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

#First approach:
    data_new<-base::data.frame(tidyquant::tq_get(c("AMZN","FB"),
             from = "2019-01-01",
             to ="2019-07-01"))
      

    data_new2<-base::data.frame(tidyquant::tq_get(c("AMZN","FB"), 
            from = "2020-04-01",
            to = "2020-07-01"))
    
    ResultM <- data_new %>%
       dplyr::full_join(data_new2, by = NULL) %>%
       dplyr::arrange(symbol)%>%
       dplyr::arrange(dplyr::desc(date))
    
#Second approach:/seminars/ - correct
    
    ResultS <- Final %>% 
      dplyr::filter((Dates >= lubridate::ymd("2019-01-01") & Dates <= lubridate::ymd("2019-07-01")) |
                    (Dates >= lubridate::ymd("2020-04-01") & Dates <= lubridate::ymd("2020-07-01")),
                    Symbol %in% c("AMZN", "FB")) %>%
      dplyr::arrange(Symbol, dplyr::desc(Dates))
      
    
#4:
    
select <- ResultS %>%
  dplyr::group_by(Symbol)%>%
  dplyr::slice(c(1, n()))%>%
  dplyr::ungroup()
    
#5:

select2 <- Final %>%
  dplyr::mutate(DatesNew = base::substr( Dates , 1, 7)) %>%
  dplyr::group_by(Symbol, DatesNew) %>%
  dplyr::slice_tail()%>%
  dplyr::ungroup()

#Question: why do we have to rewrite the date? I know that without it nothing will happen,
#but is it because in the "old" format was too "confusing" for R?

#Your 4 suggestions for data formatting /Thanks for your help/
#Date1 = substr( Dates , 1, 7)
#Date2 = floor_date( Dates, "month")
#YYYYMM1 = zoo::as.yearmon(Dates)
#YYYYMM2 = format(Dates, "%Y-%m")
#Understood how those formats work, and I like the first one the most


#Problem 2----
 SMA <- Final %>%
  dplyr::mutate(sma10 =TTR::SMA(adjusted, n = 10),
                sma26 =TTR::SMA(adjusted, n = 26),
                LagSma10 = dplyr::lag(sma10),
                LagSma26 = dplyr::lag(sma26)) %>%
  dplyr::filter(!is.na(sma26)) %>%
  dplyr::mutate(cross = dplyr::case_when(LagSma10 > LagSma26 & sma10 < sma26 ~ "from above",
                                         LagSma10 < LagSma26 & sma10 > sma26 ~ "from below",
                                         TRUE ~ "no cross"))
  
#I do not know why "no crosses" were found.

#Buy signal - when a short-run SMA crosses from below to above a long-run SMA.
#Sell signal - when a short-run SMA crosses from above to above a long-run SMA.