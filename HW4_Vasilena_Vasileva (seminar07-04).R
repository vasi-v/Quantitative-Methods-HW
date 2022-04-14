library(tidyquant)
library(tidyverse)

#Problem 1----
#1 

data<-tq_get(c("AMZN","FB", "NFLX"),
             from= "2019-01-01 ",
             to="2021-04-01") %>%
      dplyr::select(symbol, date, adjusted)
#2

dates<-data.frame(Dates = seq.Date( from = ymd("2019-01-01"),
                                    to = ymd("2021-04-01"),
                                    by = "day"))
Final <- dates %>%
  dplyr::left_join(data, by = c("Dates" = "date"))%>%
  fill(symbol,adjusted) %>%
  fill(symbol,adjusted, .direction = "up")

#3:I do not know how to do it, my process:

#?# 
    data_new<-data.frame(tq_get(c("AMZN","FB"),
             from = "2019-01-01",
             to ="2019-07-01"))

    data_new2<-data.frame(tq_get(c("AMZN","FB"), 
            from = "2020-04-01",
           to = "2020-07-01"))
    
    Result <- rbind(data_new, data_new2) %>%
      dplyr::arrange (symbol) %>%
      dplyr::arrange(desc(date))
    
#4:
    
select <- Result %>%
      dplyr::slice_head(n=2)
    
select2 <- Result %>%
      dplyr::slice_tail(n=2)

FinalSelect <- rbind(select,select2)
    
#5:I don't know how to do it

#Problem 2----
 #I did not quite understand the function
