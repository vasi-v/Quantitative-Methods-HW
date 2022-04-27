#Problem 4, HW 3----

#Again not complete/correct solutions  :/


library(tidyverse)

library(nycflights13)

nycflights13::flights
data <- flights

not_cancelled <- flights %>%
  filter (!is.na(air_time))

#4.1. 

One <- not_cancelled %>% 
  dplyr::group_by(carrier,dest) %>%
  dplyr::select(carrier,dest) %>%  
  dplyr::summarise(NrFlights = n()) %>%
  dplyr::arrange(desc(NrFlights)) %>%
  dplyr::slice_head()

#4.2.

two <- not_cancelled %>%
  dplyr::arrange(desc(arr_delay)) %>%
  dplyr::group_by(carrier)%>%
  dplyr::slice_head()

two.2 <- not_cancelled %>%
  dplyr::group_by(carrier)%>%
  dplyr::summarise((most = base::max(arr_delay, na.rm = TRUE)))

#4.3.?Questionable solution

three <- not_cancelled %>%
  dplyr::select(carrier, flight, distance) %>%
  dplyr::group_by(carrier,flight,distance) %>%
  dplyr::arrange(distance) 

#4.4.?attempt:

four <- not_cancelled %>%
  dplyr:: filter(month == 2) %>%
  dplyr::group_by(day) %>%
  dplyr::arrange(time_hour)%>%
  dplyr::summarise(first= dplyr::first(time_hour),
                   last= dplyr::last(time_hour))

#4.5.???

five <- not_cancelled %>%
  dplyr::filter(month == 3) %>%
  dplyr::group_by(carrier)
  
#4.6.?

#4.7.?

#4.8.??

n <- not_cancelled %>%
  dplyr::group_by(month,dest) %>%
  SDFunction(arr_delay)

  
