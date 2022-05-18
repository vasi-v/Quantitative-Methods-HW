#Problem 4, HW 3----

#Your help was very much appreciated!


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
  dplyr::arrange(dplyr::desc(NrFlights)) %>%
  dplyr::slice_head()

#4.2.

two <- not_cancelled %>%
  dplyr::arrange(dplyr::desc(arr_delay)) %>%
  dplyr::group_by(carrier)%>%
  dplyr::slice_head()

two.2 <- not_cancelled %>%
  dplyr::group_by(carrier)%>%
  dplyr::summarise((most = base::max(arr_delay, na.rm = TRUE)))

#4.3./Your help/:

three <- not_cancelled %>%
  dplyr::group_by(tailnum) %>%
  dplyr::summarise(TotalDistance = base::sum(distance))  %>%
  dplyr::arrange(dplyr::desc(TotalDistance))%>%
  dplyr::slice(1:3, (n()-2):n())%>%
  dplyr::ungroup()

#4.4.

four <- not_cancelled %>%
  dplyr::filter(month == 2) %>%
  dplyr::arrange(day, dep_time)%>%
  dplyr::group_by(day) %>%
  dplyr::slice(1, n())%>%
  dplyr::ungroup()
 

#4.5./Your help/:

five <- not_cancelled %>%
  dplyr::filter(month == 3) %>%
  dplyr::group_by(carrier)%>%
  dplyr::mutate(TotalMiles = base::sum(distance))%>%
  dplyr::ungroup()%>%
  dplyr::arrange(dplyr::desc(TotalMiles))%>%
  dplyr::slice(1, n())

#I do not filter for year, because all of the flights are for 2013

#4.6./With your help/:

six <-not_cancelled %>%
  dplyr::group_by(month)%>%
  dplyr::summarise(TotalDelay = base::sum(arr_delay>60))%>%
  dplyr::ungroup()%>%
  dplyr::slice_max(TotalDelay)


#4.7./Your help/:

seven <- not_cancelled %>%
  dplyr::summarise(TimebetweenFlights = base::mean(dep_time - dplyr::lag(dep_time), na.rm = TRUE))%>%
  base::round(digits = 3)


#4.8./Your help/:

SDFunction <- function(inputVector){
  Denominator = sum(inputVector)/length(inputVector)
  Nominator = sum((inputVector - Denominator)^2)
  Result <- sqrt(sum((inputVector - Denominator)^2)/(length(inputVector)-1))
  return(Result)
}  

  n <- not_cancelled %>%
    dplyr::group_by(month,dest) %>%
    dplyr::summarise(sdDelay = SDFunction(arr_delay))%>%
    dplyr::ungroup()  
  
  



