#Problem 1----
coin <- c("Head" = 0, "Tails" = 1)


  for (j in 1:1000){
  Budget = 100
  Bet = 1
  
 while (Budget > 0){
  
  toss <-sample(coin, 1,replace = TRUE, prob = c(0.514,0.486 ))
  
  if (c(toss)==1){
  Budget = Budget + Bet 
  }
  else {
  Budget = Budget - Bet
  Bet = min(Budget, Bet*2)
  }
  print(Budget)
 } 
 }
 

#Problem 2----
library(tidyverse)

library(nycflights13)

nycflights13::flights
data <- flights

#Ex.5.2.4----

#1:
ArrDelay <- dplyr::filter(flights, arr_delay >= 120)

Houston <- dplyr::filter(flights, dest == "HOU"|dest == "IAH" )

OperatedBy <- dplyr::filter(flights, carrier %in% c("DL","UA", "AA"))

Summer <- dplyr::filter(flights,month %in% c(7,8,9))

Late <- dplyr::filter(flights, arr_delay > 120 & dep_delay <= 0)

Madeit <- dplyr::filter(flights, dep_delay >= 60 & c(dep_delay - arr_delay > 30))

NightNight <- dplyr::filter(flights, hour <=6 | hour==24)

#2:Illustrates values in a numeric vector that fall in a specific range

Summer2 <- dplyr::filter(flights,between(month,7,9))

#3: Arrival,departure, delays and air time is missing: flights got canceled.
missing <- dplyr::filter(flights, is.na(dep_time))

#4:These are not NA because their outcomes are always the same
NA ^ 0
NA | TRUE
FALSE & NA

NaN ^ 0
7 ^ 0
TRUE ^ 0

7 | TRUE
FALSE & TRUE

#Ex.5.3.1----

#1

x <- dplyr::arrange(flights, desc(is.na(flights$dep_time)), dep_time)


#2: 

m <- tail(dplyr::arrange (flights,desc(is.na(flights$dep_delay)), dep_delay))

l <- tail(dplyr::arrange (flights,desc(is.na(flights$dep_delay)), desc(dep_delay)))

#3

fastANDfurious <- head(dplyr::arrange (flights, air_time))

#4

far <- head(dplyr::arrange (flights, desc(distance)))
  
near <- tail(dplyr::arrange (flights, desc(distance)))



#Ex.5.4.1----

#1

one <- dplyr::select(flights, dep_time, dep_delay,arr_time, arr_delay)

two <- dplyr::select(flights,starts_with("dep"), starts_with("arr"))

three <- dplyr::select(flights, dep_time:arr_delay, -(sched_dep_time), -(sched_arr_time))

four <- dplyr::select(flights, dep_time, contains("delay"),arr_time)

#2:Nothing happens, it shows you the variable only once.

multiple <- dplyr::select(flights, dep_time, dep_time, dep_time)

#3:Selects variables contained in a character vector (does not check for missing values).

vars <- c("year", "month", "day", "dep_delay", "arr_delay")

c <- flights %>% dplyr::select(any_of(vars))

#4:It shows variables with time in their names, nothing surprising. It is not case sensitive, however we should use ignore.case if we want to minimize errors due to case sensitivity.

surprise <- dplyr::select(flights, contains("TIME"))

z <- dplyr::select(flights, contains("TIME", ignore.case = T))

#Ex.5.5.2----

#1 

new = dplyr::transmute(flights, 
        dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
        sched_dep_time = (sched_dep_time %/% 100) * 60 + (sched_dep_time %% 100)) 


#2:The values of "arr_time - dep_time" are higher.

com <- dplyr::transmute(flights, air_time, air_new = arr_time - dep_time)

fixed <- dplyr::transmute(flights, air_time,
                          dep_time = (dep_time %/% 100) * 60 + (dep_time %% 100),
                          arr_time = (arr_time %/% 100) * 60 + (arr_time %% 100),
                          air_time_new = arr_time - dep_time) 
  
  
#3:When you subtract scheduled dep_time from the actual dep_time you get the dep_delay

compare <- dplyr::select(flights, dep_time, sched_dep_time, dep_delay)

#4

top10 = head(dplyr::arrange(flights, desc(min_rank(flights$dep_delay))), n = 10)


#5

1:3 + 1:10 
#Warning message:longer object length is not a multiple of shorter object length
#for example
1:3 + 6:8

#6: in radians
cos(x)
sin(x)
tan(x)

acos(x)
asin(x)
atan(x)
atan2(y, x)

cospi(x)
sinpi(x)
tanpi(x)