# Problem 1----

  FactorialFunction <- function(inputNumber){
    if (inputNumber == 0) {
      Result <- 1
    } else {
      
      Result <- inputNumber
      
      while(inputNumber > 1){
        Result <- (inputNumber - 1) * Result
        inputNumber <- inputNumber - 1
      }
    }
    return(Result)
  }
  
  FactorialFunction (6)
  factorial(6)
  
  6*5*4*3*2*1
  
  
  
# Problem 2----
  SDFunction <- function(inputVector){
    Denominator = sum(inputVector)/length(inputVector)
    Nominator = sum((inputVector - Denominator)^2)
    Result <- sqrt(sum((inputVector - Denominator)^2)/(length(inputVector)-1))
         return(Result)
  }
  try <- c(7,8,9,3)
  SDFunction(try)
  sd(try)
  
# Problem 3----
  library(tidyverse)
  
  library(nycflights13)
  
  nycflights13::flights
  data <- flights
  
  not_cancelled <- flights %>%
    filter (!is.na(air_time))
    
    
#5.6.7
  
#1:I find this one to be challenging
  
#2:Still need to figure out other approaches to these functions, although I found some answers
 f= not_cancelled %>% count(dest)
 d = not_cancelled %>% count(tailnum, wt = distance)
  
#3:It is enough to know whether the air_time is NA (canceled flight), or not (flight happened).

#4:Not final result, but:
  day<- dplyr::group_by(flights,year,month,day) %>%
    dplyr::summarise( 
    canceled = sum(is.na(air_time)|air_time ==0),
    avg_ad = mean(arr_delay,na.rm = TRUE),
    avg_dd = mean(dep_delay,na.rm = TRUE)
    )
#5:not done
  
#6: sort => If TRUE, will show the largest groups at the top.

  
#5.7.1: not done  
  
# Problem 4----
 #4.1. my thought process:
  One <- not_cancelled %>% dplyr::group_by(carrier) %>%
        dplyr::select(carrier,dest) %>%  
        dplyr::count(dest)
 
    
  #answer i have found: 
   #adv3 <-not_cancelled %>% 
    #group_by(carrier, dest) %>%
    #summarise(n = n()) %>%
    #mutate(rank = min_rank(desc(n))) %>%
    #filter(rank == 1)
    
  #4.2.
  
  Two <- not_cancelled %>% dplyr::group_by(carrier) %>%
    dplyr::select(carrier, max(not_cancelled$arr_delay))
  
  
  
  #still not done
  


  
  
  