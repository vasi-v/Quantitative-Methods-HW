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
  
#5.6.7
  
#1:I find this one to be challenging
  
#2:Still need to figure out other approaches to these functions, although I found some answers
  #not_cancelled %>% count(dest)
  #not_cancelled %>% count(tailnum, wt = distance)
  
#3: It is enough to know whether the air_time is NA (canceled flight), or not (flight happened).

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
  #not done
  
#I am aware that this homework is not complete.
#I will try to review the reading materials and to understand the functions.

  
  
  