#Problem 1----
for (i in 1:10){
  print (i*3)
}
#Problem 2----
for (i in rnorm(10)){
  if (i>1)
  print (i)
  else 
  print("No")
}
#Problem 3----
people <- c(rep("men", 6), rep("women", 8))
ResultVector <- NULL

for (i in 1:10000){ 
  choose <- sample(people, size = 5, replace = FALSE)
  
    if (sum(choose=="men")==3){
      ResultVector<-c(ResultVector,1)
    } else{
      ResultVector<-c(ResultVector,0)
    }
  
}
 sum(ResultVector)/10000
  dhyper(3,6,8,5)
  

#Problem 4----
  SrikePrice <- 120
  Profit <- 0
  Price <- 100

  for (k in 1:1000){
    Price <- 100
    for(i in 1:100){
      Price = c(Price + rnorm(1, mean = 0, sd = 7))
    }
  if(Price > 120){
    Profit = Profit + Price - 120
  }
    
}
  Profit/1000
  
  
  