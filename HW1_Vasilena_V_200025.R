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
choose <- sample(people, size = 5, replace = FALSE)
for (i in choose){
  if (i=="men")
    print (i)
  else 
    print("women")
}

#Problem 4----

