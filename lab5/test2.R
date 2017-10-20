## Q5
mean_s <- 6.4
q80 <- qt(0.9,df = 4.4)
q90 <- qt(0.95,df = 4.4)
str.er <- 4.4/sqrt(17)
conf.80 <- c(mean_s-q80*str.er, mean_s+q80*str.er)
test <- 4.2
if(test >= conf.80[1] && test <= conf.80[2])
{
  print("")
} else
  print("")