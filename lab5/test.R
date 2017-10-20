## Q1
q95 <- qnorm(0.975)
n <- round(q95*q95*4/(0.1*0.1),digits = 0)
n
## Q2
library(MASS)
x <- mvrnorm( n = 50, mu = 18.5, Sigma = 49, empirical = T)
y <- t.test(x, conf.level = 0.95)
z <- y$conf.int
test <- 16
if(test >= z[1] && test <= z[2])
{
  print("tolerable level is within this interval")
} else
print("tolerable level is not within this interval")
## Q3
p <- 0.24
test1 <- 0.165
q90 <- qnorm(0.95)
conf.90 <- c(p - q90*sqrt(p*(1-p)/50),p
             + q90*sqrt(p*(1-p)/50))
if(test1 >= conf.90[1] && test1 <= conf.90[2])
{
  print("the town has a mobility rate that is the same with the national average")
} else
  print("the town has a mobility rate that is different from the national average")
## Q4
### Part a
x1 <- mvrnorm(n = 20, mu = 4.1, Sigma = 14.3, empirical = T)
x2 <- mvrnorm(n = 16, mu = 3.1, Sigma = 12, empirical = T)
result1 <-t.test(x1,x2,var.equal = F)
result1
if(result1$p.value > 0.05){
  print("we cannot reject the null hypothesis")
} else{
  print("we reject the null hypothesis")
}
### Part b

### Part c
result1$p.value
### Part d
result <- t.test(x1)
result$conf.int
### Part e


## Q5
library(MASS)
x <- mvrnorm(17, mu = 6.4, Sigma = 4.4*4.4, empirical = T)
result <- t.test(x, mu = 4.2)
result
if(result$p.value > 0.05){
  print(result$p.value)
  print("we cannot reject the null hypothesis")
  print("there is no suffucient evidence to prove pollutant levelexceeds the allawable limit")
} else{
  print("we reject the null hypothesis")
}
## Q6
library(MASS)
x1 <- mvrnorm(n = 52, mu = 3.4, Sigma = 1.1*1.1, empirical = T)
x2 <- mvrnorm(n = 62, mu = 2.8, Sigma = 0.8*0.8, empirical = T)
result <- t.test(x1,x2)
result
if(result$p.value > 0.05){
  print("we cannot reject the null hypothesis")
} else{
  print("we reject the null hypothesis")
}
## Q7
## Q8
result <- prop.test(x=c(8.8,40), n=c(15,50), 
                    conf.level = 0.9)
result
y <- result$conf.int
if(result$p.value > 0.1){
  print("we cannot reject the null hypothesis")
  print("two communities are the same")
  print(result$p.value)
  print(c(y[1],y[2]))
} else{
  print("we reject the null hypothesis")
  print("two communities are different")
}
## Q9
## Q10
result <- prop.test(x = 7.5, n = 50, p = 0.1)
result
if(result$p.value > 0.05)
{
  print("No significant difference from statewide average")
} else {
  print("significant difference from statewide average")
}