## Q2
### homogeneity
library(reshape2)
library(car)
group1 <- rnorm( 12, mean = 17.17,sd = 13.25)
group2 <- rnorm(12, mean = 23.17, sd = 33.45)
sample <- as.data.frame(cbind(group1,group2))
dataset <- melt(sample)
leveneTest(value~variable,dataset)
### ANOV
low_income <- c(5,7,9,11,13,8,10,34,17,50,17,25)
high_income <- c(25,24,8,2,11,10,10,66,113,1,3,5)
income <- c(low_income,high_income)
type <- c(rep("low_income",12), rep("high_income",12))
df <- data.frame(income,type)
m <- aov(income~type, data = df)
m
summary(m)
qf(0.9,df1 = 1,df2 = 22)        
oneway.test(income~type, data = df, var.equal = T)
print("There is no difference in the two population means")

## Q3-9
IncomesA <- rnorm(n = 12,mean = 43.2,sd = 36.2) 
IncomesB <- rnorm(n = 10,mean = 34.3,sd = 20.3)
IncomesC <- rnorm(n = 8,mean = 27.2,sd = 21.4)
Incomes <- c(IncomesA,IncomesB,IncomesC)
group <- c(rep("a",12),rep("b",10),rep("c",8))
df <- data.frame(Incomes,group)
result <- aov(Incomes~group,data = df)
result
summary(result)
qf(p = 0.95,df1 = 2,df2 = )
print("reject the null hypothesis")
print("They are differet within these three groups in their incomes")

## Q4-10
Col1 <- c(23.1,13.3,15.6,1.2)
Col2 <- c(43.1,10.2,16.2,0.2)
Col3 <- c(56.5,32.1,43.3,24.4)
Col4 <- c(10002.3,54.4,8.7,54.4)
Col <- c(Col1,Col2,Col3,Col4)
group <- c(rep("Col1",4),rep("Col2",4),
           rep("Col3",4),rep("Col4",4))
df4 <- data.frame(Col,group)
kruskal.test(Col~group, data = df4)
print("I can't reject the null hypothesis")
print("the means of the four columns are equal")

## Q5-12
Low <- c(5,4,1,2,3,10,6,6,4,12,11)
Medium <- c(10,10,8,6,5,3,16,20,7,3,2)
High <- c(8,11,15,19,21,7,7,4,3,17,18)
income <- c(Low, Medium, High)
group <- c(rep("Low",11),rep("Medium",11),rep("High",11))
df5 <- data.frame(income,group)
m <- aov(income~group, data = df5)
m
summary(m)
qf(0.95,df1 = 2,df2 = 30)
print("the commuting distances do vary by income according to Anova")
leveneTest(income~group,data = df5)
print("the commuting distans do not vary by income according to Levene test")
hist(x = income)
shapiro.test(income)
print("normality distribution is not satisfied")

## Q6-13
cen <- rnorm(10,1.5,1)
sub <- rnorm(15,2.6,1.1)
rural <- rnorm(15,1.2,1.2)
residents <- c(cen,sub,rural)
area <- c(rep("cen",10),rep("sub",15),rep("rural",15))
df6 <- data.frame(residents,area)
result6 <- aov(residents~area,data = df6)
result6
summary(result6)
print("the means of three area are not equal")
