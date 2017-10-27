## Q2
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

## Q4
Col1 <- c(23.1,13.3,15.6,1.2)
Col2 <- c(43.1,10.2,16.2,0.2)
Col3 <- c(56.5,32.1,43.3,24.4)
Col4 <- c(10002.3,54.4,8.7,54.4)
Col <- c(Col1,Col2,Col3,Col4)
group <- c(rep("Col1",4),rep("Col2",4),
           rep("Col3",4),rep("Col4",4))
df4 <- data.frame(Col,group)
##boxplot(Col~group,data = df)
kruskal.test(Col~group, data = df4)
print("I can't reject the null hypothesis")

## Q5
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
print("the commuting distances do not vary by income")




