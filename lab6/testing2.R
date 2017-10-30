library(graphics)
df <- airquality
boxplot(Ozone ~ Month, data = df)
kruskal.test(Ozone ~ Month, data = df)


N <- 50
k <- 5
WSS <- 2000
Mean_sqB <- 116.3
df1 <- k - 1
df2 <- N - k
BSS <- Mean_sqB*df1
Mean_sqW <- WSS/df2
f <- Mean_sqB/Mean_sqW
Total1 <- BSS + WSS
Total2 <- df1 + df2
s_f <- qf(0.95, df1 = 4, df2 = 45)
if(f > s_f){
  print("the means of the categories are not equal")
}
else
  print("the means of the categories are equal")