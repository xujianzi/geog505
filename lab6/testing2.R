library(graphics)
df <- airquality
boxplot(Ozone ~ Month, data = df)
kruskal.test(Ozone ~ Month, data = df)