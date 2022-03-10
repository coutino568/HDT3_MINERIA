

db <- read.csv("train.csv", header = TRUE, sep =",") 

library(hopkins) 

library(ggplot2)
library(ggfortify)

myfunction <- function(data, nc=15, seed=445)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 1:nc)
    set.seed(seed)
  wss[i] <- sum(kmeans(data, centers=i)$withinss)
  plot(1:nc, wss, type="b",xlab = " # CLUSTERS", ylab=" W S S")
}

myfunction(training)
myKmeans= kmeans(training,2)

training <- db[c(2,4,5,18,19,20,21,27,35,37,38,39,44,45,47,54,55,60,67,68,76,77)]

summary(db)

show(db)
