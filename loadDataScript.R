library(readr)
library(ggplot2)

cars <- read_csv("cars.csv")


originCount <- table(cars$Origin, dnn = "Origin")
#barplot(originCount, main = "Origin")
test <- data.frame(originCount)
colnames(test) <- c("Origin", "Frequency")
p <- ggplot(data=test, aes(x=Frequency, y=Origin)) + geom_bar(stat="identity")
p

