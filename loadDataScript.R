library(readr)
library(ggplot2)

cars <- read_csv("cars.csv")


originCount <- table(cars$Origin, dnn = "Origin")
#barplot(originCount, main = "Origin")
originDataFrame <- data.frame(originCount)
colnames(originDataFrame) <- c("Origin", "Frequency")
p <- ggplot(data=originDataFrame, aes(x=Frequency, y=Origin)) + 
  geom_bar(stat="identity")
p

