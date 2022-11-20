library(readr)
library(ggplot2)

cars <- read_csv("cars.csv")


originCount <- table(cars$Origin, dnn = "Origin")
originDataFrame <- data.frame(originCount)
colnames(originDataFrame) <- c("Origin", "Frequency")
p <- ggplot(data=originDataFrame, aes(x=Frequency, y=Origin)) + 
  geom_bar(stat="identity")
p

mpgTable <- as.numeric(cars$MPG)
mpgTable
mpgTable <- cut(mpgTable, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))

mpgDf <- data.frame(mpgTable)

histo <- ggplot(mpgDf, aes(x=mpgTable)) + 
  geom_bar()
histo

