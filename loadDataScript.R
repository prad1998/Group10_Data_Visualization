#Libraries:
library(readr) #Reader for csv.
library(ggplot2) #GGplot for graphs

#Reading Data
cars <- read_csv("cars.csv")

#Quick summary of data set
summary(cars)

#View Dataset raw data
View(cars)

#Variables
Acceleration<-cars$Acceleration
Fuel <- cars$MPG
Horsepower<-cars$Horsepower

#Plots using ggplot
ggplot(data=cars, aes(x=Acceleration, y=Fuel)) + geom_point(size=1)
ggplot(data=cars, aes(x=Acceleration, y=Horsepower)) + geom_point(size=1)

originCount <- table(cars$Origin, dnn = "Origin")
#barplot(originCount, main = "Origin")
originDataFrame <- data.frame(originCount)
colnames(originDataFrame) <- c("Origin", "Frequency")
p <- ggplot(data=originDataFrame, aes(x=Frequency, y=Origin)) + 
  geom_bar(stat="identity")
p
