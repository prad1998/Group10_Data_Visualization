library(readr)
library(ggplot2)
library(dplyr)
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
# line chart

toyota <- filter(cars, Car == "Toyota Corolla")

pinto <- filter(cars, Car == "Ford Pinto")

toyota

pinto


ggplot(toyota, aes(x=toyota$Model, y=toyota$MPG, group=1)) +
  geom_line()+
  geom_point()



ggplot(pinto, aes(x=pinto$Model, y=pinto$MPG, group=1)) +
  geom_line()+
  geom_point()


