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

us <- filter(cars, Origin == "US")


us
pinto <- filter(cars, Car == "Ford Pinto")



#pinto


ggplot(us, aes(x=us$MPG, y=us$Model, group=1)) +
  geom_line()+
  geom_point()



ggplot(pinto, aes(x=pinto$Model, y=pinto$MPG, group=1)) +
  geom_line()+
  geom_point()


p = ggplot() + 
  geom_line(data = us, aes(x = Model, y = MPG, group=1), color = "blue") +
  geom_line(data = pinto, aes(x = Model, y = MPG, group=1), color = "red") +
  xlab('Model') +
  ylab('MPG')

print(p)

