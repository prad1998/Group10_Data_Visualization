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


corolla <- filter(cars, Car == "Toyota Corolla")

corolla

pinto <- filter(cars, Car == "Ford Pinto" )

pinto 

rabbit <- filter(cars, Car =="Volkswagen Rabbit")

rabbit



ggplot(corolla, aes(x=corolla$MPG, y=corolla$Model, group=1)) +
  geom_line()+
  geom_point()

ggplot(pinto, aes(x=pinto$MPG, y=pinto$Model, group=1)) +
  geom_line()+
  geom_point()

ggplot(rabbit, aes(x=rabbit$MPG, y=rabbit$Model, group=1)) +
  geom_line()+
  geom_point()

p = ggplot() + 
  geom_line(data = corolla, aes(x = Model, y = MPG, group=1, color = "blue")) + 
  geom_line(data = pinto, aes(x = Model, y = MPG, group=1, color= "yellow")) +
  geom_line(data = rabbit, aes(x = Model, y = MPG, group=1, color = "brown")) +
  ggtitle("The consumption of fuel changed over the years in each region")+
  theme(plot.title = element_text(hjust = 0.5)) +
  xlab('Model') +
  ylab('MPG') + 
 
  scale_x_continuous(breaks = seq(70, 82, by=3), limits=c(70,82))

print(p)

