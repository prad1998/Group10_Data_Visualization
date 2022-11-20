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


######## Boxplot 

library(readr) 
library(ggplot2) 
library(dplyr)
library(gganimate)


#Reading Data
cars <- read_csv("cars.csv")

amc <- filter(cars, Car == "AMC Matador")

#static grouped boxplots for region  

ggplot(cars, aes(x=as.factor(Origin), y=MPG)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("Origin")


#animated grouped boxplots for region that have Year as state   
ggplot(cars, aes(factor(Origin), MPG)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  # The gganimate code
  transition_states(
    Model,  
    transition_length = 20,
    state_length = 1
  ) + labs(title = "Year: 19{closest_state}")  +
  
  # Used to control the non-persistent data during a tween  
  enter_fade() + 
  exit_shrink()
  







