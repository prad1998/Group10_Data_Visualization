#Libraries:
library(readr) #Reader for csv.
library(ggplot2) #GGplot for graphs
library(ggpubr) #Regression and R^2

#Reading Data
cars <- read_csv("cars.csv")

#Quick summary of data set
summary(cars)

#View Dataset raw data
View(cars)

#Variables
Acceleration<-cars$Acceleration
MilesPerGallon <- cars$MPG
Horsepower<-cars$Horsepower
Weight<-cars$Weight
Displacement<-cars$Displacement
Cylinders<-cars$Cylinders

#Plots using ggplot
#Correlation between Acceleration and MPG
ggplot(data=cars, aes(x=Acceleration, y=MilesPerGallon)) + geom_point() +
geom_smooth(method = lm) +
  stat_regline_equation(label.y = 60, aes(label = ..eq.label..)) +  
  stat_regline_equation(label.y = 50, aes(label = ..rr.label..))

#Correlation between Acceleration and Horsepower
ggplot(data=cars, aes(x=Acceleration, y=Horsepower)) + geom_point() + 
  geom_smooth(method = lm) +
  stat_regline_equation(label.y = 70, aes(label = ..eq.label..)) + 
  stat_regline_equation(label.y = 50, aes(label = ..rr.label..))

#Correlation between Acceleration and Cylinders
ggplot(data=cars, aes(x=Acceleration, y=Cylinders)) + geom_point() + 
  geom_smooth(method = lm) +
  stat_regline_equation(label.y = 20, aes(label = ..eq.label..)) + 
  stat_regline_equation(label.y = 15, aes(label = ..rr.label..))

#Correlation between Acceleration and Weight
ggplot(data=cars, aes(x=Acceleration, y=Weight)) + geom_point() + 
  geom_smooth(method = lm) +
  stat_regline_equation(label.y = 900, aes(label = ..eq.label..)) + 
  stat_regline_equation(label.y = 800, aes(label = ..rr.label..))

#Correlation between Acceleration and Displacement
ggplot(data=cars, aes(x=Acceleration, y=Displacement)) + geom_point() + 
  geom_smooth(method = lm) +
  stat_regline_equation(label.y = 750, aes(label = ..eq.label..)) + 
  stat_regline_equation(label.y = 700, aes(label = ..rr.label..))
