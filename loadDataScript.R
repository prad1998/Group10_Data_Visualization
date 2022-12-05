#Libraries:
library(readr) #Reader for csv.
library(ggplot2) #GGplot for graphs
library(ggpubr) #Regression and R^2
library(plotly) 

#Reading Data
cars <- read_csv("cars.csv")

#Quick summary of data set
summary(cars)

#View Dataset raw data
View(cars)

#Variables
Car<-cars$Car
Acceleration<-cars$Acceleration
MilesPerGallon <- cars$MPG
Horsepower<-cars$Horsepower
Weight<-cars$Weight
Displacement<-cars$Displacement
Cylinders<-cars$Cylinders

#Function to plot
ggPlotFunction <- function(xValue, yValue) {
  ggplot(data=cars, aes(x=xValue, y=yValue)) + geom_point() +
    geom_smooth(method = lm) +
    stat_regline_equation(aes(label = ..eq.label..)) +  
    stat_regline_equation(aes(label = ..rr.label..))
}

#Plots using ggplot
#Correlation between Acceleration and MPG
ggPlotFunction(Acceleration, MilesPerGallon)
ggPlotFunction(Horsepower, MilesPerGallon)
ggPlotFunction(Cylinders, MilesPerGallon)
ggPlotFunction(Weight, MilesPerGallon)
ggPlotFunction(Displacement, MilesPerGallon)

#Correlation between Acceleration and MPG
p1<- ggplot(data=cars, aes(x=MilesPerGallon, y=Acceleration)) + geom_point(aes(key=Car)) +
geom_smooth(method = lm) 


#Correlation between MPG and Horsepower
p2<- ggplot(data=cars, aes(x=MilesPerGallon, y=Horsepower)) + geom_point(aes(key=Car)) + 
  geom_smooth(method = lm) 

#Correlation between MPG and Cylinders
p3<- ggplot(data=cars, aes(x=MilesPerGallon, y=Cylinders)) + geom_point(aes(key=Car)) + 
  geom_smooth(method = lm) 
#Correlation between MPG and Weight
p4<- ggplot(data=cars, aes(x=MilesPerGallon, y=Weight)) + geom_point(aes(key=Car)) + 
  geom_smooth(method = lm) 

#Correlation between MPG and Displacement
p5<- ggplot(data=cars, aes(x=MilesPerGallon, y=Displacement)) + geom_point(aes(key=Car)) + 
  geom_smooth(method = lm) 

ggplotly(p1)
ggplotly(p2)
ggplotly(p3)
ggplotly(p4)
ggplotly(p5)
