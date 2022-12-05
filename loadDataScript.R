#Libraries:
library(readr) #Reader for csv.
library(ggplot2) #GGplot for graphs
library(plotly) 
library(rcartocolor)

#Reading Data
cars <- read_csv("cars.csv")

#Variables
Car<-cars$Car
Origin<-cars$Origin
Acceleration<-cars$Acceleration
MilesPerGallon <- cars$MPG
Horsepower<-cars$Horsepower
Weight<-cars$Weight
Displacement<-cars$Displacement
Cylinders<-cars$Cylinders

#Function to plot
functionToPlot <- function(xval, yval) {
  xLabel<-rlang::as_label(rlang::ensym(xval))
  yLabel<-rlang::as_label(rlang::ensym(yval))
  p<- ggplot(data=cars, aes(x=xval, y=yval, col=Origin)) + geom_point(aes(key=Car)) + 
    geom_smooth(method = lm) 
  col<-carto_pal(4, "Vivid")
  p<-p + scale_color_manual(values=col)
  p<-p + labs(x = xLabel, y = yLabel)
  ggplotly(p)
}

#Plotting
functionToPlot(MilesPerGallon, Displacement)
functionToPlot(MilesPerGallon, Weight)
functionToPlot(MilesPerGallon, Cylinders)
functionToPlot(MilesPerGallon, Horsepower)
functionToPlot(MilesPerGallon, Acceleration)


