library(readr) #Reader for csv.
library(ggplot2) #GGplot for graphs
library(dplyr)
library(rcartocolor)
library(plotly)

#Reading Data
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

test <- data.frame(as.numeric(cars$MPG), cars$Origin)
split <- split(test, f = test$cars.Origin)


yeet <- cut(split$Europe$as.numeric.cars.MPG., breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
yiit <- data.frame(yeet)
yiit <- yiit %>%
  mutate(Origin = "Europe")
yiit
yeet2 <- cut(split$Japan$as.numeric.cars.MPG., breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
yiit2 <- data.frame(yeet2)
yiit2 <- yiit2 %>%
  mutate(Origin = "Japan")
yeet3 <- cut(split$US$as.numeric.cars.MPG., breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
yiit3 <- data.frame(yeet3)
yiit3 <- yiit3 %>%
  mutate(Origin = "USA")

colnames(yiit) <- c("MPG", "Origin")
colnames(yiit2) <- c("MPG", "Origin")
colnames(yiit3) <- c("MPG", "Origin")

nest <- rbind(yiit, yiit2)
nest <- rbind(nest, yiit3)

cbbPalette <- carto_pal(4, "ag_Sunset")

cbbPalette
stacked <- ggplot(data=nest, aes(x=MPG, y=1, fill=Origin)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=cbbPalette)
stacked <- stacked + labs(title = "MPG Distribution", x = "MilesPerGallon", y = "Samples")
stacked

