######## Boxplot 
library(shinydashboard)
library(shiny)
library(readr) 
library(ggplot2) 
library(dplyr)
library(gganimate)
library(ggpubr)
library(plotly) 
library(rcartocolor)
library(gifski)

#Reading Data
cars <- read_csv("cars.csv")

amc <- filter(cars, Car == "AMC Matador")

#static grouped boxplots for region  

ggplot(cars, aes(x=as.factor(Origin), y=MPG)) + 
  geom_boxplot(fill="#245668", alpha=0.2) + 
  xlab("Origin")


#Variables
Car<-cars$Car
Origin<-cars$Origin
Acceleration<-cars$Acceleration
MilesPerGallon <- cars$MPG
Horsepower<-cars$Horsepower
Weight<-cars$Weight
Displacement<-cars$Displacement
Cylinders<-cars$Cylinders

ui <- dashboardPage(
  dashboardHeader(title = "Dashboard of cars eller noget" ),
  dashboardSidebar(disable = TRUE),
  
  skin = "purple",
  dashboardBody(
    # Boxes need to be put in a row (or column)
  
                box(
                  title = "Static Boxplot: Correlation between MPG and Origin ",
                  plotOutput("plot_box")
                ),
                
                box(
                  title = "Animated Box plot: Correlation between MPG and Origin over years",
                  imageOutput("plot_boxs", height = "100%", width = "100%")
                ),
                
                box(
                  title = "Histogram: ",
                  imageOutput("Histogram_plot")
                ),
                
                selectInput("Variable", "Select what correlation to examine:",
                            list(`Variables` = list("Acceleration", "Horsepower", "Cylinders", "Weight", "Displacement"))
                ),
                textOutput("selectedVariable"),
                
                box(
                  title = "Scatterplot: Correlation with MPG",
                  plotlyOutput("Scatterplot_plot")
                )

                ,tags$footer("A color for our footer that we should decide"), 
  )
)
server <- function(input, output) {
  
  outfile <- tempfile(fileext='.gif')
  

  output$plot_box <- renderPlot({ggplot(cars, aes(x=as.factor(Origin), y=MPG, fill=Origin)) + 
    geom_boxplot(alpha=0.2) +
    scale_fill_manual(values=c("#4b2991", "#c0369d", "#fa7876")) + 
    xlab("Origin")})
  
  
  
  mpgTable <- as.numeric(cars$MPG)
  mpgTable
  mpgTable <- cut(mpgTable, breaks=c(0, 5, 10, 15, 20, 25, 30, 35, 40))
  
  mpgDf <- data.frame(mpgTable)
  
  histo <- ggplot(mpgDf, aes(x=mpgTable)) + 
    geom_bar()
  
  
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
  
  colnames(yiit) <- c("yiit", "Origin")
  colnames(yiit2) <- c("yiit", "Origin")
  colnames(yiit3) <- c("yiit", "Origin")
  
  nest <- rbind(yiit, yiit2)
  nest <- rbind(nest, yiit3)
  
  cbbPalette <- cbbPalette <- carto_pal(4, "ag_Sunset")
  
  
  # MPG eller mpgtable? 
  stacked <- ggplot(data=nest, aes(x=mpgTable, y=1, fill=Origin)) +
    geom_bar(stat="identity") +
    scale_fill_manual(values=cbbPalette)
  stacked <- stacked + labs(title = "MPG Distribution", x = "MilesPerGallon", y = "Samples")
  
  output$Histogram_plot <- renderPlot({stacked})
  
  output$selectedVariable <- function(){
    if(input$Variable == "Acceleration"){
      Scatterplot_plot<-ggplot(data=cars, aes(x=MilesPerGallon, y=Acceleration, col=Origin)) + 
        geom_point(aes(key=Car)) + 
        geom_smooth(method = lm)
      col<-carto_pal(4, "ag_Sunset")
      Scatterplot_plot<-Scatterplot_plot + scale_color_manual(values=col) 
      ggplotly(Scatterplot_plot)
      output$Scatterplot_plot<-renderPlotly({ Scatterplot_plot})
      
    } else if (input$Variable == "Horsepower"){
      Scatterplot_plot<-ggplot(data=cars, aes(x=MilesPerGallon, y=Horsepower, col=Origin)) + 
        geom_point(aes(key=Car)) + 
        geom_smooth(method = lm)
      col<-carto_pal(4, "ag_Sunset")
      Scatterplot_plot<-Scatterplot_plot + scale_color_manual(values=col) 
      ggplotly(Scatterplot_plot)
      output$Scatterplot_plot <-  renderPlotly({ Scatterplot_plot})
    } else if(input$Variable == "Cylinders"){
      Scatterplot_plot<-ggplot(data=cars, aes(x=MilesPerGallon, y=Cylinders, col=Origin)) + 
        geom_point(aes(key=Car)) + 
        geom_smooth(method = lm)
      col<-carto_pal(4, "ag_Sunset")
      Scatterplot_plot<-Scatterplot_plot + scale_color_manual(values=col) 
      ggplotly(Scatterplot_plot)
      output$Scatterplot_plot<-renderPlotly({ Scatterplot_plot})
      
    } else if(input$Variable == "Weight"){
      Scatterplot_plot<-ggplot(data=cars, aes(x=MilesPerGallon, y=Weight, col=Origin)) + 
        geom_point(aes(key=Car)) + 
        geom_smooth(method = lm)
      col<-carto_pal(4, "ag_Sunset")
      Scatterplot_plot<-Scatterplot_plot + scale_color_manual(values=col) 
      ggplotly(Scatterplot_plot)
      output$Scatterplot_plot <-  renderPlotly({ Scatterplot_plot})
      
    } else if(input$Variable == "Displacement"){
      Scatterplot_plot<-ggplot(data=cars, aes(x=MilesPerGallon, y=Displacement, col=Origin)) + 
        geom_point(aes(key=Car)) + 
        geom_smooth(method = lm)
      col<-carto_pal(4, "ag_Sunset")
      Scatterplot_plot<-Scatterplot_plot + scale_color_manual(values=col) 
      ggplotly(Scatterplot_plot)
      output$Scatterplot_plot <-  renderPlotly({ Scatterplot_plot})
      
    }
    ""
  }

  #animated grouped boxplots for region that have Year as state   
  output$plot_boxs <- renderImage(
    {
     boxplottest <- ggplot(cars, aes(factor(Origin), MPG,  fill=Origin)) + 
    geom_boxplot(alpha=0.2) +
    scale_fill_manual(values=c("#4b2991", "#c0369d", "#fa7876"))
       
  
     
    anim <- boxplottest +  
      # The gganimate code
      transition_states(
        Model,  
        transition_length = 20,
        state_length = 1
      ) + labs(title = "Year: 19{closest_state}")  +
      
      # Used to control the non-persistent data during a tween  
      enter_fade() + 
      exit_shrink()
  
    anim_save("outfile.gif", animate(anim))  
    list(src = "outfile.gif", contentType = "image/gif")
    
    }, deleteFile = TRUE
    
    )
  
}
shinyApp(ui, server)


  
















