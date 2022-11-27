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




library(shinydashboard)
library(shiny)


ui <- dashboardPage(
  dashboardHeader(title = "Dashboard of cars eller noget"),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    # Boxes need to be put in a row (or column)
  
                box(
                  title = "Statisk Box Plot for sjov",
                  plotOutput("plot_box")
                ),
                
                box(
                  title = "Animated Box plot",
                  imageOutput("plot_boxs", height = "100%", width = "100%")
                ),
                
                box(
                  title = "Histogram",
                  imageOutput("Histogram_plot", height = "100%", width = "100%")
                ),
                
                box(
                  title = "Scatterplot",
                  imageOutput("Scatterplot_plot", height = "100%", width = "100%")
                )

  )
)
server <- function(input, output) {
  
  outfile <- tempfile(fileext='.gif')
  

  output$plot_box <- renderPlot({ggplot(cars, aes(x=as.factor(Origin), y=MPG)) + 
    geom_boxplot(fill="slateblue", alpha=0.2) + 
    xlab("Origin")})
  
  
  output$Histogram_plot <- 
  
  #animated grouped boxplots for region that have Year as state   
  output$plot_boxs <- renderImage(
    {
    
     boxplottest <- ggplot(cars, aes(factor(Origin), MPG)) + 
    geom_boxplot(fill="slateblue", alpha=0.2)
  
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


  
















