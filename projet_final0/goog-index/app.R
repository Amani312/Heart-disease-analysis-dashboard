# Load packages -----------------------------------------------------
library(shiny)
library(shinythemes)
library(dplyr)
library(readr)
library(shiny)
library(tidyverse)
# Load data ---------------------------------------------------------
coeur <- read_csv("heart_2020_cleaned.csv")
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Projet final"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
      
        sidebarPanel(
        # Select variable for y-axis ----------------------------------
          selectInput(inputId = "y", 
                      label = "Y-axis:",
                      choices = c("Race","Sex","HeartDisease","PhysicalHealth","BMI","MentalHealth","SleepTime"), #"PhysicalHealth","BMI","MentalHealth","SleepTime",
                      selected = "HeartDisease"),
          # Select variable for x-axis ----------------------------------
          selectInput(inputId = "x", 
                      label = "X-axis:",
                      choices = c("Race","Sex","HeartDisease","PhysicalHealth","BMI","MentalHealth","SleepTime"), #"PhysicalHealth","BMI","MentalHealth","SleepTime",
                      selected = "SleepTime"),
        # Select variable for colors ----------------------------------
        selectInput(inputId = "z", 
                    label = "Color by:",
                    choices = c("Race","Sex","HeartDisease","PhysicalHealth","BMI","MentalHealth","SleepTime"), 
                    selected = "Race"),

        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot"),
           DT::dataTableOutput(outputId = "table")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
      ifelse(coeur$HeartDisease=="Yes",1,0)
      table(coeur$HeartDisease)
      table(coeur$Sex)
      table(coeur$Race)
      table(coeur$HeartDisease, coeur$Sex,coeur$Race)
      ggplot(data = coeur, aes_string(x = input$x, Y=input$y,fill=input$z)) +
        geom_bar(position = "dodge",alpha=0.5)+
        theme_bw()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        #facet_wrap(~HeartDisease)
    })
    
    # Print data table if checked -------------------------------------
    output$table <- DT::renderDataTable({
      DT::datatable(data = coeur[, 1:10], 
                    options = list(pageLength = 10, rownames = FALSE) 
      ) 
    })
}

# Run the application
shinyApp(ui = ui, server = server)
