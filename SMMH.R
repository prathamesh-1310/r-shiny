# Load required libraries
library(shiny)
library(dplyr)
library(ggplot2)
library(shinythemes)  # Load shinythemes library for Cerulean theme

# Define UI for application with Cerulean theme
ui <- fluidPage(
  
  # Application title
  titlePanel("Social Media and Mental Health Dashboard"),
  
  # Use Cerulean theme
  theme = shinytheme("cerulean"),  # Apply Cerulean theme
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel
    sidebarPanel(
      # Select variable for x-axis
      selectInput("xvar", "X-axis variable:",
                  choices = c("Age", "RelationShip_Status", "Occupation_Status")),
      
      # Select variable for y-axis
      selectInput("yvar", "Y-axis variable:",
                  choices = c("Distraction", "Feel_Rstless", "Bother_by_Worries", "Difficult_to_Concentrate", "Depress", "Slepping_Issues")),
      
      # Select plot type
      selectInput("plottype", "Plot type:",
                  choices = c("Bar plot", "Box plot", "Scatter plot"))
    ),
    
    # Main panel
    mainPanel(
      # Display plot
      plotOutput("plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  
  # Load data
  data <- read.csv("smmh.csv")
  
  
  # Render plot based on user inputs
  output$plot <- renderPlot({
    xvar <- switch(input$xvar,
                   "Age" = data$X1..What.is.your.age.,
                   "RelationShip_Status" = factor(data$X3..Relationship.Status, levels = c("Single", "In a relationship", "Married", "Divorced")),
                   "Occupation_Status" = factor(data$X4..Occupation.Status, levels = c("University Student", "Salaried Worker", "School Student","Retired"))
    )
    
    
    yvar <- switch(input$yvar,
                   "Distraction" = data$X10..How.often.do.you.get.distracted.by.Social.media.when.you.are.busy.doing.something.,
                   "Feel_Rstless" = data$X11..Do.you.feel.restless.if.you.haven.t.used.Social.media.in.a.while.,
                   "Bother_by_Worries" = data$X13..On.a.scale.of.1.to.5..how.much.are.you.bothered.by.worries.,
                   "Difficult_to_Concentrate" = data$X14..Do.you.find.it.difficult.to.concentrate.on.things.,
                   "Depress" = data$X18..How.often.do.you.feel.depressed.or.down.,
                   "Slepping_Issues" = data$X20..On.a.scale.of.1.to.5..how.often.do.you.face.issues.regarding.sleep.)
    
    if (input$plottype == "Bar plot") {
      ggplot(data, aes_string(x = xvar, y = yvar)) +
        geom_bar(stat = "identity") +
        labs(x = input$xvar, y = input$yvar)
    } else if (input$plottype == "Box plot") {
      ggplot(data, aes_string(x = xvar, y = yvar)) +
        geom_boxplot() +
        labs(x = input$xvar, y = input$yvar)
    } else if (input$plottype == "Scatter plot") {
      ggplot(data, aes_string(x = xvar, y = yvar)) +
        geom_point() +
        labs(x = input$xvar, y = input$yvar)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
