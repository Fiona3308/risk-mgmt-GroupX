#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("VaR Calculation System"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      titlePanel('Input Parameters'),
      # numericInput(inputId ='numericInput',label='Horizon(days)'),
      # numericInput(label='Window(years)'),
      # numericInput(label='VaR Probability'),
      # numericInput(label='ES Probability'),
       sliderInput("bins",
                   "Number of bins:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("Stocks", plotOutput("distPlot")),
        tabPanel("Options"),
        tabPanel('Portfolio')
      )
    )
  )
))
