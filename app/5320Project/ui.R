#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library('shiny')
library('shinythemes')
library('quantmod')
library('magrittr')


# Define UI for application that draws a histogram
shinyUI(
  navbarPage("VaR Calculation System",
             
             tabPanel("Stock",
                      sidebarLayout(
                      sidebarPanel(
                        titlePanel('Input Parameters'),
                        textInput(inputId = "stock_enter", label="Ticker", value="XOM"
                        ),
                        dateInput(inputId = "start_date", label="Start Date",value = "2010-01-01"
                        ),
                        dateInput(inputId = "end_date", label="End Date"
                        ),
                        submitButton("Run System")
                      ),
                      mainPanel(
                        tabsetPanel(
                          tabPanel("Basics",
                                   h4("Stock Chart Daily Chart"),
                                   dygraphOutput("plot1"),
                                   h4("Stock Price(in Table)"),
                                   tableOutput(outputId="table1")
                            
                          ),
                          tabPanel("VaR"),
                          tabPanel('ES')
                        )
                      )
                    )
             ),
             
             tabPanel("Option"),
             
             tabPanel("Portfolio")

)
)
