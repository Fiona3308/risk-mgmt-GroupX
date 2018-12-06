#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library('shiny')
library('shinythemes')
library('quantmod')
library('magrittr')
library(dygraphs)
library(DT)
library(data.table)

##------------------------------Define Funcions------------------------------##
# Download data for a stock if needed, and return the data
require_symbol <- function(symbol, envir = parent.frame(),date1,date2) {
  if (is.null(envir[[symbol]])) {
    envir[[symbol]] <- getSymbols(symbol,# src = "yahoo", 
                                  auto.assign = FALSE, from = date1,end=date2)
  }
  envir[[symbol]]
}





##-------------------------------------Server--------------------------------##
shinyServer(function(input, output) {
  # Create an environment for storing data
  # symbol_env <- new.env()
  
  # Stock
  # Basics Part
  dataInput <- reactive({
    ticker <- require_symbol(input$stock_enter,date1 = input$start_date,date2 = input$end_date)
  })
  
  output$plot1 <- renderDygraph({
    ticker <- dataInput()
    dygraph(Ad(ticker))%>%
      dyRangeSelector()
  })
  
  output$table1 <- renderDataTable({
    ticker <- dataInput()
  })
})
