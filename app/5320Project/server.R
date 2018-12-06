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

# m <- require_symbol('XOM', symbol_env,date1 = 2010-01-01,date2=2018-12-06)
# df <- data.frame(date=index(m),m)
# head(df)

# date <- format(index(m))
# date <- format(time(m))
# head(date)


##-------------------------------------Server--------------------------------##
shinyServer(function(input, output) {
  # Create an environment for storing data
  symbol_env <- new.env()
  
  # Stock
  # Basics Part
  dataInput <- reactive({
    ticker <- require_symbol(input$stock_enter,date1 = input$dates[1],date2 = input$dates[2])
  })
  
  # generalData <- reactive({
  #   ticker <- require_symbol(input$stock_enter,date1 = input$start_date,date2 = input$end_date)
  #   df <- data.frame(date=index(ticker),ticker)
  # })
  
  output$plot1 <- renderDygraph({
    ticker <- dataInput()
    dygraph(Ad(ticker))%>%
      dyRangeSelector()
  })
  
  output$table1 <- renderTable({
    ticker <- dataInput()
    df <- data.frame(Date=format(index(ticker)),ticker)
  })
})
