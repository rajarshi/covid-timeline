#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(htmlwidgets)


# Define UI for application that draws a histogram
shinyUI(fluidPage(theme = "bootstrap.css",
    titlePanel("COVID-19 Timeline"),
    fluidRow(
        column(8,
               fluidRow(plotlyOutput("distPlot")),
               fluidRow(verbatimTextOutput("click"))),
        column(4,
               fluidRow(imageOutput("map1")),
               fluidRow(imageOutput("map2")))
    )))
