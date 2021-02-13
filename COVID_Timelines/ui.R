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
library(leaflet)
library(leaflet.extras)
library(leaflet.esri)
library(shinyjs)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
    useShinyjs(), 
    
    theme = "bootstrap.css",
    titlePanel("COVID-19 Timeline"),
    
    shinyjs::hidden(textInput("hidden", "Will be hidden", "foo")),
    #textInput("hidden", "Will be hidden", "foo"),
    
    fluidRow(
        column(6,
               fluidRow(
                   tabsetPanel(id='tabs',
                       type="pills",
                       tabPanel("Cases", id='Cases', plotlyOutput("casesplot")),
                       tabPanel("Deaths", id='Deaths', plotlyOutput("deathsplot"))
                       
                       
                   )
               ),
               fluidRow(verbatimTextOutput("click"))),
        
        column(6,
               fluidRow(leafletOutput("map1"))
               )
    ),
    
    
    
))
