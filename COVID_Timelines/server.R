#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(dplyr)
library(lubridate)
library(htmlwidgets)

source("data.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlotly({

        p <- dat %>% 
            plot_ly() %>% 
            add_trace(type='bar',  x=~Date, y=~Daily_Cases, 
                      text = paste(dat$Daily_Cases, "cases<br><i>", dat$Daily_Cases_cum, "cumulative cases</i>" ),
                      hoverinfo = "text",
                      marker=list(color='lightgrey')) %>% 
            add_lines(x=~Date, y=~Daily_Cases_ma) %>% 
            layout(showlegend = FALSE)
        
        # make event lines
        tmp <- dat %>% dplyr::filter(!is.na(events))
        event_lines <- lapply(1:nrow(tmp), function(i) {
            list(type = 'line', 
                 line = list(color=event_cols[as.numeric(tmp$e_type[i])], 
                             dash='dash',  width=1),
                 opacity = 1, x0=tmp$Date[i], x1=tmp$Date[i], 
                 xref='x', y0=0.05, y1=1, yref='paper')
        })
        
        p %>% 
            add_trace(type = 'bar', x=~Date, y=~dummy,
                      marker = list(color='yellow', width=16),
                      data = tmp %>% mutate(dummy = max(dat$Daily_Cases)),
                      opacity=0) %>%
            layout(shapes = event_lines, 
                   xaxis = list(title="")) %>% 
            event_register("plotly_click")
        # onRender("
            #         function(el) { 
            #             el.on('plotly_click', function(d) { 
            #                 console.log('Click: ', d);
            #             });
            #         }
            #         ")

    })
    
    output$click <- renderPrint({
        d <- event_data("plotly_click")
        if (is.null(d)) 
            return("Click on a event line to see the text")

        if (d$curveNumber[1] == 2) {
            x <- d$x[1]
            tmp <- subset(dat, Date == x)
            tmp$e_text[1]
        } 
    })
    
    output$map1 <- renderImage({
        d <- event_data("plotly_click")
        if (is.null(d)) 
            return(list(src=""))
        if (d$curveNumber[1] == 2) {
            outfile <- tempfile(fileext='.jpg')
            download.file("https://picsum.photos/300/200.jpg", 
                          destfile=outfile, quiet=TRUE, mode="wb")
            list(src = outfile, width = 300,height = 200)
        } else {
            return(list(src=""))
        }
    }, deleteFile=TRUE)

    output$map2 <- renderImage({
        d <- event_data("plotly_click")
        if (is.null(d)) 
            return(list(src=""))
        if (d$curveNumber[1] == 2) {
            outfile <- tempfile(fileext='.jpg')
            download.file("https://picsum.photos/300/200.jpg", 
                          destfile=outfile, quiet=TRUE, mode="wb")
            list(src = outfile, width = 300,height = 200)
        } else {
            return(list(src=""))
        }
    }, deleteFile=TRUE)
    
})
