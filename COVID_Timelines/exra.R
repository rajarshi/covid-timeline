library(plotly)
library(zoo)
library(RColorBrewer)
library(htmlwidgets)
source(data.R)



p <- dat %>% 
  plot_ly() %>% 
  add_trace(type='bar',  x=~Date, y=~Daily_Cases, 
           text = paste(dat$Daily_Cases, "cases<br><i>", dat$Daily_Cases_cum, "cumulative cases</i>" ),
           hoverinfo = "text",
           marker=list(color='lightgrey')) %>% 
  add_lines(x=~Date, y=~Daily_Cases_ma) %>% 
  layout(showlegend = FALSE)
  
# make event lines
tmp <- dat %>% dplyr::filter(!is.na(events)) %>% mutate(dummy = max(dat$Daily_Cases))
event_lines <- lapply(1:nrow(tmp), function(i) {
  list(type = 'line', 
       line = list(color=event_cols[as.numeric(tmp$events[i])], dash='dash', width=1),
       opacity = 1, x0=tmp$Date[i], x1=tmp$Date[i], 
       xref='x', y0=0.05, y1=1, yref='paper')
})

p %>% 
  add_trace(type = 'bar', x=~Date, y=~dummy, 
            marker = list(color='yellow', width=16),
            data = tmp, opacity=0) %>% 
  layout(shapes = event_lines) %>% 
  event_register("plotly_click")


download.file("https://picsum.photos/300/200.jpg", "./map1.jpg", mode="wb")

