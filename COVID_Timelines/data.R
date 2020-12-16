library(dplyr)
library(RColorBrewer)
library(zoo)

set.seed(1234)

event_cols <- brewer.pal(n=5, 'Dark2')

dat <- read.csv("CTfromJune1.csv", header=TRUE) %>% 
  mutate(Date = mdy(Date), 
         Daily_Cases_ma = rollmean(Daily_Cases, k=3, na.pad=TRUE),
         Daily_Cases_cum = cumsum(Daily_Cases))

events <- sample(c(NA, 'EO-1', 'EO-2', 'EO-3'), size=nrow(dat), replace=TRUE,
                 prob=c(0.9, 0.005, 0.05, 0.05))
events <- data.frame(e_type = as.factor(events),
                     e_text = ifelse(is.na(events), "", sprintf("This is an event of type %s on %s",
                                      events, dat$Date)))
dat <- cbind(dat, events)
