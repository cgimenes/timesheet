# install.packages("jsonlite")
# install.packages("tidyverse")
# install.packages("data.table")

library(jsonlite)
library(ggplot2)
library(dplyr)
library(lubridate)
library(data.table)
dates <- fromJSON("dates2.json", flatten = TRUE)
#sorted <- dates[order(dates$date),]
#df <- data.frame("date" = strptime(sorted$date,"%Y-%m-%d"), "first" = strptime(sorted$punch0,"%H:%M"))
#b <- ggplot(df, aes(x = date, y = first))
#b + geom_point() + geom_smooth(method = "loess")

dates$data <- as.Date(dates$date)
dates %>% 
  mutate(hour = as.factor(hour(hm(punch0))),
         monthYear = as.factor(format(data, "%Y-%m"))) %>% 
  select(monthYear,hour)  %>% 
  count(monthYear,hour) %>% 
  arrange(monthYear,hour) %>% 
  ggplot(aes(monthYear,hour,fill=n)) +
  geom_tile()

plot <- dates %>% 
  mutate(dt = as.ITime(strptime(punch0,"%H:%M")),
         monthYear = as.factor(format(data, "%Y-%m"))) %>% 
  group_by(monthYear) %>% 
  summarise(media = mean(dt),
            std= sd(dt)) %>% 
  arrange(monthYear)
plot$media = as.numeric(plot$media)
nums <- seq(from = mean(plot$media) - 21000 , to =  mean(plot$media) +21000, length.out = 9)
labels <- as.POSIXct(nums, origin = "1970-01-01", tz = "GMT") %>%
  format("%H:%M")


ggplot(plot, aes(x=monthYear, y=media))+
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=media-std, ymax=media+std),
                width=.2,position=position_dodge(.9)) +
  xlab('Mes-Ano') + 
  ylab('Hora') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylim(c(min(nums), max(nums)))+
  scale_y_continuous(breaks = nums,labels = labels)

# http://www.sthda.com/english/articles/32-r-graphics-essentials/131-plot-two-continuous-variables-scatter-graph-and-alternatives/