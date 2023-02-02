#### Libraries
library(ggplot2)
#library(dplyr)
library(scales)
#library(patchwork)
#library(plotrix)
library(tidyverse)
library(zoo)

#### Read Input
data <- data.frame(read.delim(file = "input/ZAMBOANGA_Ne+onset.csv", sep = ',', header = TRUE))
data$date  <- as.Date(data$date,format="%d/%m/%Y")

#### 7-day MA for the onset 
smoothedData <- data %>% 
  mutate(roll_mean = rollmean(onset, 7, na.pad = T))

#### setting the date limits + coefficient to scale the axes to
minimum <- min(data$date)
maximum <- max(data$date)
coeff <-30#300

final.plot <- smoothedData %>%   
  ggplot(aes(date, median))+
  geom_line(size=2.5, aes(colour="a"))+
  geom_line(aes(y=roll_mean/coeff, colour="b"), size=1)+
  geom_ribbon(aes(ymin=lower, ymax=upper, x=date), alpha = 0.1, fill="darkblue")+
  geom_col(aes(y=onset/coeff), alpha=0.2) +
  scale_x_date(labels = date_format("%b-%Y"), 
               limits = as.Date(c(minimum-1, maximum+1), format="%Y-%m-%d"),
               date_breaks = '1 month') +
  scale_y_continuous(name = "Ne", sec.axis = sec_axis(~.*coeff, name="Reported Cases"))+
  theme_bw()+
  scale_color_manual(name=NULL, labels=c('Median Ne', 'Cases (7-day moving average)'), values=c('a'='darkblue','b'='red'))+
  theme(legend.position = c(0.17, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))+
  ylab("Re")+
  xlab("Date")+
  labs(title = "Effective Population Size (Ne) and Reported Cases of Zamboanga Peninsula") ######### region name :)


final.plot

#### Saving the plot in the OUTPUT folder
ggsave (plot = final.plot, 
        filename = 'ZAMBOANGA_Ne+onset.png',     ### REGION NAME :)
        path = "output/ZAMBOANGA/Ne",
        width = 2943, height = 1848, units = "px", dpi = 300)





