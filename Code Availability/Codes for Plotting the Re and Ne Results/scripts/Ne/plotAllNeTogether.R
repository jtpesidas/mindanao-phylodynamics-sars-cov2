library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(plotrix)
library(wesanderson)
library(zoo)
ne <- read.delim(file = "input/dailyCasesCount_allRegions.csv", sep = ',', header = TRUE) 
ne$date  <- as.Date(ne$date,format="%Y-%m-%d") #,format="%Y-%m-%d"

minimum <- min(ne$date)
maximum <- max(ne$date)

#### 7-day MA for the onset 
smoothedDataOnset <- ne %>% 
  mutate(roll_mean = rollmean(cases, 7, na.pad = T))

FINAL.PLOT <- smoothedDataOnset %>%
  ggplot(aes(date, roll_mean, fill = factor(Region))) 
  geom_col()+
  scale_x_date(labels = date_format("%b-%Y"), 
               limits = as.Date(c(minimum-1, maximum+1), format="%Y-%m-%d"),
               date_breaks = '1 month') +
  scale_color_manual(values=c("brown3", "goldenrod3", "darkolivegreen4", "deepskyblue3","darkblue", "indianred4"))+
  scale_fill_manual(values=c("brown3", "goldenrod3", "darkolivegreen4", "deepskyblue3","darkblue", "indianred4"))+
  theme_bw()+
  ylab("Cases")+ # ylab("Ne")+
  xlab("Date")+
  labs(title = "Reported Cases for All Regions in Mindanao")+ 
  theme(legend.position= c(0.13,0.85),
        legend.background = element_rect(fill = "white", color = "black"),
        legend.title = element_text("Regions"))
FINAL.PLOT                               

#### Saving the plot in the OUTPUT folder
ggsave (plot = FINAL.PLOT, 
        filename = 'Allcases.png', #filename = 'AllNe_legendInside.png',
        path = "output/",
        width = 3700, height = 4500, units = "px", dpi = 300)
