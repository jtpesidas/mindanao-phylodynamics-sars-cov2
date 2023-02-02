library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)
library(patchwork)
library(plotrix)
library(wesanderson)
re <- read.delim(file = "input/ReDatafile (1).csv", sep = ',', header = TRUE)
re$date  <- as.Date(re$date,format="%Y-%m-%d")

minimum <- min(re$date)
maximum <- max(re$date)
FINAL.PLOT <- re %>%
  ggplot(aes(date, median, colour = Region)) + #c('a','b','c','d','e','f')))+
  geom_point(size=5, alpha=0.1)+
  geom_line(size=2, lineend="round")+
  geom_hline(yintercept=1, linetype="dashed", color="dimgray")+
  scale_x_date(labels = date_format("%b-%Y"), 
               limits = as.Date(c(minimum-1, maximum+1), format="%Y-%m-%d"),
               date_breaks = '1 month') +
  scale_color_manual(values=c("brown3", "goldenrod3", "darkolivegreen4", "deepskyblue3","darkblue", "indianred4"))+
  theme_bw()+
  ylab("Re")+
  xlab("Date")+
  labs(title = "Effective Reproduction Number (Re) for All Regions in Mindanao")+
  theme(legend.position= c(0.15,0.8),
        legend.background = element_rect(fill = "white", color = "black"))

FINAL.PLOT                               

#### Saving the plot in the OUTPUT folder
ggsave (plot = FINAL.PLOT, 
        filename = 'AllRe_legendInside.png',
        path = "output/")#,
        #width = 3700, height = 4500, units = "px", dpi = 300)
