#### Libraries
library(ggplot2)
library(scales)
library(tidyverse)
library(zoo)

#### Read Input
data <- data.frame(read.delim(file = "input/BARMM_Re+onset+deaths.csv", sep = ',', header = TRUE))
data$date  <- as.Date(data$date,format="%d/%m/%Y")

#### 7-day MA for the onset 
smoothedData <- data %>% 
  mutate(roll_mean = rollmean(onset, 7, na.pad = T))

#### setting the date limits + coefficient to scale the axes to
minimum <- min(data$date)
maximum <- max(data$date)
coeff <- 50

final.plot <- smoothedData %>%   
  ggplot(aes(date, median))+
  geom_line(size=2.5, aes(colour="a"))+
  geom_line(aes(y=roll_mean/coeff, colour="b"), size=1)+
  geom_hline(yintercept=1, linetype="dashed", color="dimgray")+
  geom_ribbon(aes(ymin=lower, ymax=upper, x=date), alpha = 0.1, fill="darkblue")+
  geom_col(aes(y=onset/coeff), alpha=0.1) +
  scale_x_date(labels = date_format("%b-%Y"), 
               limits = as.Date(c(minimum-1, maximum+1), format="%Y-%m-%d"),
               date_breaks = '1 month') +
  scale_y_continuous(name = "Re", sec.axis = sec_axis(~.*coeff, name="Reported Cases"))+
  theme_bw()+
  scale_color_manual(name=NULL, labels=c('Median Re', 'Cases (7-day moving average)'), values=c('a'='darkblue','b'='red'))+
  theme(legend.position = c(0.17, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))+
  ylab("Re")+
  xlab("Date")+
  labs(title = "Effective Reproduction Number (Re) and Reported Cases of BARMM") ######### region name :)


final.plot
#### Saving the plot in the OUTPUT folder
ggsave (plot = final.plot, 
          filename = 'BARMM_Re+onset.png',     ### REGION NAME :)
          path = "output/BARMM",
          width = 2943, height = 1848, units = "px", dpi = 300)






















#### first try on plotting
# final.plot <- ggplot(barmm,aes(x=date)) +
#   #geom_line() +
#   geom_point(y=median, size=5,alpha=0.1)+
#   geom_line(aes(y=median, color="darkblue"), size = 2, alpha = 0.7)+
#   geom_ribbon(aes(ymin=lower, ymax=upper, x=date, fill = "95% HPD"), alpha = 0.5)+
#   scale_colour_manual("",values="blue")+
#   scale_fill_manual("",values="lightblue")+
#   geom_col(aes(y=onset/coeff)) +
#   geom_smooth(aes(y=onset/coeff),method = "gam", se=FALSE, col="red")+  theme_bw() +
#   scale_x_date(labels = date_format("%b-%Y"), 
#                limits = as.Date(c(minimum, maximum+1), format="%Y-%m-%d"),
#                date_breaks = '1 month') +
#   scale_y_continuous(name = "Re", sec.axis = sec_axis(~.*coeff, name="Reported Cases"))+
#   theme(axis.title.y.left = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)),
#         axis.title.y.right = element_text(margin = margin(t = 0, r = 0, b = 0, l = 10)),
#         axis.title.x = element_text(margin = margin(t = 10, r = 0, b = 0, l = 0))) + 
#   #ylim(0,400) + 
#   labs(x = "Month and Year",
#        y = "Reported Cases",
#        title = paste0("Reported cases in BARMM"))#+
#   #legend(1)#, 95, legend=c("Line 1", "Line 2"),
#          #col=c("red", "blue"), lty=1:2, cex=0.8)
