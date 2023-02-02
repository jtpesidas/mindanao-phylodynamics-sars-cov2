#### plotting stat Re and the BDSKY Re
compare_Re <- data.frame(read.delim(file = "output/CARAGA/CARAGA_untilAug28.csv", sep = ',', header = TRUE))
compare_Re$Date  <- as.Date(compare_Re$date, format="%d/%m/%Y")

minDate <- min(compare_Re$Date)
maxDate <- max(compare_Re$Date)
aug <- maxDate -101

coeff <- 500

final.plot <- compare_Re %>%   
  ggplot(aes(Date))+
  geom_line(size=2.5, aes(Date,statRe,colour="a"))+
  geom_line(aes(y=BDSKY_Re, colour="b"), size=1)+
  geom_hline(yintercept=1, linetype="dashed", color="dimgray")+
  geom_ribbon(aes(ymin=lower, ymax=upper, x=Date), alpha = 0.1, fill="darkblue")+  # optional
  geom_col(aes(y=onset/coeff), alpha=0.1) +                                        # optional
  scale_y_continuous(name = "Re", sec.axis = sec_axis(~.*coeff, name="Reported Cases"))+ # optional
  scale_x_date(labels = date_format("%b-%Y"), 
               limits = as.Date(c(minDate-1, maxDate+1),format="%Y-%m-%d"), 
               date_breaks = '1 month') +
  theme_bw()+
  scale_color_manual(name=NULL, labels=c('Statistical Re', 'BDSKY Re'), values=c('a'='darkblue','b'='red'))+
  theme(legend.position = c(0.1, 0.9),
        legend.background = element_rect(fill = "white", color = "black"))+
  ylab("Re")+
  xlab("Date")+
  labs(title = "Effective Reproduction Number (Re) of CARAGA")

final.plot

#### Saving the plot in the OUTPUT folder
ggsave (plot = final.plot, 
        filename = '[clean] CARAGA_statRe_untilAug28.png',     ### REGION NAME 
        path = "output/CARAGA",
        width = 2943, height = 1848, units = "px", dpi = 300)