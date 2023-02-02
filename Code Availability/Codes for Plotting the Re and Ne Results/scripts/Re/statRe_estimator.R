#### Load libraries
library("tidyverse")
library("readxl")
library("lubridate")
library("data.table")
library("EpiEstim")
library("ggplot2")
library("incidence")
library("tidyverse")
library("zoo")
library("scales")

### Read argument
args = commandArgs(trailingOnly = TRUE)
asOf <- args[1]


### Read inputs
data <- read.delim(file = paste0("input/", list.files(path = "input/", pattern="^[^~].*csv$")), sep = ',', header = TRUE)
data$date  <- as.Date(data$Row.Labels, format="%d/%m/%Y")


### Calculate date range
minDate <- min(data$date, na.rm = TRUE)
maxDate <- max(data$date, na.rm = TRUE)


### Calculate I
data.case <- data.frame(data)
colnames(data.case) <- c("Row.Labels", "I", "dates")
data.case$dates = as.Date(data.case$dates)
data.case <- data.case %>%
  complete(dates = seq.Date(as.Date(minDate, format="%Y-%m-%d"), as.Date(maxDate, format="%Y-%m-%d"), by = "day"))
data.case <- subset(data.case, select=-Row.Labels)
data.case[is.na(data.case)] <- 0


### Apply modeling
data.case$dates <- as.Date(data.case$dates)
window <- 7
t_start <- 2:(length(data.case$I) - window + 1)
t_end <- t_start + window - 1
res_parametric_si <- estimate_R(data.case,
                                method ="parametric_si",
                                config = make_config(list(t_start = t_start, t_end = t_end,
                                                          mean_si = 4.8, std_si = 2.3, mean_prior = 2.6, std_prior= 2))) 
plot(res_parametric_si, what='R')

### Make situational report table
table <- res_parametric_si$R
table$t_start <- NULL
names(table)[names(table) == 't_end'] <- 'Date'
dates <- rev(seq.Date(as.Date(minDate), by = "1 day", length.out = nrow(table))) # as.Date(asOf) #by = "-1 day"
table$Date <- format(as.Date(dates), "%d-%b-%y")
rownames(table) <- NULL


### Save table
write.csv(table, file = paste0("output/ZAMBOANGA/ZAMBOANGA_untilAug28.csv"), row.names = FALSE)