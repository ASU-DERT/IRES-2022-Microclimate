setwd("/Users/Siena/Desktop/microclimate")

library(tidyverse)

#import dataset
weather_phase1 =read.csv("Gobabebmet_phase1.csv",header=TRUE)

# Value used to transform the data
coeff <- 100

weather_phase1$Datetime <- as.POSIXct(weather_phase1$Datetime, format = "%m/%d/%y %H:%M")

p1 <- ggplot(weather_phase1, aes(x=Datetime)) +
  geom_bar(aes(y=Fog), stat = "identity", fill='#54b8e3') +
  geom_line(aes(y=Humidity / coeff), color = "#1b98e0") +
  geom_area(aes(y=Humidity / coeff), fill = "#1b98e0", alpha=0.2) +
  ylim(0,1) +
  scale_x_datetime(labels=format("%H:%M"), expand=c(0,0)) +
  xlim(c(as.POSIXct('07/24/22 11:00', format = "%m/%d/%y %H:%M"),
        as.POSIXct('07/26/22 7:00', format = "%m/%d/%y %H:%M"))) +
  scale_y_continuous(name = "Fog (mm)", sec.axis = sec_axis(~.*coeff, name="Humidity (%)"),expand=c(0,0)) +
  xlab("Time") + 
  ylab("Fog (mm)") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank())
p1
p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

