

library(tidyverse)
library(here)

#import dataset
weather_phase2 = read.csv(here("data", "gobabeb-auss-microphase2.csv"),header=TRUE)


my_colors <- c('#2ba3ff', '#06286b')

# Value used to transform the data
coeff <- 100

# get datetime in correct format
weather_phase2$Datetime <- as.POSIXct(weather_phase2$Datetime, format = "%m/%d/%y %H:%M")

p1 <- ggplot(weather_phase2, aes(x=Datetime, color=Station)) +
  aes(group=Station) +
  geom_bar(aes(y=Fog, fill=Station), stat = "identity") +
  scale_color_manual(values = my_colors) +
  geom_line(aes(y=Humidity / coeff, color=Station, linetype=Station), size=1.5) +
  ylim(0,1) +
  scale_x_datetime(labels=format("%H:%M"), expand=c(0,0)) +
  xlim(c(as.POSIXct('07/30/22 12:00', format = "%m/%d/%y %H:%M"),
         as.POSIXct('08/1/22 6:00', format = "%m/%d/%y %H:%M"))) +
  scale_y_continuous(name = "Fog (mm)", sec.axis = sec_axis(~.*coeff, name="Humidity (%)"),expand=c(0,0)) +
  xlab("Time") + 
  ylab("Fog (mm)") +
  theme_bw() +
  theme(text = element_text(size = 15)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
p1
