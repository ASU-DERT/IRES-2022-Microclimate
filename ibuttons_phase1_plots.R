# Analysis of IRES Litter Microclimate Group data for microphase 1 experiment
# HThroop wrote script on August 2, 2022 
# HThroop made minor tweaks on Sept 20, 2022
# SSmania adjusted code to be used for iButton data in place of litter moisture Sep 27, 2023
setwd("/Users/Siena/Desktop/microclimate")

library(tidyverse)
library(ggplot2)
library(scales)
library(dplyr)
library(data.table)
library(lubridate)

my_colors <- c("#fa882a","#6e4424","#5dba2b","#166610")

#read csv with manually made datetime column
df_Temps1=read.csv('Microphase1_iButtons_ALL_dt.csv')


#add rainfall based on quadrat
df_Temps1 <- df_Temps1 %>% 
  mutate(Rainfall = case_when(
    endsWith(quadrat, '1') ~ 0,
    endsWith(quadrat, '5') ~ 0,
    endsWith(quadrat, '7') ~ 0,
    endsWith(quadrat, '11') ~ 0,
    endsWith(quadrat, '13') ~ 0,
    endsWith(quadrat, '2') ~ 5,
    endsWith(quadrat, '4') ~ 5,
    endsWith(quadrat, '8') ~ 5,
    endsWith(quadrat, '12') ~ 5,
    endsWith(quadrat, '14') ~ 5,
    endsWith(quadrat, '3') ~ 10,
    endsWith(quadrat, '6') ~ 10,
    endsWith(quadrat, '9') ~ 10,
    endsWith(quadrat, '10') ~ 10,
    endsWith(quadrat, '15') ~ 10
  ))

df_Temps1$datetime <- as.POSIXct(df_Temps1$datetime, format = "%m/%d/%y %H:%M")
df_Temps1 <- df_Temps1 %>% 
  mutate(interval = cut(datetime, breaks = "60 min"))

# calculate mean values for line graphs 
df_Temps1.means <- df_Temps1 %>% 
  group_by(Type, Rainfall, interval) %>% 
  summarise(Temp_mean=mean(temperature, na.rm=TRUE), n=n(), sd=sd(temperature, na.rm=TRUE), se=sd/sqrt(n))
df_Temps1.means$se<-as.numeric(df_Temps1.means$se)
df_Temps1.means$Temp_mean<-as.numeric(df_Temps1.means$Temp_mean)
df_Temps1.means

p1 <- ggplot(df_Temps1.means, aes(x=interval, y=Temp_mean, color=Type)) + 
  aes(group=Type) +
  geom_errorbar(aes(ymin=Temp_mean-se, ymax=Temp_mean+se), width=0.1) +
  geom_line(aes(color=Type, linetype=Type)) +
  geom_point(size=0.5) +
  xlab("Time") + 
  ylab("Temperature (ºC)") +
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0)) +
  #scale_x_datetime(date_labels = "%m/%d/%y %H:%M",breaks = date_breaks("2 hours"))+
  #theme(axis.text.x = element_text(angle = 90, vjust = 1.0, hjust = 1.0)) +
  #scale_x_datetime(labels=format("%H:%M"), expand=c(0,0)) +
  #xlim(c(as.POSIXct('07/24/22 11:00', format = "%m/%d/%y %H:%M"),
         #as.POSIXct('07/26/22 7:00', format = "%m/%d/%y %H:%M"))) +
  facet_grid(~Rainfall) 
p1

