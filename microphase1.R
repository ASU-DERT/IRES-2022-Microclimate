# Analysis of IRES Litter Microclimate Group data for microphase 1 experiment
# HThroop wrote script on August 2, 2022 
# HThroop made minor tweaks on Sept 20, 2022
setwd("/Users/Siena/Desktop/microclimate")

library(tidyverse)
library(scales)

my_colors <- c("#fa882a","#6e4424","#5dba2b","#166610")

#import dataset
df_microphase1=read.csv("microphase1.csv",header=TRUE)
# remove interval 13 - these were extras (alternative would be to treat them as interval 12 - they were collected then)
#df_microphase1<-df_microphase1[!(df_microphase1$Interval > 12),]
# treat rainfall as a factor (categorical variable)
df_microphase1$Rainfall <- factor(df_microphase1$Rainfall) 

#df_microphase1$Datetime <- as.POSIXct(df_microphase1$Datetime, format = "%m/%d/%y %H:%M")

# change time from 12 h to 24 h time
#df_microphase1$Time <- format(as.POSIXct(df_microphase1$Time,format='%I:%M:%S %p'),format="%H:%M:%S")

# create a new variable "datetime" that combines data and time
#df_microphase1$datetime <- as.POSIXct(paste(df_microphase1$Time, df_microphase1$Date), format="%m-%d-%y %H:%M:%S")


# calculate litter wet and dry masses
df_microphase1$LitterWetMass <- df_microphase1$Sample_mass - df_microphase1$SandMasswBag # sample wet mass at collection
df_microphase1$LitterDryMass <- df_microphase1$Drymasswbag - df_microphase1$D_bagMasswSand # sample dry mass at collection
# calculate sample dry mass (percent by mass) at collection
df_microphase1$LitterMoisture <- ((df_microphase1$LitterWetMass - df_microphase1$LitterDryMass) / df_microphase1$LitterDryMass) *100 
# divide into separate files for the two litter species
df_microphase1.SAPE <- filter(df_microphase1, Species == "SAPE")
df_microphase1.STSA <- filter(df_microphase1, Species == "STSA")

# make some line graphs ---------------------------------------------------
# ylim.prim <- c(0, 150)   # in this example, precipitation
# ylim.sec <- c(0, 50)    # in this example, temperature




# calculate mean values for line graphs 
df_microphase1.means <- df_microphase1 %>% 
  group_by(Cond_spec, Rainfall, Datetime) %>% 
  summarise(LitterMoisture_mean=mean(LitterMoisture, na.rm=TRUE), n=n(), sd=sd(LitterMoisture, na.rm=TRUE), se=sd/sqrt(n))
df_microphase1.means$se<-as.numeric(df_microphase1.means$se)
df_microphase1.means$LitterMoisture_mean<-as.numeric(df_microphase1.means$LitterMoisture_mean)
df_microphase1.means

# exclude values above and below the 95th and 5th percentile
#microphase1.means_quantiles <- quantile(df_microphase1.means$LitterMoisture_mean, c(0.05, 0.95))
#microphase1.means_quantiles
#data_subset <- df_microphase1.means[df_microphase1.means$LitterMoisture_mean > microphase1.means_quantiles[1] &   # Drop rows below/above percentiles
                         #df_microphase1.means$LitterMoisture_mean < microphase1.means_quantiles[2], ]

#df_microphase1.means$Cond_spec_num <- as.numeric(substr(df_microphase1.means$Cond_spec,2,))

p1 <- ggplot(df_microphase1.means, aes(x=Datetime, y=LitterMoisture_mean, color=Cond_spec)) + 
  aes(group=Cond_spec) +
  geom_errorbar(aes(ymin=LitterMoisture_mean-se, ymax=LitterMoisture_mean+se), width=0.1) +
  geom_line(aes(color=Cond_spec, shape=Cond_spec, linetype=Cond_spec)) +
  geom_point() +
  annotate('rect', xmin=7, xmax=8.5, ymin=-5, ymax=150, alpha=.5, fill='#54b8e3') +
  xlab("Collection Time") + 
  ylab("Litter Moisture (% by mass)") +
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0)) +
  facet_grid(~Rainfall) 
p1

p1 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



