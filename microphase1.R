# Analysis of IRES Litter Microclimate Group data for microphase 1 experiment
# HThroop wrote script on August 2, 2022 
# HThroop made minor tweaks on Sept 20, 2022
# SSmamia rewrote script Sept 25, 2023

#setwd("/Users/Siena/Desktop/microclimate") # will not allow others to run your code
# see https://www.tidyverse.org/blog/2017/12/workflow-vs-script/

library(tidyverse)
library(scales)
library(here)

my_colors <- c("#fa882a","#6e4424","#5dba2b","#166610")

#import dataset
df_microphase1=read.csv(here("microphase1.csv"),header=TRUE) #HT modified for "here"
df_microphase1$Rainfall <- factor(df_microphase1$Rainfall) 


# calculate litter wet and dry masses
df_microphase1$LitterWetMass <- df_microphase1$Sample_mass - df_microphase1$SandMasswBag # sample wet mass at collection
df_microphase1$LitterDryMass <- df_microphase1$Drymasswbag - df_microphase1$D_bagMasswSand # sample dry mass at collection
# calculate sample dry mass (percent by mass) at collection
df_microphase1$LitterMoisture <- ((df_microphase1$LitterWetMass - df_microphase1$LitterDryMass) / df_microphase1$LitterDryMass) *100 
# divide into separate files for the two litter species
df_microphase1.SAPE <- filter(df_microphase1, Species == "SAPE")
df_microphase1.STSA <- filter(df_microphase1, Species == "STSA")


# calculate mean values for line graphs 
df_microphase1.means <- df_microphase1 %>% 
  group_by(Cond_spec, Rainfall, Datetime) %>% 
  summarise(LitterMoisture_mean=mean(LitterMoisture, na.rm=TRUE), n=n(), sd=sd(LitterMoisture, na.rm=TRUE), se=sd/sqrt(n))
df_microphase1.means$se<-as.numeric(df_microphase1.means$se)
df_microphase1.means$LitterMoisture_mean<-as.numeric(df_microphase1.means$LitterMoisture_mean)
df_microphase1.means


p1 <- ggplot(df_microphase1.means, aes(x=Datetime, y=LitterMoisture_mean, color=Cond_spec)) + 
  aes(group=Cond_spec) +
  geom_errorbar(aes(ymin=LitterMoisture_mean-se, ymax=LitterMoisture_mean+se), width=0.1) +
  geom_line(aes(color=Cond_spec, linetype=Cond_spec)) +
  geom_point(aes(shape=Cond_spec)) +
  annotate('rect', xmin=7, xmax=8.5, ymin=-5, ymax=150, alpha=.5, fill='#54b8e3') +
  xlab("Collection Time") + 
  ylab("Litter Moisture (% by mass)") +
  scale_color_manual(values = my_colors) +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  scale_y_continuous(expand=c(0,0)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  facet_grid(~Rainfall) 
p1




