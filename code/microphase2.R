# Analysis of IRES Litter Microclimate Group data for microphase 2 experiment
# HThroop wrote script on August 3, 2022 

setwd("/Users/Siena/Desktop/microclimate")
library(tidyverse)
library("viridis")
my_colors <- c("#e766f2","#6671f2","#f26d66")

#import dataset
df_microphase2=read.csv("microphase2.csv",header=TRUE)
df_microphase2$time<-(df_microphase2$Interval)*6
df_microphase2$Interval <- factor(df_microphase2$Interval) # make interval a categorical variable

# calculate litter wet and dry masses
df_microphase2$LitterWetMass <- df_microphase2$Sample_mass - df_microphase2$SandMasswBag - (df_microphase2$D_bagMasswSand-df_microphase2$Dryingbagmass) # sample wet mass at collection
df_microphase2$LitterDryMass <- df_microphase2$Drymasswbag - df_microphase2$D_bagMasswSand # sample dry mass at collection
df_microphase2$LitterMoisture <- ((df_microphase2$LitterWetMass - df_microphase2$LitterDryMass) / df_microphase2$LitterDryMass) *100 #sample dry mass at collection

df_microphase2 <- subset(df_microphase2,LitterMoisture > 0 )
df_microphase2 <- subset(df_microphase2,LitterMoisture < 100 )

Rainfall.labs <- c("0 mm", "5 mm", "10 mm") # New facet label names for dose variable
names(Rainfall.labs) <- c("0", "5", "10")



df_microphase2.means <- df_microphase2 %>% 
  group_by(DEPTH, Rainfall, Datetime) %>% 
  summarise(LitterMoisture_mean=mean(LitterMoisture, na.rm=TRUE), n=n(), sd=sd(LitterMoisture, na.rm=TRUE), se=sd/sqrt(n))
df_microphase2.means$se<-as.numeric(df_microphase2.means$se)
df_microphase2.means$LitterMoisture_mean<-as.numeric(df_microphase2.means$LitterMoisture_mean)
df_microphase2.means

df_microphase2.means$Rainfall <- as.factor(df_microphase2.means$Rainfall) # make target mass a categorical variable

p2 <- ggplot(df_microphase2.means, aes(x=Datetime, y=LitterMoisture_mean, colour=DEPTH)) +
  aes(group=DEPTH) +
  geom_errorbar(aes(ymin=LitterMoisture_mean-se, ymax=LitterMoisture_mean+se), width=0.1) +
  geom_line(aes(color=DEPTH)) +
  geom_point() +
  scale_color_viridis(option='C') +
  #scale_color_manual(values = my_colors) +
 # scale_size_manual(values=c(1, 1.5, 2))+
  xlab("Collection Time") + 
  ylab("Litter Moisture (% by mass)") +
  theme_bw() +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank()) +
  theme(panel.grid.minor = element_blank()) +
  theme(legend.position = c(0.8, 0.8)) +
  scale_y_continuous(expand=c(0,0)) +
  facet_grid(~Rainfall)
p2
#p2 +   scale_fill_discrete(labels = c("0 mm", "5 mm", "10 mm")) 
p2 + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.position="top")


write.csv(df_microphase2,'Microphase2_coded_littermoisture.csv')


