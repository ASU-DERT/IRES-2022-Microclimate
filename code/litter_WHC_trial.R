# Analysis of IRES Litter Microclimate Group data on litter water holding capacity
# Experiment started on July 14, 2022
# HThroop wrote script on July 15, 2022 

#import dataset
df_moisture=read.csv("220715_litter_moisture.csv",header=TRUE)

#retrieve the package ggplot2 from the library
library(ggplot2) 
library(tidyverse) 

#calculate derived moisture variables
df_moisture$MassT1noBag <- df_moisture$MassT1wBag - df_moisture$Bag_Mass #mass of litter at T1 (bag mass subtracted)
df_moisture$MassT2noBag <- df_moisture$MassT2wBag - df_moisture$Bag_Mass #mass of litter at T2 (bag mass subtracted)
df_moisture$T1pctMassLoss <- ((df_moisture$MassT0noBag - df_moisture$MassT1noBag)/df_moisture$MassT0noBag)*100 
  # T1pctMassLoss is the percent of the initial wet litter mass that is lost by time 1. Ultimately this can be replaced 
  # with a variable that is relative to dry mass
df_moisture$T2pctMassLoss <- (df_moisture$MassT0noBag - df_moisture$MassT2noBag)/df_moisture$MassT0noBag*100
  # T2pctMassLoss is the percent of the initial wet litter mass that is lost by time 1. Ultimately this can be replaced 
  # with a variable that is relative to dry mass

box_pctMassLoss <- ggplot(df_moisture, aes(x=TargetMass, y=T1pctMassLoss)) +
  geom_boxplot() +
  facet_grid(Species~LitterCond) +
  xlab("Litter Target Mass (g)") + 
  ylab("Mass Loss from T0 (%)") +
  ggtitle("Time 1")
box_pctMassLoss

box_pctMassLoss <- ggplot(df_moisture, aes(x=TargetMass, y=T2pctMassLoss)) +
  geom_boxplot() +
  facet_grid(Species~LitterCond) +
  xlab("Litter Target Mass (g)") + 
  ylab("Mass Loss from T0 (%)") +
  ggtitle("Time 2")
box_pctMassLoss