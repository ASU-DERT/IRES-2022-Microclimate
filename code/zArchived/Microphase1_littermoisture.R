
# Oct 3, 2022 HT
# 2025-07-02 update file locations - HT

#### CURENTLY TRYING TO FIGURE OUT WHAT IS HERE -- IN PROGRESS

## Set Up 

library(here) # v. 0.1
library(stringr) # v. 1.2.0
library(purrr) # v. 0.2.3
library(tidyverse)
library(lubridate)

#import dataset for the microphase 1 litter moisture data
df_microphase1=read.csv(here("data", "microphase1.csv"), header=TRUE)

# Clean up the data
# remove interval 13 - these were extras (alternative would be to treat them as interval 12 - they were collected then)
df_microphase1<-df_microphase1[!(df_microphase1$Interval > 12),]
# treat rainfall as a factor (categorical variable)
df_microphase1$Rainfall <- factor(df_microphase1$Rainfall) 
# change time from 12 h to 24 h time
#df_microphase1$Time <- format(as.POSIXct(df_microphase1$Time,format='%I:%M:%S %p'),format="%H:%M:%S")

# create new calculated variables from litter wet and dry masses
df_microphase1 <- df_microphase1 |>
  mutate(
    LitterWetMass = Sample_mass - SandMasswBag, # sample wet mass at collection
    LitterDryMass = Drymasswbag - D_bagMasswSand, # sample dry mass at collection
    # calculate sample dry mass (percent by mass) at collection
    LitterMoisturePct = ((LitterWetMass - LitterDryMass) / LitterDryMass)*100
  )

# divide into separate files for the two litter species
df_microphase1.SAPE <- df_microphase1 |>
  filter(Species == "SAPE")
df_microphase1.STSA <- df_microphase1 |>
  filter(Species == "STSA")


# line graph of litter moisture over time by species and litter condition ---------------------------------------------------

# calculate mean values for line graphs 
df_microphase1.means <- df_microphase1 |> 
  group_by(Littercond, Rainfall, Datetime, Species) |> 
  summarise(LitterMoisture_mean=mean(LitterMoisture, na.rm=TRUE), 
            n=n(), sd=sd(LitterMoisture, na.rm=TRUE), se=sd/sqrt(n))

#df_microphase1.means$se<-as.numeric(df_microphase1.means$se)
#df_microphase1.means$LitterMoisture_mean<-as.numeric(df_microphase1.means$LitterMoisture_mean)
#df_microphase1.means

df_microphase1.means$Rainfall <-as.factor(df_microphase1.means$Rainfall)

# line graph of litter moisture over time by condition and species 
p1 <- df_microphase1.means |>
  ggplot(aes(x=Datetime, y=LitterMoisture_mean, color=Rainfall)) + 
    geom_errorbar(aes(ymin=LitterMoisture_mean-se, ymax=LitterMoisture_mean+se), width=.1) +
    geom_line(aes(linetype=Rainfall, color=Rainfall), size = 1) +
    geom_point() +
    xlab("Collection Time") + 
    ylab("Litter Moisture (% by mass)") +
    theme_bw() +
    theme(text = element_text(size = 20)) +
    theme(panel.grid.major = element_blank()) +
    theme(panel.grid.minor = element_blank()) +
    facet_grid(Species~Littercond) 
p1




