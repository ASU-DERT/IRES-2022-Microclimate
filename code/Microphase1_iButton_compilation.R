# The script compiles the raw iButton data files used for Microphase1
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



#--iButton data for microphase 1-----------

df_microphase1=read.csv(here("data", "microphase1.csv"), header=TRUE)


# list all files to read in
( allfiles = list.files(path = here("data", "Microphase1.iButtons"), # get a list of all files in folder
                        pattern = ".csv", # only files that end in .csv
                        full.names = TRUE,  # return the complete file path (and not just the file name)
                        recursive = TRUE) ) # include the child folders in the directory

# read in one file and extract identifying information from the file name
#( test = read.csv(allfiles[1], #read in the first file listed in "allfiles"
#                  skip = 15, # skip the first five lines (they are extraneous header info)
#                  header = FALSE, # there are no column headers
#                  col.names = c("date", "time", "unit", "temperature") ) ) #add column headers

#( allnames = str_split( allfiles[1], pattern = "/", simplify = TRUE) ) #create a matrix of strings for file info

#str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the block number from the file name
#test$condition = str_extract(allnames[, ncol(allnames)], pattern = "B|G") #extract the litter condition from the file name and add as a column
#test$species = str_extract(allnames[, ncol(allnames)], pattern = "SS|SP") #extract the species code from the file name and add as a column
#test$quadrat = str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the quadrat from the file name and add as a column

# function to do the above reading and extracting identifying information (so this can be done on all the files)
read_fun = function(path) {
  test = read.csv(path, 
                  skip = 15,
                  header = FALSE,
                  col.names = c("date", "time", "unit", "temperature") )
  allnames = str_split( path, pattern = "/", simplify = TRUE)
  test$condition = str_extract(allnames[, ncol(allnames)], pattern = "B|G") #extract the litter condition from the file name and add as a column
  test$species = str_extract(allnames[, ncol(allnames)], pattern = "SS|SP") #extract the species from the file name and add as a column
  test$quadrat = str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the quadrat from the file name and add as a column
  test
}

read_fun(allfiles[1]) # use to test the function on an individual file (this can be tested on any of them)

Microphase1_iButtons_df = map_dfr(allfiles, read_fun) # combine all the files
Microphase1_iButtons_df$loggertype <- "iButton" #add a column for the kind of logger used (iButton in this case)

# add a column that calculates the elapsed time since the experiment started

#create a column assigning rainfall to plot numbers
Microphase1_iButtons_df <- Microphase1_iButtons_df |>
  mutate(Rainfall = case_when(
    endsWith(quadrat, "Q1") ~ "0",
    endsWith(quadrat, "Q2") ~ "5",
    endsWith(quadrat, "Q3") ~ "10",
    endsWith(quadrat, "Q5") ~ "0",
    endsWith(quadrat, "Q7") ~ "0",
    endsWith(quadrat, "Q11") ~ "0",
    endsWith(quadrat, "Q13") ~ "0",
    endsWith(quadrat, "Q4") ~ "5",
    endsWith(quadrat, "Q8") ~ "5",
    endsWith(quadrat, "Q12") ~ "5",
    endsWith(quadrat, "Q14") ~ "5",
    endsWith(quadrat, "Q6") ~ "10",
    endsWith(quadrat, "Q9") ~ "10",
    endsWith(quadrat, "Q10") ~ "10",
    endsWith(quadrat, "Q15") ~ "10",
  ))
#average the temperatures per rainfall
#aggregate(Microphase1_iButtonsRain_df$temperature, by=list(Rainfall=Microphase1_iButtonsRain_df$Rainfall), FUN=mean)
Rain_TempAverage = Microphase1_iButtons_df |>
  group_by(Rainfall, Datetime, condition) |>
  summarise(
    AverageTemp = mean(temperature)
    ) |>
  mutate(Size = NA)

MicrophaseTemps = merge(Microphase1_iButtons_df,Rain_TempAverage,by="Datetime")

# Preliminary Plots -------------------------------------------------------

# preliminary plots with all data
# super messy plots of all the data
p <- ggplot(Microphase1_iButtons_df, aes(x=datetime, y=temperature, line=condition, color = condition)) + geom_line()
p + facet_grid(species ~ quadrat)

#plots using rainfall instead of quadrats
p <- ggplot(Microphase1_iButtonsRain_df, aes(x=datetime, y=temperature, line=condition, color = condition)) + geom_line()
p + facet_grid(species ~ Rainfall)

#plot for rainfall with averaged temps
p <- ggplot(MicrophaseTemps, aes(x=datetime, y=AverageTemp, line=condition.y, color = condition.y)) + geom_line()
  #annotate("rect", xmin = as.Date("2022-07-25 23:00:00"), xmax = as.Date("2022-07-26 06:00:00"), ymin = 0, ymax = 45, alpha = 0.2)
p + facet_grid(species ~ Rainfall)

#p <- ggplot(MicrophaseTemps, aes(x=datetime, y=AverageTemp, line=condition.y, color = condition.y)) + geom_smooth(span=0.00001)
#p + facet_grid(species ~ Rainfall)


#merge litter moisture data with ibutton data
iMoisture = merge(MicrophaseTemps, df_microphase1.means, by="datetime")

layered <- ggplot(iMoisture, aes(x=datetime, y=LitterMoisture_mean, color=Rainfall)) +
  geom_line(aes(linetype=Rainfall, color=Rainfall), size = 1) +
  geom_point() +
  geom_line(aes(y=AverageTemp/10)) +
  scale_y_continuous("LitterMoisture_mean", sec.axis = sec_axis(~.*10, name="Temperature (C)"))
