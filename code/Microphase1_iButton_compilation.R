# The script compiles the raw iButton data files used for Microphase1
# Oct 3, 2022 HT
setwd("/Users/Siena/Desktop/microclimate")

library(here) # v. 0.1
library(stringr) # v. 1.2.0
library(purrr) # v. 0.2.3
library(tidyverse)
library(lubridate)
library(dplyr)

#import dataset
df_microphase1=read.csv("microphase1.csv",header=TRUE)
# remove interval 13 - these were extras (alternative would be to treat them as interval 12 - they were collected then)
df_microphase1<-df_microphase1[!(df_microphase1$Interval > 12),]
# treat rainfall as a factor (categorical variable)
df_microphase1$Rainfall <- factor(df_microphase1$Rainfall) 

# change time from 12 h to 24 h time
#df_microphase1$Time <- format(as.POSIXct(df_microphase1$Time,format='%I:%M:%S %p'),format="%H:%M:%S")

# create a new variable "datetime" that combines data and time
df_microphase1$datetime <- as.POSIXct(hms((df_microphase1$Time))+ mdy(df_microphase1$Date))

# calculate litter wet and dry masses
df_microphase1$LitterWetMass <- df_microphase1$Sample_mass - df_microphase1$SandMasswBag # sample wet mass at collection
df_microphase1$LitterDryMass <- df_microphase1$Drymasswbag - df_microphase1$D_bagMasswSand # sample dry mass at collection
# calculate sample dry mass (percent by mass) at collection
df_microphase1$LitterMoisture <- ((df_microphase1$LitterWetMass - df_microphase1$LitterDryMass) / df_microphase1$LitterDryMass) *100 
# divide into separate files for the two litter species
df_microphase1.SAPE <- filter(df_microphase1, Species == "SAPE")
df_microphase1.STSA <- filter(df_microphase1, Species == "STSA")

# make some line graphs ---------------------------------------------------

# calculate mean values for line graphs 
df_microphase1.means <- df_microphase1 %>% 
  group_by(Littercond, Rainfall, datetime, Species) %>% 
  summarise(LitterMoisture_mean=mean(LitterMoisture, na.rm=TRUE), n=n(), sd=sd(LitterMoisture, na.rm=TRUE), se=sd/sqrt(n))
df_microphase1.means$se<-as.numeric(df_microphase1.means$se)
df_microphase1.means$LitterMoisture_mean<-as.numeric(df_microphase1.means$LitterMoisture_mean)
df_microphase1.means

p1 <- ggplot(df_microphase1.means, aes(x=datetime, y=LitterMoisture_mean, color=Rainfall)) + 
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



#-----------------------------------------------------




# list all files to read in
( allfiles = list.files(path = here("Microphase1.iButtons"), # get a list of all files in folder
                        pattern = ".csv", # only files that end in .csv
                        full.names = TRUE,  # return the complete file path (and not just the file name)
                        recursive = TRUE) ) # include the child folders in the directory

# read in one file and extract identifying information from the file name
# note that identifying info can also be extracted from file path if needed
( test = read.csv(allfiles[1], #read in the first file listed in "allfiles"
                  skip = 15, # skip the first five lines (they are extraneous header info)
                  header = FALSE, # there are no column headers
                  col.names = c("date", "time", "unit", "temperature") ) ) #add column headers

( allnames = str_split( allfiles[1], pattern = "/", simplify = TRUE) ) #create a matrix of strings for file info

str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the block number from the file name
test$condition = str_extract(allnames[, ncol(allnames)], pattern = "B|G") #extract the litter condition from the file name and add as a column
test$species = str_extract(allnames[, ncol(allnames)], pattern = "SS|SP") #extract the species code from the file name and add as a column
test$quadrat = str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the quadrat from the file name and add as a column

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

# change time from 12 h to 24 h time
Microphase1_iButtons_df$time <- format(as.POSIXct(Microphase1_iButtons_df$time,format='%I:%M:%S %p'),format="%H:%M:%S")

# create a new variable "datetime" that combines data and time
Microphase1_iButtons_df$datetime <- as.POSIXct(hms((Microphase1_iButtons_df$time))+ mdy(Microphase1_iButtons_df$date))

#create a column assigning rainfall to plot numbers
Microphase1_iButtonsRain_df <- Microphase1_iButtons_df %>%
  mutate(Rainfall = case_when(
    endsWith(quadrat, "Q1") ~ "0mm",
    endsWith(quadrat, "Q2") ~ "5mm",
    endsWith(quadrat, "Q3") ~ "10mm",
    endsWith(quadrat, "Q5") ~ "0mm",
    endsWith(quadrat, "Q7") ~ "0mm",
    endsWith(quadrat, "Q11") ~ "0mm",
    endsWith(quadrat, "Q13") ~ "0mm",
    endsWith(quadrat, "Q4") ~ "5mm",
    endsWith(quadrat, "Q8") ~ "5mm",
    endsWith(quadrat, "Q12") ~ "5mm",
    endsWith(quadrat, "Q14") ~ "5mm",
    endsWith(quadrat, "Q6") ~ "10mm",
    endsWith(quadrat, "Q9") ~ "10mm",
    endsWith(quadrat, "Q10") ~ "10mm",
    endsWith(quadrat, "Q15") ~ "10mm",
  ))
#average the temperatures per rainfall
#aggregate(Microphase1_iButtonsRain_df$temperature, by=list(Rainfall=Microphase1_iButtonsRain_df$Rainfall), FUN=mean)
Rain_TempAverage = Microphase1_iButtonsRain_df %>%
  group_by(Rainfall, datetime, condition) %>%
  summarise(
    AverageTemp = mean(temperature)
    ) %>%
  mutate(Size = NA)

MicrophaseTemps = merge(Microphase1_iButtons_df,Rain_TempAverage,by="datetime")

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
