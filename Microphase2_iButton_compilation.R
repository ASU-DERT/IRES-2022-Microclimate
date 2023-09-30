setwd("/Users/Siena/Desktop/microclimate")

library(here) # v. 0.1
library(stringr) # v. 1.2.0
library(purrr) # v. 0.2.3
library(tidyverse)
library(lubridate)
library(dplyr)

# list all files to read in
( allfiles = list.files(path = here("Microphase2.iButtons"), # get a list of all files in folder
                        pattern = ".csv", # only files that end in .csv
                        full.names = TRUE,  # return the complete file path (and not just the file name)
                        recursive = TRUE) ) # include the child folders in the directory

# read in one file and extract identifying information from the file name
# note that identifying info can also be extracted from file path if needed
( test = read.csv(allfiles[1], #read in the first file listed in "allfiles"
                  header = TRUE,
                  col.names = c("Date", "Time", "Temperature") ) ) #add column headers

( allnames = str_split( allfiles[1], pattern = "/", simplify = TRUE) ) #create a matrix of strings for file info

str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the block number from the file name
test$depth = str_extract(allnames[, ncol(allnames)], pattern = "D[0-3]") #extract the litter depth from the file name and add as a column
test$quadrat = str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the quadrat from the file name and add as a column

# function to do the above reading and extracting identifying information (so this can be done on all the files)
read_fun = function(path) {
  test = read.csv(path,
                  header = TRUE,
                  col.names = c("Date", "Time", "Temperature") )
  allnames = str_split( path, pattern = "/", simplify = TRUE)
  test$depth = str_extract(allnames[, ncol(allnames)], pattern = "D[0-3]") #extract the litter depth from the file name and add as a column
  test$quadrat = str_extract(allnames[, ncol(allnames)], pattern = "Q[1-9]") #extract the quadrat from the file name and add as a column
  test
}

read_fun(allfiles[1]) # use to test the function on an individual file (this can be tested on any of them)

microphase2_iButtons_df = map_dfr(allfiles, read_fun) # combine all the files
microphase2_iButtons_df$loggertype <- "iButton" #add a column for the kind of logger used (iButton in this case)

# change time from 12 h to 24 h time
microphase2_iButtons_df$Time <- format(as.POSIXct(microphase2_iButtons_df$Time,format='%I:%M:%S %p'),format="%H:%M:%S")

write.csv(microphase2_iButtons_df,'Microphase2_ibuttons.csv',row.names=FALSE)
