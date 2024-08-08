## ---------------------------
##
## Script name: Clean Tutorial Data
##
## Author: Rhemi Toth
##
## Date Created: 2024-08-07
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script thins the data set that comes with the Gundog Tracks Tutorial
##   
##
## ---------------------------


# Load Data ---------------------------------------------------------------

gundog_data <- read.delim("/Users/rhemitoth/Documents/PhD/Cembra/Dead_Reckoning/Data/Gundog_Tutorial/Test.Data.P10A_selection_split#1_0.txt")


# Thin data ---------------------------------------------------------------

# Assign a "keep" value to the data
gundog_data$keep <- 1
for(i in 2:nrow(gundog_data)){
  prev_time <- gundog_data$Time.hh.mm.ss[i-1]
  current_time <- gundog_data$Time.hh.mm.ss[i]
  gps <- gundog_data$GPS.fix.present[i]
  if(current_time == prev_time && gps == 0){
    gundog_data$keep[i] <- 0
  }
}

# Filter for rows where "keep" == 1

gundog_data_thin <- gundog_data %>%
  filter(keep == 1)

# Export the result
write.csv(gundog_data_thin, "/Users/rhemitoth/Documents/PhD/Cembra/Dead_Reckoning/Data/Gundog_Tutorial/activity_data.csv")
