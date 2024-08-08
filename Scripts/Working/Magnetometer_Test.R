## ---------------------------
##
## Script name: Magnetomter Test
##
## Author: Rhemi Toth
##
## Date Created: 2024-08-05
##
## Email: rhemitoth@g.harvard.edu
##
## ---------------------------
##
## Notes: This script generates dead-reckoned trajetories two ways:
##   1) Using heading derived from magnetometer data
##   2) Estimating position from the second derivative of acceleration
##
## The two methods are compared to see if dead-reckoning can be achieved without yaw (from magnetometer)
## ---------------------------


# Clear Environment -------------------------------------------------------

rm(list = ls())

# Load Packages -----------------------------------------------------------

library(zoo)
library(tidyverse)
library(doParallel)

# Load Data ---------------------------------------------------------------

df <- read_csv("Data/Gundog_Tutorial/activity_data.csv")

source("Scripts/Working/Gundog_Tracks_Tutorial/Gundog.Compass.R") #Change as appropriate


# Format Data for Analyses ------------------------------------------------

# Timestamp column
df$timestamp <- as.POSIXct(strptime(paste(df$Date, df$Time.hh.mm.ss.ddd), format = "%d/%m/%Y %H:%M:%OS", tz = "GMT"))

# Keep only the first record from each H:M:S timestamp

# Marked event column (for callibration period)
df$Marked.event <- ifelse(df$Marked.event == 2, "M", df$Marked.event)

# rowID column
df$rowID <- 1:nrow(df)

# elapsed time
df$elapsed_time <- NA
for(i in 2:nrow(df)){
  t1 <- df$timestamp[i-1]
  t2 <- df$timestamp[i]
  df$elapsed_time[i] <- difftime(t1,t2,units = "secs")
}

# Functions ---------------------------------------------------------------

pitch_rotation_matrix <- function(pitch){
  
  # create matrix
  mat <- matrix(data = c(1,0,0,0,cos(pitch),-sin(pitch),0,sin(pitch),cos(pitch)),
                nrow = 3,
                ncol = 3,
                byrow = TRUE)
  
  # return result
  return(mat)
}

roll_rotation_matrix <- function(roll){
  
  # create matrix
  mat <- matrix(data = c(cos(roll),0,sin(roll),0,1,0,-sin(roll),0,cos(roll)),
                nrow = 3,
                ncol = 3,
                byrow = TRUE)
  
  # return result
  return(mat)
}
# Dead Reckoning with Magnetometer ----------------------------------------


# ** Step 1: Compute Acceleration Components ----------------------------------

# Static Acceleration

w <- 40 # moving window width
df$Gx <- rollapply(df$Acc_x, width = w, FUN = mean, align = "center", fill = "extend")
df$Gy <- rollapply(df$Acc_y, width = w, FUN = mean, align = "center", fill = "extend")
df$Gz <- rollapply(df$Acc_z, width = w, FUN = mean, align = "center", fill = "extend")

#  Dynamic Acceleration

df$DA_x <- df$Acc_x - Gx
df$DA_y <- df$Acc_y - Gy
df$DA_z <- df$Acc_z - Gz

#  VeDBA

df$VeDBA <- sqrt((df$DA_x^2) + (df$DA_y^2) + (df$DA_z^2))


# ** Step 2: Compute Pitch and Roll ---------------------------------------

df$pitch <- atan2(df$Gy, sqrt(df$Gx*df$Gx+df$Gz*df$Gz))*180/pi
df$roll <- atan2(df$Gx,sqrt(df$Gy*df$Gy+df$Gz*df$Gz))*180/pi


# ** Step 3: Perform hard and soft iron corrections -----------------------

# Filter for magnetic calibration events

df_cal <- df %>%
  filter(Marked.event == "M")

# Calculate offsets
Ox <- (max(df_cal$Mag_x) - min(df_cal$Mag_x))/2
Oy <- (max(df_cal$Mag_y) - min(df_cal$Mag_y))/2
Oz <- (max(df_cal$Mag_z) - min(df_cal$Mag_z))/2

# Correct Magnetometer Output
df$Mx <- df$Mag_x - Ox
df$My <- df$Mag_y - Oy
df$Mz <- df$Mag_z - Oz


# ** Step 4: Normalize Compass Data ---------------------------------------

fm <- sqrt(df$Mx^2 + df$My^2 + df$Mz^2) # normalizing factor
df$NMx <- df$Mx/fm
df$NMy <- df$My/fm
df$NMz <- df$Mz/fm


# ** Steps 5 - 7 --------------------
# Step 5: Rotating axes according to pitch and roll
# Step 6: Calculating Speed from VeDBA
# Step 7: Initial dead reckoning calculation

# Rotation matrix for pitch
Rx <- lapply(pitch,pitch_rotation_matrix)

# Rotation matrix for roll
Ry <- lapply(pitch,roll_rotation_matrix)

# Function to calculate magnetometer output if device were level and derive the heading
dead_reckoning <- function(df, pitch_rotation_matrices, roll_rotation_matrices){
  
  # Index 
  dat <- df %>%
    filter(rowID == i)
  NM <- matrix(data = c(df$NMx[i], df$NMy[i], df$NMz[i]),
               nrow = 1,
               ncol = 3,
               byrow = TRUE)
  Rx <- Rx_list[[i]]
  Ry <- Ry_list[[i]]
  
  # Correct Magnetometer Output
  RNM <- NM %*% Rx %*% Ry
  
  # Derive heading
  H <- atan2(RNM[2], -RNM[1])*180/pi
  
  # Calculate speed from VeDBA
  m <- 1
  c <- 0.1
  s <- (m*df$VeDBA) + c
  
  # Calculate distance traveled
  d <- s*df$elapsed_time
  
  # Speed coefficient
  q <- d/6.371e6
  
  # Latitude
  lat <- asin()
  
  # Return result
  return(H)
}

# Set up cluster
cl <- makeCluster(2)
registerDoParallel(cl)
start_time <- Sys.time()
num_iters <- length(NMx)

# Calculate headings in parallel
headings <- foreach(i = 2:num_iters, .combine = "rbind") %dopar% get_heading(NMx_list = NMx, 
                                                                               NMy_list = NMy,
                                                                               Rx_list = Rx,
                                                                               Ry_list = Ry)
end_time <- Sys.time()
stopCluster(cl)
time.taken <- round(end_time - start_time,2)
print(time.taken)


# ** Step 7: Initial calculation of speed from VeDBA -----------------------------------

m <- 1 # constant of proportionality
c <- 0.1 # constant



