# Filename: OpenData-Offenses-All.r
# Author: @russl_corey
# Date: oct 10, 2022
# 
# This program is free software: you can redistribute it and/or modify it under 
# the terms of the GNU General Public License as published by the Free Software 
# Foundation, either version 3 of the License, or (at your option) any later 
# version.
# 
# This program is distributed in the hope that it will be useful, but WITHOUT ANY 
# WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A 
# PARTICULAR PURPOSE. See the GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License along with 
# this program. If not, see <https://www.gnu.org/licenses/>. 

# milestone 1 goal: import and run summary statistics

library(readr)
library(dplyr)
library(lubridate)

# Milestone 1 goal: load raw data set

# Define date set url for citation
offenses_url <- "source: https://www.oregon.gov/osp/Pages/Uniform-Crime-Reporting-Data.aspx"

# Set the working directory this script is located
setwd("/home/russell/Dropbox/DataAnalysis/Oregon_Crime_Report")

# Load in arrest data, look for local data set, if not download from source
if(!file.exists("data/OpenData-Offenses-All.csv")){
  offenses <- read_csv("https://www.oregon.gov/osp/Docs/Open-Data/OpenData-Offenses-All.csv")
  write_csv(offenses, "data/OpenData-Offenses-All.csv")
}else{
  # Run this block to load locally saved data
  offenses <- read_csv("data/OpenData-Offenses-All.csv")
}


# Milestone 2 goal: validate loaded data

# Check that the need columns are present
for(col in c('IncidentDate', 'County', 'NIBRS Report Title', 'Distinct Offenses')){
  if(!(col %in% colnames(offenses))) stop(paste('Missing column:', col)) 
}

# Validate number of reports
if(length(offenses[,1]) < 1) stop('No records loaded')

# Validate number of counties for Oregon
if(length(unique(tolower(offenses$County))) != 36) stop('Wrong number of counties found for Oregon (not 36)') 


# Milestone 3 goal: Format data and others

# Format created formatted date column in each data frame
offenses$date <- as.Date(offenses$IncidentDate, format = "%m/%d/%Y", origin="1900-01-01")

# Sort by dates
offenses = offenses[order(offenses$date),]

# Create column of for lower case county name
offenses$county <- tolower(offenses$County)

# Milestone 4 goal: cleanup
rm(col)