# Filename: Oregon_Census_Counties.r
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
library(lubridate)
library(dplyr)
library(ggplot2) 
library(readxl)

# Milestone 1 goal: load data

# Set the working directory, change this where you run this
setwd("/home/russell/Dropbox/DataAnalysis/Oregon_Crime_Report/data")

# Define date set url for citation
census_data_url <- "https://drive.google.com/uc?export=download&id=1JrrmYiQUBPux8nnJ88epAAk9U5rbRDBD"
census_data_citation <- "U.S. Census Bureau, PL94-171 redistricting data files. Compiled by PSU Population Research Center, www.pdx.edu/prc"

# Load in census data
if(!file.exists("COUNTIES_2020.xlsx")){
  print("File does not exist locally, downloading")
  destfile <- "COUNTIES_2020.xlsx"
  download.file(url, destfile)
}

#  import Oregon county census population data
county_pop <- read_excel("COUNTIES_2020.xlsx", skip = 3)

# Name columns
names(county_pop) = c('county', '2010', '2020', 'na', 'change', 'percent change')

# Drop the last empty rows
county_pop <- head(county_pop, -3)

# Validate number of counties plus one entry for the state
if(length(county_pop$county) != 37){
  stop(paste("Expected 38 rows for county data, got: ", length(county_pop$county)))
}

# Milestone 2 goal: format and create new calculated columns

# Drop the empty 'na' column, and empty last rows
county_pop <- subset(county_pop, select = c('county', '2010', '2020', 'percent change'))

# Format percantage to proper percentage by multipling by 100, also round for aes
# rename 'precent change' to ratio change
county_pop$`ratio change` <- county_pop$`percent change`
county_pop$`percent change` <- round(county_pop$`percent change` * 100, digits = 1)

county_pop$`change per year` <- county_pop$`ratio change` / 10
county_pop$`percent per year` <- round(county_pop$`change per year` * 100, digits = 2)

# Take county column to lower case
county_pop$county <- tolower(county_pop$county)

# remove " county" from county column using regex
county_pop$county <- gsub(" county$", '', county_pop$county)

# Milestone 3 goal: forcast population from census to today using
# linea regression

# define start and end dates for forecast
start_date <- as.Date('01/01/2020', format = "%m/%d/%Y")
end_date <- floor_date(Sys.Date(), unit='month')

# Create dataframe for forcast results, start with column of months
pop_forcast <- data.frame(month=seq(start_date, length=40, by="1 month"))

# Calculate forcasted pop for each county
for(county in unique(county_pop$county)){
  
  # calculate the change multiplier
  county_change <- county_pop$`change per year`[county_pop$county == county]
  change_factor <-  (county_change / 12)
  
  # county initial pop
  county_init_pop <- county_pop$`2020`[county_pop$county == county] 
  
  # create vector of length 40 to store calc results
  popv <- (cbind(1:40) * change_factor) +1
  
  # calculate forecast
  popv <- cbind(popv * county_init_pop)
  
  # name column before merging results
  colnames(popv) = county
  
  # add results to forecast data frame
  pop_forcast <- cbind(pop_forcast,  popv)
}


# Make function to plot county histograms
county_histogram <- function(df, name){
  # get subset of data for county
  county_dates <- subset(df$date, df$county == name)
  
  hist(county_dates, "month", format = "%Y %b", 
       main=paste(name,"county Arrests by Month"), 
       sub=offenses_url,
       xlab="Months") 
}
