# Filename: Oregon_Crime_Offenses.r
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
library(ggplot2) 
library(dplyr)
library(lubridate)


# Set the working directory to where the data sets are located
setwd("/home/russell/Dropbox/DataAnalysis/Oregon_Crime_Report")

# Load formatted data by sourcing associated r script
source('OpenData-Offenses-All.r')

all_offenses <- offenses

# Filter out records from last two months
end_month <- Sys.Date() %m-% months(2) %>%
  floor_date(unit='months')

# Filter out records from last two months
offenses <- offenses[offenses$date < end_month,]

# Milestone 2 goal: make table of number of offenses per month by type

# Use function to wrap tallying distinct offenses
tally_offenses <- function(offenses){
  # Create a table counting the number of offenses data set to study window
  offenses_m_count <- offenses %>%
    group_by(month=floor_date(date, 'month')) %>%
    summarize(count = n(), sum=sum(`Distinct Offenses`)) %>%
    rename_at("count", ~ "Total Records") %>%
    rename_at("sum", ~ "Total Distinct Offenses")
  
  # Create tables for each of the types of violent offense
  for(title in unique(offenses$`NIBRS Report Title`)){
    
    # Format column name
    col_name <- title 
    
    # Create subset of violent offenses for title and run same count
    tmp_counts <- offenses %>%
      subset(offenses$`NIBRS Report Title` == title) %>%
      group_by(month=floor_date(date, 'month')) %>%
      summarize(sum = sum(`Distinct Offenses`)) %>%
      rename_at("sum", ~ col_name) 
    
    # Merge results to month_counts
    offenses_m_count <- merge(x=offenses_m_count, y=tmp_counts, by=c('month'), all=TRUE)
  }
  
  # clean up
  rm(tmp_counts)
  rm(col_name)
  
  return(offenses_m_count)
}

# function that calls tally_offenses for county
tally_county_offenses <- function(county){
  # pipe offenses subset into the tally_offenses function defined above
  return(offenses %>% subset(`county` == county) %>% tally_offenses())
}

# Run tally algo on all offenses
offenses_m_count <- tally_offenses(offenses)

# Milestone 4 goal: explore crime types and counts 

# Create data frame for offense counts by type
crime_counts <- data.frame(title=cbind(unique(offenses$`NIBRS Report Title`)))

# relabel rows as 1,2,3,...
rownames(crime_counts) = 1:length(crime_counts[,1])

# Iterate over offense title and store result to crime_counts vector
for(tmp_crime in crime_counts$title){
  
  # Calculate the number of distinct offenses by summing column 
  # 'Distinct Offenses' w.r.t. report title.
  tmp_count <- offenses %>%
    subset(offenses$`NIBRS Report Title`== tmp_crime) %>%
    select(`Distinct Offenses`) %>%
    sum()
  
  # Save results to crime_count data frame
  crime_counts$count[crime_counts$title == tmp_crime] = tmp_count
}

# Milestone 5 goal: create a subset of "Violent Crimes"

# Violent crime is defined by the FBI as "murder and nonnegligent manslaughter, rape, robbery, and aggravated assault"
# source: https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/violent-crime

# create vector of titles that match description of "violent crime"
violent_titles <- c("Aggravated Assault",
                        "Forcible Rape", 
                        "Forcible Sodomy", 
                        "Robbery",
                        'Willful Murder')

# Encode offenses with distinct violent/nonviolent offenses
# offenses$`Distinct Violent`[offenses$`NIBRS Report Title` %in% violent_titles] <- 
#   offenses$`Distinct Offenses`[offenses$`NIBRS Report Title` %in% violent_titles]

# Create subset of all offenses using violent_titles vector
violent_offenses <- subset(offenses, offenses$`NIBRS Report Title` %in% violent_titles)

# Count total distinct offenses and violent offenses per county


# Milestone 8: count violent crimes by month


# Create a table counting the number of violent offenses per month

tally_violent_offenses <- function(offenses){
  
  # call tally_offenses and filter columns of violent offenses
  violent_m_count <- tally_offenses(offenses) %>%
    select(one_of(c(c('month', 'Total Distinct Offenses'), violent_titles))) 
  
  # sum remaining columns sans month
  violent_m_count$`All Violent Offenses` <- rowSums(select(violent_m_count, -c('month', `Total Distinct Offenses`)), na.rm=TRUE)
  
  # Calculate non violent offenses by subtracting violent offenses from total offenses
  violent_m_count$`Nonviolent Offenses` <- violent_m_count$`Total Distinct Offenses` - violent_m_count$`All Violent Offenses`
  
  return(violent_m_count)
}


violent_m_count <- tally_violent_offenses(offenses)

#lane <- tally_county_offenses('lane')

# Milestone 9: Plot violent offenses by month
plot_violent_offenses <- function (violent_m_count){
  p <- ggplot() +
    labs(y='Number per Month', title="Violent Offenses in Oregon") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
    geom_line(data=violent_m_count, aes(x=month, y=`All Violent Offenses`, color='All Violence')) +
    geom_line(data=violent_m_count, aes(x=month, y=`Aggravated Assault`, color="Assault")) +
    geom_line(data=violent_m_count, aes(x=month, y=`Robbery`, color="Robbery")) +
    geom_line(data=violent_m_count, aes(x=month, y=`Forcible Rape`, color="Rape")) +
    geom_line(data=violent_m_count, aes(x=month, y=`Willful Murder`, color="Murder")) 
  
  p
}

plot_violent_vs_non <- function(violent_m_count){
  p <- ggplot() +
    labs(y='Number per Month', title="Violent vs Non Violent Offenses in Oregon") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
    geom_line(data=violent_m_count, aes(x=month, y=`All Violent Offenses`, color='Violent Offenses')) +
    geom_line(data=violent_m_count, aes(x=month, y=`Nonviolent Offenses`, color="Nonviolent Offenses")) 
  p  
}
# Milestone 10 goal: compute offenses month counts per capita

# load census data
source("Oregon_Census_Counties.r")

# calculate oregon per capita offenses
oregon_percap <-  merge(x=pop_forcast, 
                        y=offenses_m_count, by=c('month'), all=FALSE) %>%
  select(c('month', 'Total Distinct Offenses', 'oregon'))
oregon_percap$percap <- oregon_percap$`Total Distinct Offenses` / oregon_percap$oregon


tally_percap <- function(county){
  
  # get subset of offenses for county
  county_offenses <- offenses[offenses$county == all_of(county),]
  
  # make table of violent offenses per month for the county
  county_violent_counts <- tally_violent_offenses(county_offenses)
  
  # get subset of population table for county
  county_pop <- pop_forcast %>% select(c('month', all_of(county)))
  
  # merge pop table to violent counts
  c_cap<- merge(x=county_violent_counts, y=county_pop, by=c('month'), all=FALSE) %>%
    select('month', all_of(county), 'Total Distinct Offenses','All Violent Offenses', 'Nonviolent Offenses') %>%
    rename_at(county, ~ 'county pop') 
  
  
  c_cap$'Total Distinct Offenses' <- c_cap$'Total Distinct Offenses' / c_cap$'county pop'
  c_cap$'All Violent Offenses' <- c_cap$'All Violent Offenses' / c_cap$'county pop'
  c_cap$'Nonviolent Offenses' <- c_cap$'Nonviolent Offenses' / c_cap$'county pop'
  

  # cleanup
  rm(county_pop)
  rm(county_violent_counts)
  rm(county_offenses)
  
  return(c_cap)
}


# Create table of avarge offeneses per county
tally_county_avarges <- function(offenses){
  
  # get list of counties from census
  counties <- county_pop$county
  
  # empty vector for average distinct offenses
  total_v <- c()
  percap_v <- c()
  
  for(county in counties){
    #print(paste('Proccessing:', county))
    v_count <- offenses$`Distinct Offenses`[offenses$`NIBRS Report Title` %in% violent_titles & offenses$county == county] %>%
      sum()
    
    v_percap <- 100 * v_count / county_pop$`2020`[county_pop$county == county]
    
    # add results to vectors
    total_v <- rbind(total_v, v_count)
    percap_v <- rbind(percap_v, v_percap)
  }
  
  # put result vectors in dataframe
  counts <- data.frame(county=counties, offenses=total_v, percap_off=percap_v)
  rownames(counts) = c(1:length(counties))
  
  return(counts)
}

county_averages <- tally_county_avarges(offenses)

plot_percap_all_offenses <- function (){
  p <- ggplot() +
    labs(y='Number per Month', title=" Offenses in Oregon") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
    geom_line(data=oregon_percap, aes(x=month, y=`percap`, color='offenses percapita')) 
  
  p
}

# Milestone 1 goal: compute violent offenses month counts per capita

plot_percap_violent_offenses <- function (){
  p <- ggplot() +
    labs(y='Number per Month', title="Violent Offenses in Oregon") +
    scale_x_date(date_labels = "%b %Y", date_breaks = "6 month") +
    geom_line(data=oregon_percap_violent, aes(x=month, y=`percap`, color='offenses percapita')) 
  
  p
}

# Clean up
rm(tmp_count)
rm(tmp_crime)
rm(popv)
rm(violent_offenses)

gc()

  
  
  
  
  
  