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

# define end of study period. date from outlier analysis
end_date <- as.Date('2022-09-01')
end_month <- end_date %m-% months(1)

# create list of the months in the study period
month_list <- seq(floor_date(min(offenses$date), unit='month'), end_month, by='month')
start_month <- month_list[1]

# Filter out records from last two months
offenses <- offenses[offenses$date < end_date,]

# Milestone 2 goal: make table of number of offenses per month by type

# Use function to wrap tallying distinct offenses
tally_off_month <- function(offenses){
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
tally_county_off_month <- function(county){
  # pipe offenses subset into the tally_offenses function defined above
  return(offenses %>% subset(`county` == county) %>% tally_off_month())
}

# Run tally algo on all offenses
off_per_month <- tally_off_month(offenses)

# Milestone 3 goal: create a subset of "Violent Crimes"

# Violent crime is defined by the FBI as "murder and nonnegligent manslaughter, rape, robbery, and aggravated assault"
# source: https://ucr.fbi.gov/crime-in-the-u.s/2019/crime-in-the-u.s.-2019/topic-pages/violent-crime

# create vector of titles that match description of "violent crime"
violent_titles <- c("Aggravated Assault",
                        "Forcible Rape", 
                        "Forcible Sodomy", 
                        "Robbery",
                        'Willful Murder')

# Create subset of all offenses using violent_titles vector
violent_offenses <- subset(offenses, offenses$`NIBRS Report Title` %in% violent_titles)

# Milestone 4: count violent crimes by month

# Create a table counting the number of violent offenses per month
tally_vo_month <- function(offenses){

  # call tally_offenses and filter columns of violent offenses
  violent_m_count <- offenses %>%
    tally_off_month() %>%
    select(one_of(c(c('month', 'Total Distinct Offenses'), violent_titles)))

  # sum remaining columns sans month
  violent_m_count$`All Violent Offenses` <- rowSums(select(violent_m_count, -c('month', `Total Distinct Offenses`)), na.rm=TRUE)

  # Calculate non violent offenses by subtracting violent offenses from total offenses
  violent_m_count$`Nonviolent Offenses` <- violent_m_count$`Total Distinct Offenses` - violent_m_count$`All Violent Offenses`

  # Add a column for the month number
  violent_m_count$m <- 1:length(violent_m_count$month)
  violent_m_count$v_off <- violent_m_count$`All Violent Offenses`

  return(violent_m_count)
}

# get counts of violent offenses per month
vo_per_month <- tally_vo_month(offenses)

# wrap above equation for subsets of counties
tally_vo_county_month <- function(countyname){
  offenses %>%
    subset(county == countyname) %>%
    tally_vo_month() %>%
    return()
}


# Milestone 5 goal: compute offenses month counts per capita

# load census data
source("Oregon_Census_Counties.r")

# Create correlation matrix

# start with data frame with both months and months enumerated as 1,2,3,...
county_violent_month <- data.frame(x=1:length(month_list), month=month_list)

# call tally_offenses and filter columns of violent offenses
vm_count <- tally_off_month(offenses) %>%
  select(one_of(c(c('month', 'Total Distinct Offenses'), violent_titles))) 

# sum remaining columns sans month
vm_count$'All Violent Offenses' <- rowSums(select(vm_count, -c('month', `Total Distinct Offenses`)), na.rm=TRUE)

# drop left over columns
vm_count <- vm_count %>%
  select('month', 'All Violent Offenses' )

# rename 'All Violent Offenses' to county name
names(vm_count)[names(vm_count) == 'All Violent Offenses'] <- 'oregon'

# merge results to county_violent_month 
county_violent_month <- merge(x=county_violent_month, y=vm_count, by='month', all=TRUE) 

# next fill out the above data frame by looping over the counties
for(countyname in county_list){
  
  # pull subset of county offenses
  county_offenses <- subset(offenses, county == countyname)
  
  # call tally_offenses and filter columns of violent offenses
  violent_m_count <- tally_off_month(county_offenses) %>%
  select(one_of(c(c('month', 'Total Distinct Offenses'), violent_titles))) 
  
  # sum remaining columns sans month
  violent_m_count$'All Violent Offenses' <- rowSums(select(violent_m_count, -c('month', `Total Distinct Offenses`)), na.rm=TRUE)
  
  # drop left over columns
  violent_m_count <- violent_m_count %>%
    select('month', 'All Violent Offenses' )
  
  # rename 'All Violent Offenses' to county name
  names(violent_m_count)[names(violent_m_count) == 'All Violent Offenses'] <- countyname
  
  # merge results to county_violent_month 
  county_violent_month <- merge(x=county_violent_month, y=violent_m_count, by='month', all=TRUE) 
}  

# set na's to 0 
county_violent_month[is.na(county_violent_month)] = 0

cnty_violent_cor <- county_violent_month %>%
  select(-c('month')) %>%
  cor() %>%
  data.frame() %>%
  rename('pearson_cor' = 'x') %>%
  select('pearson_cor') %>%
  mutate(pearson_cor = round(pearson_cor, digits=3))

cnty_violent_cor$county = rownames(cnty_violent_cor)


# Create table counting total violent offenses plus per capita for study period
tally_vo_totals <- function(offenses){
  
  # get list of counties from census
  counties <- county_pop$county
  
  # empty vector for average distinct offenses
  vo_total <- c()
  vo_percap <- c()
  
  for(county in counties){
    # Treat 'oregon' separately 
    if(county == 'oregon'){
      vo_sum <- offenses$`Distinct Offenses`[offenses$`NIBRS Report Title` %in% violent_titles] %>%  sum()
    }else{
      vo_sum <- offenses$`Distinct Offenses`[offenses$`NIBRS Report Title` %in% violent_titles & offenses$county == county] %>%
        sum()
    }
    
    # calculate percapita violent offenses
    vo_percap_sum <- 100 * vo_sum / county_pop$`2020`[county_pop$county == county]
    
    # add results to vectors
    vo_total <- rbind(vo_total, vo_sum)
    vo_percap <- rbind(vo_percap, vo_percap_sum)
  }
  
  # put result vectors in data frame
  counts <- data.frame(county=counties, offenses=vo_total, vo_percap=vo_percap)
  
  # rename rows in data frame for month number
  rownames(counts) = c(1:length(counties))
  
  # cleanup
  rm(vo_sum)
  rm(vo_total)
  rm(vo_percap)
  
  return(counts)
}

vo_county_sums <- tally_vo_totals(offenses)


# Linear Model
violent_m_count <- tally_vo_month(offenses)

cnty_lm <- data.frame(county = c('oregon', county_list))

lm1 <- lm(month ~ `Total Distinct Offenses`, data= violent_m_count)
cnty_lm$vo_rate_year[cnty_lm$county == 'oregon'] <- round(lm1$coefficients[2] * 12, digits=3)

for(countyname in c('oregon',county_list)){
  # tally monthly violent offenses for county
  if(countyname == 'oregon'){
    v_month_total <- tally_vo_month(offenses)
    #print('oregon')
  }else{
    v_month_total <- tally_vo_county_month(countyname)
  }
  

  # create linear model
  lm1 <- lm(v_off ~ m, data= v_month_total)
  
  # copy rate coefficient from model to data frame
  cnty_lm$vo_rate_year[cnty_lm$county == countyname] <- round(lm1$coefficients[2] * 12, digits=3)
}

lm_summary <- merge(x=cnty_lm, y=cnty_violent_cor, by='county',  all=TRUE)
lm_summary <- merge(x=lm_summary, y=county_pop, by='county', all=TRUE)
lm_summary$adjusted_rate <- round(lm_summary$vo_rate_year - lm_summary$pop_rate_year, digits=3)

# Check if the sum of vo_rates across counties match oregon's
sum_county_vo_rates <- sum(lm_summary$vo_rate_year[lm_summary$county != 'oregon' & !is.na(lm_summary$vo_rate_year)])
diff_oregon <- lm_summary$vo_rate_year[lm_summary$county == 'oregon'] - sum_county_vo_rates


# Clean up
rm(cnty_lm)
rm(tmp_count)
rm(tmp_crime)
rm(popv)
rm(violent_offenses)
rm(lm1)
rm(county_offenses)
#rm(county_violent_month)

gc()

  
  
  
  
  
  