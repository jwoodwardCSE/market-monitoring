## Financial analysis mainframe 2
#setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")


## Process NESO archetype import profiles
source("scripts/consumption_profiles.R")

## Process heat pump import profile
source("scripts/heatpump_profiles_in.R")

## Process price data
#source("scripts/process_prices.R")

## Run consumption profiles against price data
source("scripts/construct_bills.R")

