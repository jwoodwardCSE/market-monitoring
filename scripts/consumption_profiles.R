## Consumption profiles in

## Set up
{
  #setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")
  
  library(dplyr)
  library(tidyr)
  
}


## Read in data
{
  ## Read in normalised half-hourly data
  export.norm <- read.csv("in_raw/consumption/electricity-export-normalised-hh-profiles.csv")
  import.norm <- read.csv("in_raw/consumption/electricity-import-normalised-hh-profiles.csv")
  
  ## Read in monthly summaries
  export.mthly <- read.csv("in_raw/consumption/electricity-export-summaries-by-month.csv")
  import.mthly <- read.csv("in_raw/consumption/electricity-import-summaries-by-month.csv")
}

## Calculate yearly total import and export values
{
  export.yrly <- export.mthly %>% 
    group_by(archetype) %>%
    summarise(
      kwh.yr.exp.mean = sum(elec_exp_total_kwh_mean)
    )
  
  import.yrly <- import.mthly %>% 
    group_by(archetype) %>%
    summarise(
      kwh.yr.imp.mean = sum(elec_imp_total_kwh_mean)
    )
  
  
}

## Calculate non-normalised import and export values
{
  ## Merge the monthly and the halfhourly dataframes together
  #   This is a one-to-many match, with many half-hour windows corresponding 
  #   to the same month
  
  export.kwh <- merge(export.norm, export.yrly, all.x = T, by = "archetype")
  import.kwh <- merge(import.norm, import.yrly, all.x = T, by = "archetype")
  
  ## Calculate non-normalised kwh 
  #   For (e.g.) the 10th percentile, kwh = 10th percentile of % consumed, 
  #   multiplied by the mean total consumption over a year
  
  export.kwh <- export.kwh %>% mutate(
    ex.kwh.mean = elec_exp_mean * kwh.yr.exp.mean,
    ex.kwh.10th = elec_exp_10th * kwh.yr.exp.mean,
    ex.kwh.25th = elec_exp_25th * kwh.yr.exp.mean,
    ex.kwh.50th = elec_exp_50th * kwh.yr.exp.mean,
    ex.kwh.75th = elec_exp_75th * kwh.yr.exp.mean,
    ex.kwh.90th = elec_exp_90th * kwh.yr.exp.mean,
  )
  
  import.kwh <- import.kwh %>% mutate(
    im.kwh.mean = elec_imp_mean * kwh.yr.imp.mean,
    im.kwh.10th = elec_imp_10th * kwh.yr.imp.mean,
    im.kwh.25th = elec_imp_25th * kwh.yr.imp.mean,
    im.kwh.50th = elec_imp_50th * kwh.yr.imp.mean,
    im.kwh.75th = elec_imp_75th * kwh.yr.imp.mean,
    im.kwh.90th = elec_imp_90th * kwh.yr.imp.mean,
  )
  
}

## Merge the two dataframes together and tidy it up
{
  combined.kwh <- merge(export.kwh, import.kwh, by = c("archetype", "read_month", "read_type_of_day", "read_hh"), all.y = T)
  
  ## Modify month variable to be a number
  combined.kwh <- combined.kwh %>% mutate(
    
    month = case_when(
      read_month == "jan" ~ 1,
      read_month == "feb" ~ 2,
      read_month == "mar" ~ 3,
      read_month == "apr" ~ 4,
      read_month == "may" ~ 5,
      read_month == "jun" ~ 6,
      read_month == "jul" ~ 7,
      read_month == "aug" ~ 8,
      read_month == "sep" ~ 9,
      read_month == "oct" ~ 10,
      read_month == "nov" ~ 11,
      read_month == "dec" ~ 12
      
    ))
  
  ## Modify type_of_day to a dummy == 1 if weekend
  combined.kwh$weekend <- as.integer(combined.kwh$read_type_of_day == "weekend")
    
  ## Modify halfhourly variable to be in the form 05:30 = 5.5
  combined.kwh$window <- (combined.kwh$read_hh - 1) / 2
  
  combined.kwh <- combined.kwh %>% select(archetype, month, weekend, window, 
                                          ex.kwh.mean, ex.kwh.10th, ex.kwh.25th, ex.kwh.50th, ex.kwh.75th, ex.kwh.90th,
                                          im.kwh.mean, im.kwh.10th, im.kwh.25th, im.kwh.50th, im.kwh.75th, im.kwh.90th)

}

## Save dataframe to csv
{
  write.csv(combined.kwh, "in_clean/consumption_profiles.csv", row.names=FALSE)
  
}