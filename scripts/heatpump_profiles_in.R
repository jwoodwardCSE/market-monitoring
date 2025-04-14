# Heat pump consumption profiles

## Set up
{
  #setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)
  
}

## Read in NESO archetype consumption profiles
{
  profiles <- read.csv("in_clean/consumption_profiles.csv")

}

## Read in HP consumption profile
{

  ## Read in heat pump energy use data
  HP.profile <- read.csv("in_raw/HP_ESC_profile.csv") %>% select(window, month, weekend, mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in)
  
  colnames(HP.profile)[1:3] <- c("window", "month", "weekend")
  
  ## Add some variables to help with merging to neso profile data
  {
    HP.profile$archetype <- "HP"
  }
 
}

#### Create new profiles for two new profiles:

## Persona 3 : HP + EV
{
  ## Merge the heat pump consumption profile with the consumption profile for EV owners ('V')
  persona3.calc <- merge(profiles %>% filter(archetype == "V"), HP.profile, by = c("window", "month", "weekend"))
  
  
  ## Calculate halfhourly elec import per percentile/mean and archetype, 
  #  by summing archetype "V" elec import and HP elec import
  
  persona3.calc$im.kwh.mean <- persona3.calc$im.kwh.mean + persona3.calc$mean.Q_heat_elec_in
  persona3.calc$im.kwh.50th <- persona3.calc$im.kwh.50th + persona3.calc$p50.Q_heat_elec_in
  persona3.calc$im.kwh.10th <- persona3.calc$im.kwh.10th + persona3.calc$p10.Q_heat_elec_in
  persona3.calc$im.kwh.25th <- persona3.calc$im.kwh.25th + persona3.calc$p25.Q_heat_elec_in
  persona3.calc$im.kwh.75th <- persona3.calc$im.kwh.75th + persona3.calc$p75.Q_heat_elec_in
  persona3.calc$im.kwh.90th <- persona3.calc$im.kwh.90th + persona3.calc$p90.Q_heat_elec_in
  
  ## Select relevant variables
  persona3.calc <- persona3.calc %>% select(window, month, weekend, 
                                            im.kwh.mean, im.kwh.50th, im.kwh.10th, im.kwh.25th, im.kwh.75th, im.kwh.90th,
                                            ex.kwh.mean, ex.kwh.50th, ex.kwh.10th, ex.kwh.25th, ex.kwh.75th, ex.kwh.90th,
                                            mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in )
  
  persona3.calc$archetype <- "HP_EV"
 
}

## Persona 3a : HP + PV
{
  ## Merge the heat pump consumption profile with the consumption profile for EV owners ('X')
  persona3A.calc <- merge(profiles %>% filter(archetype == "X"), HP.profile, by = c("window", "month", "weekend"))
  
  
  ## Calculate halfhourly elec import per percentile/mean and archetype, 
  #  by summing archetype "X" elec import and HP elec import
  
  persona3A.calc$im.kwh.mean <- persona3A.calc$im.kwh.mean + persona3A.calc$mean.Q_heat_elec_in
  persona3A.calc$im.kwh.50th <- persona3A.calc$im.kwh.50th + persona3A.calc$p50.Q_heat_elec_in
  persona3A.calc$im.kwh.10th <- persona3A.calc$im.kwh.10th + persona3A.calc$p10.Q_heat_elec_in
  persona3A.calc$im.kwh.25th <- persona3A.calc$im.kwh.25th + persona3A.calc$p25.Q_heat_elec_in
  persona3A.calc$im.kwh.75th <- persona3A.calc$im.kwh.75th + persona3A.calc$p75.Q_heat_elec_in
  persona3A.calc$im.kwh.90th <- persona3A.calc$im.kwh.90th + persona3A.calc$p90.Q_heat_elec_in
  
  ## Select relevant variables
  persona3A.calc <- persona3A.calc %>% select(window, month, weekend, 
                                            im.kwh.mean, im.kwh.50th, im.kwh.10th, im.kwh.25th, im.kwh.75th, im.kwh.90th,
                                            ex.kwh.mean, ex.kwh.50th, ex.kwh.10th, ex.kwh.25th, ex.kwh.75th, ex.kwh.90th,
                                            mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in )
  
  persona3A.calc$archetype <- "HP_PV"
  
}

## Persona 3B : HP + PV + EV
{
  ## Merge the heat pump consumption profile with the consumption profile for EV owners ('VX')
  persona3B.calc <- merge(profiles %>% filter(archetype == "VX"), HP.profile, by = c("window", "month", "weekend"))
  
  
  ## Calculate halfhourly elec import per percentile/mean and archetype, 
  #  by summing archetype "VX" elec import and HP elec import
  
  persona3B.calc$im.kwh.mean <- persona3B.calc$im.kwh.mean + persona3B.calc$mean.Q_heat_elec_in
  persona3B.calc$im.kwh.50th <- persona3B.calc$im.kwh.50th + persona3B.calc$p50.Q_heat_elec_in
  persona3B.calc$im.kwh.10th <- persona3B.calc$im.kwh.10th + persona3B.calc$p10.Q_heat_elec_in
  persona3B.calc$im.kwh.25th <- persona3B.calc$im.kwh.25th + persona3B.calc$p25.Q_heat_elec_in
  persona3B.calc$im.kwh.75th <- persona3B.calc$im.kwh.75th + persona3B.calc$p75.Q_heat_elec_in
  persona3B.calc$im.kwh.90th <- persona3B.calc$im.kwh.90th + persona3B.calc$p90.Q_heat_elec_in
  
  ## Select relevant variables
  persona3B.calc <- persona3B.calc %>% select(window, month, weekend, 
                                              im.kwh.mean, im.kwh.50th, im.kwh.10th, im.kwh.25th, im.kwh.75th, im.kwh.90th,
                                              ex.kwh.mean, ex.kwh.50th, ex.kwh.10th, ex.kwh.25th, ex.kwh.75th, ex.kwh.90th,
                                              mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in )
  
  persona3B.calc$archetype <- "HP_VX"
  
}



## Persona 3C : HP ONLY
{
  
  ## Merge the heat pump consumption profile with the consumption profile for G22
  persona3C.calc <- merge(profiles %>% filter(archetype == "G22"), HP.profile, by = c("window", "month", "weekend"))
  
  
  ## Calculate halfhourly elec import per percentile/mean and archetype, 
  #  by summing archetype "VX" elec import and HP elec import
  
  persona3C.calc$im.kwh.mean <- persona3C.calc$im.kwh.mean + persona3C.calc$mean.Q_heat_elec_in
  persona3C.calc$im.kwh.50th <- persona3C.calc$im.kwh.50th + persona3C.calc$p50.Q_heat_elec_in
  persona3C.calc$im.kwh.10th <- persona3C.calc$im.kwh.10th + persona3C.calc$p10.Q_heat_elec_in
  persona3C.calc$im.kwh.25th <- persona3C.calc$im.kwh.25th + persona3C.calc$p25.Q_heat_elec_in
  persona3C.calc$im.kwh.75th <- persona3C.calc$im.kwh.75th + persona3C.calc$p75.Q_heat_elec_in
  persona3C.calc$im.kwh.90th <- persona3C.calc$im.kwh.90th + persona3C.calc$p90.Q_heat_elec_in
  
  ## Select relevant variables
  persona3C.calc <- persona3C.calc %>% select(window, month, weekend, 
                                              im.kwh.mean, im.kwh.50th, im.kwh.10th, im.kwh.25th, im.kwh.75th, im.kwh.90th,
                                              ex.kwh.mean, ex.kwh.50th, ex.kwh.10th, ex.kwh.25th, ex.kwh.75th, ex.kwh.90th,
                                              mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in )
  
  persona3C.calc$archetype <- "HP_only"
  
}

## Join the rows from these new profiles to the main dataset
profiles <- bind_rows(profiles, persona3.calc, persona3A.calc, persona3B.calc, persona3C.calc )

## Trim unused columns from dataframe, and filter out irrelevant archetypes
profiles <- profiles %>% 
  select(archetype, window, month, weekend,
         ex.kwh.mean, ex.kwh.50th, ex.kwh.10th, ex.kwh.25th, ex.kwh.75th, ex.kwh.90th, 
         im.kwh.mean, im.kwh.50th, im.kwh.10th, im.kwh.25th, im.kwh.75th, im.kwh.90th,
         mean.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p50.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in
         ) %>% 
  filter(archetype %in% c("G10p", "ES", "G22", "VX", "HP_EV", "HP_PV", "HP_VX", "HP_only"))


## Write output to csv
{
  write.csv(profiles, "in_clean/cons_profiles_w_heatpump.csv")
  
  }
