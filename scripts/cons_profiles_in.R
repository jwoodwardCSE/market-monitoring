### Read in half hourly consumption profiles by NESO archetype

## Description:1

#   This file reads in consumption profiles for each NESO consumer archetype,
#   from the outputs of the original archetyping project, with data originating
#   from SERL.

#   Additionally, data from the ESC heat pump survey is used to produce a
#   consumption profile for heat pump usage. This is then appended to NESO
#   archetype 'V' to create a consumption profile for a persona with an EV
#   and a heat pump.


## Set up
{
  setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")
  
  library(dplyr)
  library(ggplot2)
  library(tidyr)
  library(gridExtra)

}
  
## Read in NESO consumption profiles

#   These are normalised, each value representing the percentage of total 
#   yearly electricity import that occurred in each type of window (on average)
#   I.e., if for archetype VX, the window 00:00-00:30 on February weekdays
#   will have values for mean, median, 10th, 25th, 75th and 90th percentiles.
#   If the value for median = 0.0005, 0.0005% of annual
#   import occurred

{
  neso.in <- read.csv("./in_raw/neso_archetype_hlfhrly.csv")
  
  neso.in$elec_imp_mean <- prettyNum(neso.in$elec_imp_mean, scientific = FALSE, digits = 10)
  neso.in$elec_imp_50th <- prettyNum(neso.in$elec_imp_50th, scientific = FALSE, digits = 10)
  neso.in$elec_imp_10th <- prettyNum(neso.in$elec_imp_10th, scientific = FALSE, digits = 10)
  neso.in$elec_imp_25th <- prettyNum(neso.in$elec_imp_25th, scientific = FALSE, digits = 10)
  neso.in$elec_imp_75th <- prettyNum(neso.in$elec_imp_75th, scientific = FALSE, digits = 10)
  neso.in$elec_imp_90th <- prettyNum(neso.in$elec_imp_90th, scientific = FALSE, digits = 10)
  
  ## Convert text month to number
  neso.in <- neso.in %>% mutate(
    
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
      
    )
    
  )
  
  ## Add a variable to represent the number of windows of that type in a year
  neso.in$n_days <- 8.7
  neso.in$n_days[neso.in$read_type_of_day == "weekday"] <- 21.7
  
}


## Test to see if normalised values sum to 1 within percentile/mean groups
{
 norm.test <-neso.in %>% 
    group_by(archetype) %>%
    summarise(
      mean.sum = sum(as.numeric(elec_imp_mean) * n_days),
      p50th.sum =sum(as.numeric(elec_imp_50th) * n_days),
      p75th.sum = sum(as.numeric(elec_imp_75th) * n_days),
      p25th.sum = sum(as.numeric(elec_imp_25th) * n_days),
      p90th.sum = sum(as.numeric(elec_imp_90th) * n_days),
      p10th.sum = sum(as.numeric(elec_imp_10th) * n_days)
    )
  
  
}


## Read in data of monthly summaries of electricity import
{
  neso.sum <- read.csv("./in_raw/neso_monthly.csv")
  colnames(neso.sum)[3] <- "read_month"
}

## Calculate yearly import from monthly import data
{
  
  ## Create numeric month variable for the monthly dataset
  neso.sum <- neso.sum %>% mutate(
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
  
  
  ## Summarise yearly import (as a sum of monthly import) by archetype
  neso.year <- neso.sum %>% group_by(archetype) %>%
    summarise(
      mean.year = sum(elec_imp_total_kwh_mean),
      p50th.year = sum(elec_imp_total_kwh_50th),
      p10th.year = sum(elec_imp_total_kwh_10th),
      p25th.year = sum(elec_imp_total_kwh_25th),
      p75th.year = sum(elec_imp_total_kwh_75th),
      p90th.year = sum(elec_imp_total_kwh_90th),
    )
  
}


## Merge the halfhourly and yearly dataframes together

#   To each half-hour window, attach the sum of consumption for that archetype
#   over a year

profiles <- merge(neso.in,  neso.year, by = "archetype" )


## Calculate halfhourly kwh from yearly kwh and halfhourly proportion
{
  
  ## halfhourly kWh import = mean yearly import * halfhourly % of mean yearly import
  
  profiles$kwh.mean <- as.numeric(profiles$elec_imp_mean) * profiles$mean.year
  profiles$kwh.50th <- as.numeric(profiles$elec_imp_50th) * profiles$mean.year
  profiles$kwh.10th <- as.numeric(profiles$elec_imp_10th) * profiles$mean.year
  profiles$kwh.25th <- as.numeric(profiles$elec_imp_25th) * profiles$mean.year
  profiles$kwh.75th <- as.numeric(profiles$elec_imp_75th) * profiles$mean.year
  profiles$kwh.90th <- as.numeric(profiles$elec_imp_90th) * profiles$mean.year
}



### Identify which NESO archetypes correspond to each persona. match them
{
  ## Persona 1: Small young family in council housing, with storage heaters and Economy 7 meter.
  # NESO Arch: ES
  
  ## Persona 2: Older single renter with limited digital skills. Gas central heating and prepayment meter (PPM).
  # NESO Arch: G10p
  
  ## Persona 3: Owner-occupier couple with heat pump, solar PV, EV charger and smart meter.
  # NESO Arch: VX
  
  ## Persona 4: Large family owner-occupiers, gas central heating and smart meter.
  # NESO Arch: G22
  
  ## Filter dataframe to select for these archetypes
  #profiles <- profiles %>% filter(archetype == "ES" | archetype == "G10p" | archetype == "VX" | archetype == "G22")
  
  ## Create a new persona variable
  profiles$persona <- "P1"
  profiles$persona[profiles$archetype == "G10p"] <- "P2"
  profiles$persona[profiles$archetype == "VX"] <- "P3"
  profiles$persona[profiles$archetype == "G22"] <- "P4"
  
}


## Write outputs to csv
{
  write.csv(neso.year, "./in_clean/neso_yearly_summaries.csv", row.names = FALSE)
  
  write.csv(profiles, "./in_clean/neso_cons_profiles_clean.csv", row.names = FALSE)
  
}


## Sort the heat pump profiles
{
  ## Read in heat pump energy use data
  HP.profile <- read.csv("./in_clean/HP_ESC_profile.csv") %>% select(window, month, weekend, mean.Q_heat_elec_in, p50.Q_heat_elec_in, p10.Q_heat_elec_in, p25.Q_heat_elec_in, p75.Q_heat_elec_in, p90.Q_heat_elec_in)
  
  colnames(HP.profile) <- c("window", "month", "weekend", "kwh.mean", "kwh.50th", "kwh.10th", "kwh.25th", "kwh.75th", "kwh.90th")
  
  ## Add some variables to help with merging to neso profile data
  {
    neso.in$weekend <- as.integer(neso.in$read_type_of_day == "weekend")
    HP.profile$read_hh <- (HP.profile$window +0.5) * 2
    HP.profile$archetype <- "HP"
  }
  
  HP.long <- HP.profile %>% pivot_longer( cols = kwh.mean:kwh.90th , names_to = "type", values_to = "kwh" )
  
  ## Test to see if works by plotting it
  {
    persona.plot.percentiles <- function(df, month.in, dayofweek, title_in){
      
      plot.out <- ggplot(df %>% filter(month == month.in & weekend == dayofweek),
                         aes(x=as.numeric(window), y= kwh, color = type)) +
        geom_line( size = 1.2, aes( linetype = ifelse(type == "kwh.mean" | type == "kwh.50th", "dashed", "solid") ) ) +
        scale_color_manual(values = group.colors) +
        labs(title = title_in) +
        guides(linetype = "none") +
        ylim(0, 1.5)#0.00065)
      
      
      return(plot.out)
    } 
    
    group.colors <- c(kwh.10th = "#11d646", kwh.25th = "#aef046", kwh.50th ="red", kwh.75th = "#f0d046", kwh.90th = "#f08a46", kwh.mean = "blue")    
    
    HP.plot <- persona.plot.percentiles(HP.long, 2, 0, "1/2 hourly electricity imports: HP, heating only, feb weekdays")
    HP.plot
  }
  
  ## Create two new profiles:
  {
    ## Persona 3 : HP + EV
    {
      persona3.calc <- merge(profiles %>% filter(archetype == "V"), HP.profile, by = c("read_hh", "month", "weekend"))
      
      persona3.calc$kwh.mean <- persona3.calc$kwh.mean.x + persona3.calc$kwh.mean.y
      persona3.calc$kwh.50th <- persona3.calc$kwh.50th.x + persona3.calc$kwh.50th.y
      persona3.calc$kwh.10th <- persona3.calc$kwh.10th.x + persona3.calc$kwh.10th.y
      persona3.calc$kwh.25th <- persona3.calc$kwh.25th.x + persona3.calc$kwh.25th.y
      persona3.calc$kwh.75th <- persona3.calc$kwh.75th.x + persona3.calc$kwh.75th.y
      persona3.calc$kwh.90th <- persona3.calc$kwh.90th.x + persona3.calc$kwh.90th.y
      
      persona3.calc <- persona3.calc %>% select(read_hh, month, weekend, kwh.mean, kwh.50th, kwh.10th, kwh.25th, kwh.75th, kwh.90th )
      
      persona3.calc$archetype <- "HP_EV"
      persona3.calc$window <- (persona3.calc$read_hh - 1) / 2
      
      HPEV.plot <- persona.plot.percentiles(persona3.calc %>% pivot_longer(names_to = "type", cols = starts_with("kwh."), values_to = "kwh"), 2, 0,"1/2 hourly electricity imports: HP_EV, heating only, feb weekdays")
      HPEV.plot
    }
    
    ## Persona 3a : HP + PV
    {
      persona3A.calc <- merge(profiles %>% filter(archetype == "X"), HP.profile, by = c("read_hh", "month", "weekend"))
      
      persona3A.calc$kwh.mean <- persona3A.calc$kwh.mean.x + persona3A.calc$kwh.mean.y
      persona3A.calc$kwh.50th <- persona3A.calc$kwh.50th.x + persona3A.calc$kwh.50th.y
      persona3A.calc$kwh.10th <- persona3A.calc$kwh.10th.x + persona3A.calc$kwh.10th.y
      persona3A.calc$kwh.25th <- persona3A.calc$kwh.25th.x + persona3A.calc$kwh.25th.y
      persona3A.calc$kwh.75th <- persona3A.calc$kwh.75th.x + persona3A.calc$kwh.75th.y
      persona3A.calc$kwh.90th <- persona3A.calc$kwh.90th.x + persona3A.calc$kwh.90th.y
      
      persona3A.calc <- persona3A.calc %>% select(read_hh, month, weekend, kwh.mean, kwh.50th, kwh.10th, kwh.25th, kwh.75th, kwh.90th )
      
      persona3A.calc$archetype <- "HP_PV"
      persona3A.calc$window <- (persona3A.calc$read_hh - 1) / 2
      
      HPPV.plot <- persona.plot.percentiles(persona3A.calc %>% pivot_longer(names_to = "type", cols = starts_with("kwh."), values_to = "kwh"), 2, 0,"1/2 hourly electricity imports: HP_PV, heating only, feb weekdays")
      HPPV.plot
      
    }
    
    
    profiles <- bind_rows(profiles, persona3.calc, persona3A.calc )
    
    
  }
}





## Pivot data long
p.long <- profiles %>% select(archetype, read_hh, month, weekend, kwh.mean, kwh.50th, kwh.10th, kwh.25th, kwh.75th, kwh.90th ) %>% pivot_longer(names_to = "type", cols = starts_with("kwh."), values_to = "kwh")


### Save cleaned data
write.csv(p.long,"./in_clean/long_profiles.csv", row.names = FALSE)
write.csv(profiles,"./in_clean/profiles.csv", row.names = FALSE)



### Plot for each persona
{
  
  group.colors <- c(kwh.10th = "#11d646", kwh.25th = "#aef046", kwh.50th ="red", kwh.75th = "#f0d046", kwh.90th = "#f08a46", kwh.mean = "blue")
  
  persona.plot.percentiles <- function(df, pers, month.in, dayofweek){
    
    plot.out <- ggplot(df %>% filter(archetype == pers & read_month == month.in & weekend == dayofweek),
                       aes(x=read_hh, y= kwh, color = type)) +
      geom_line( size = 1.2, aes( linetype = ifelse(type == "kwh.mean" | type == "kwh.50th", "dashed", "solid") ) ) +
      scale_color_manual(values = group.colors) +
      labs(title = paste("1/2 hourly electricity imports:", pers, month.in, dayofweek, sep = " "))+
      guides(linetype = "none") +
      ylim(0, 1.5)#0.00065)
    
    
    return(plot.out)
  }
  
  persona.plot.months <- function(df, pers, pctl, dayofweek){
    
    plot.out <- ggplot(df %>% filter(archetype == pers & weekend == dayofweek & type == pctl),
                       aes(x=read_hh, y= kwh, color = as.factor(mth.num))) +
      geom_line(  ) +
      labs(title = paste("1/2 hourly electricity imports:", pers, pctl, dayofweek, sep = " "))+
      guides(linetype = "none") +
      ylim(0, 1.5)#0.0003)
    
    
    return(plot.out)
  }
  
  persona.plot.personas <- function(df, dayofweek, month.in, title.in){
    
    plot.out <- ggplot(df %>% filter(month == month.in & weekend == dayofweek & type == "kwh.mean" & archetype %in% c( "V", "X", "VX", "HP_EV", "HP_PV")),
                       aes(x=read_hh, y= kwh, color = archetype)) +
      geom_line( size = 1.2 ) +
      labs(title = title.in)+
      guides(linetype = "none") +
      ylim(0, 1.5)#0.00065)
    
    
    return(plot.out)
    
  }
  
  {
   
    test <- persona.plot.personas(p.long , 1, 2, "comparison across personas")
    test 
    
    ## Plot each relevant percentile (and mean), for each persona (for weekdays in feb)
    p1.pct <- persona.plot.percentiles(p.long, "ES", "feb", 0)
    p2.pct <- persona.plot.percentiles(p.long, "G10p", "feb", 0)
    VX.pct <- persona.plot.percentiles(p.long, "VX", "feb", 0)
    p4.pct <- persona.plot.percentiles(p.long, "G22", "feb", 0)
    V.pct <- persona.plot.percentiles(p.long, "V", "feb", 0)
    X.pct <- persona.plot.percentiles(p.long, "X", "feb", 0)
    
    
    grid.arrange(V.pct, X.pct, VX.pct, HPEV.plot, HPPV.plot, HP.plot, nrow = 3 )
    
    
    ## Plot mean weekday electricity import profiles by month, for each persona
    p1.mth <- persona.plot.months(p.long, "ES", "kwh.mean", "weekday")
    p2.mth <- persona.plot.months(p.long, "G10p", "kwh.mean", "weekday")
    p3.mth <- persona.plot.months(p.long, "VX", "kwh.mean", "weekday")
    p4.mth <- persona.plot.months(p.long, "G22", "kwh.mean", "weekday")
    
    grid.arrange(p1.pct, p2.pct, p3.pct, p4.pct,
                 nrow = 2)
    
    grid.arrange(p1.mth, p2.mth, p3.mth, p4.mth,
                 nrow = 2)
    
    grid.arrange(V.pct, X.pct, p3.pct, nrow = 1)
    
    
  }
}

## plot G22

p.long$hr <- (p.long$read_hh / 2) - 0.5

ggplot(p.long %>% filter(archetype == "G22" & read_month == "feb" & read_type_of_day == "weekday", type == "kwh.mean"),
       aes(x=hr, y= kwh)) +
  geom_line( size = 1.2, aes( linetype = ifelse(type == "kwh.mean" | type == "kwh.50th", "dashed", "solid") ) ) +
  labs(title = "1/2 hourly electricity imports: G22 feb weekday")+
  guides(linetype = "none") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2),
                     minor_breaks=seq(0,1, by= 0.1), limits = c(0,1))+
  scale_x_continuous(breaks = seq(0, 24, by = 6),
                     minor_breaks=seq(0,24, by= 1))


