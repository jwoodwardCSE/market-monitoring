## Construct bills

## Set up
{
  #setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")
  
  library(dplyr)
  library(tidyr)
  library(lubridate)
  library(gridExtra)
  library(ggplot2)
}


## Read in data
{
  prices <- read.csv("in_clean/all_prices.csv") %>% filter(!is.na(flex.oct_unit))
  {
    prices$date <-  as.Date(prices$win.start)
    
    ## Add Outgoing Octopus and Outgoing Octopus Lite
    prices$outgoing.oct_unit <- 15
    prices$outgoing.oct.lite_unit <- 8
    
    ## Add BG PeakSave (bodged, using Octopus Fixed 12M)
    
    {
      ## Make a list of the day of week for each date in the price df
      dayofweeks <- weekdays(prices$date)
      
      ## Create BG peaksave = Octopus Fixed 12M, unless on sundays (11am-4PM), 
      #     when = 0.5 * Octopus Fixed 12M
      prices$bg.peaksave_unit <- prices$PAYG_unit #Updated to PAYG rate
      prices$bg.peaksave_unit[dayofweeks == "Sunday" & prices$window >10.5 & prices$window < 16] <- 0.5 * prices$PAYG_unit[dayofweeks == "Sunday" & prices$window >10.5 & prices$window < 16]
      
      
    }
    
    
    ## Create BG Dimplex tariff
    ## price == 9.9p/ kWh between 12:30 and 7:30 AM
    ## day rate == Octopus fixed Nov 2024 e7 day rate otherwise
    {
      prices <- prices %>% group_by(date) %>% mutate(
        bg.dimplex_unit = case_when(
          window > 0 & window < 7.5 ~ 0.99,
          window == 0 | window > 7 ~  max(oct.e7.fix.nov24_unit)
          
        )
      )
    }
     
    
  }

  consumption <- read.csv("in_clean/cons_profiles_w_heatpump.csv")
  {
    #consumption$window <- (consumption$read_hh - 1)/ 2 
    #consumption <- consumption %>% select(window, month, weekend, archetype, mean.year, kwh.mean, kwh.10th, kwh.25th, kwh.50th, kwh.75th, kwh.90th)
    
  }
}
unique(bills_24$window)
## Calculate bills for 2024
{
  ## Filter out prices from 2025
  prices_24 <- prices %>% filter(win.start< as.POSIXct("2025-01-01 00:00:00"))
  
  ## Join with consumption profiles
  bills_24  <- left_join(consumption, prices_24, by = c("month", "window","weekend"))%>%
    mutate(edf.hpt.unit=case_when(window>=4&window<7~(flex.oct_unit-10),
                                  window>=13&window<16~(flex.oct_unit-10),
                                  TRUE~flex.oct_unit)) #Added to include edf heat pump tracker
  
  ## Calculate bills for all personas, for all tariffs
  bills_24_tab <- bills_24 %>% group_by(archetype) %>%
    summarise(
      mean.fixed = round(sum(im.kwh.mean * oct.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.fixed.e7 = round(sum(im.kwh.mean * oct.e7.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.e7.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      mean.cosy = round(sum(im.kwh.mean * cosy_unit, na.rm=T) /100, 2) + round(sum(cosy_stand, na.rm=T) /(100*48), 2),
      mean.go = round(sum(im.kwh.mean * go_unit, na.rm=T) /100, 2) + round(sum(go_stand, na.rm=T) /(100*48), 2),
      mean.flux = round(sum(im.kwh.mean * flux.import_unit, na.rm=T) /100, 2) + round(sum(flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intel_flux = round(sum(im.kwh.mean * intel.flux.import_unit, na.rm=T) /100, 2) + round(sum(intel.flux.import_stand, na.rm=T) /(100*48), 2),
      mean.PAYG = round(sum(im.kwh.mean * PAYG_unit, na.rm=T) /100, 2) + round(sum(PAYG_stand, na.rm=T) /(100*48), 2),
      mean.flexPAYG = round(sum(im.kwh.mean * flex.PAYG_unit, na.rm=T) /100, 2) + round(sum(flex.PAYG_stand, na.rm=T) /(100*48), 2),
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave = round(sum(im.kwh.mean * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      
      mean.SEG_out  = round(sum(ex.kwh.mean * SEG_unit, na.rm=T) /100, 2),
      mean.flux_out  = round(sum(ex.kwh.mean * flux.out_unit, na.rm=T) /100, 2),
      mean.intel_flux_out = round(sum(ex.kwh.mean * intel.flux.out_unit, na.rm=T) /100, 2),
      mean.agile_out  = round(sum(ex.kwh.mean * agile.out_unit, na.rm=T) /100, 2),
      mean.outgoing_oct = round(sum(ex.kwh.mean * outgoing.oct_unit, na.rm=T) /100, 2),
      mean.outgoing_oct_lite = round(sum(ex.kwh.mean * outgoing.oct.lite_unit, na.rm=T) /100, 2),
      mean.edf.hpt = round(sum(im.kwh.mean * edf.hpt.unit, na.rm=TRUE)/100,2)
    )
  
  ## Calculate bills for specific personas
  
  ## Persona 3 and 5
  bills_24_tab_per3_5 <- bills_24 %>% 
    filter(archetype %in% c("VX", "HP_only")) %>%
    group_by(archetype) %>%
    summarise(
      ## Total import and export over the period
      sum.im.kwh = round(sum(im.kwh.mean),2),
      sum.ex.kwh = round(sum(ex.kwh.mean),2),
      
      ## Baselines for comparison
      mean.fixed = round(sum(im.kwh.mean * oct.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.oct24_stand, na.rm=T) /(100*48), 2), #Changed this to jan to match 2_4
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      
      ## Persona specific tariffs
      mean.cosy = round(sum(im.kwh.mean * cosy_unit, na.rm=T) /100, 2) + round(sum(cosy_stand, na.rm=T) /(100*48), 2),
      mean.go = round(sum(im.kwh.mean * go_unit, na.rm=T) /100, 2) + round(sum(go_stand, na.rm=T) /(100*48), 2),
      mean.flux = round(sum(im.kwh.mean * flux.import_unit, na.rm=T) /100, 2) + round(sum(flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intel_flux = round(sum(im.kwh.mean * intel.flux.import_unit, na.rm=T) /100, 2) + round(sum(intel.flux.import_stand, na.rm=T) /(100*48), 2),

      ## Agile, import = mean
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      
      ## Export tariffs
      mean.SEG_out  = round(sum(ex.kwh.mean * SEG_unit, na.rm=T) /100, 2),
      mean.flux_out  = round(sum(ex.kwh.mean * flux.out_unit, na.rm=T) /100, 2),
      mean.intel_flux_out = round(sum(ex.kwh.mean * intel.flux.out_unit, na.rm=T) /100, 2),
      mean.agile_out  = round(sum(ex.kwh.mean * agile.out_unit, na.rm=T) /100, 2),
      mean.outgoing_oct = round(sum(ex.kwh.mean * outgoing.oct_unit, na.rm=T) /100, 2),
      mean.outgoing_oct_lite = round(sum(ex.kwh.mean * outgoing.oct.lite_unit, na.rm=T) /100, 2),
      mean.edf.hpt = round(sum(im.kwh.mean * edf.hpt.unit, na.rm=TRUE)/100,2)
      
      
    )
}

unique(prices_24$outgoing.oct_unit)
names(prices_24)

## Calculate bills for 2024, including demand shifting into "cheaper" windows
#   Required for shifting demand under BG 'PeakSave' Sundays tariff
{
  ## Filter out prices from 2025
  prices_24_shift <- prices %>% filter(win.start< as.POSIXct("2025-01-01 00:00:00"))
  
  ## Join with consumption profiles
  bills_24_shift  <- left_join(consumption, prices_24_shift, by = c("month", "window","weekend"))
  
  ## Calculate shifted consumption
  {
    ## BG "PeakSave"
    ##  consumers shift X% of their total Saturday demand to the Sunday "PeakSave" period
    ##  
    {
      
      ## Calculate total Saturday demand per persona
      ##  Total saturday demand = 50% of total sunday demand,
      ##  as weekend days are treated identically in consumption profiles
      
      bills_24_shift$dayofweek <- weekdays(bills_24_shift$date)
      
      bills_24_shift <- bills_24_shift %>%
        group_by(archetype) %>%
        mutate(
          ## im.kwh.mean * weekend = total weekend consumption, as weekend is a dummy == 1 if weekend
          ## 0.5 * total weekend import = total Saturday import
          im_saturday = 0.5 * sum(im.kwh.mean * weekend) / 52)
      
      ## Calculate import value
      
      bills_24_shift <- bills_24_shift %>% mutate(
        
        
        ## 10% of saturday import shifted to peaksave window
        
        im.kwh.BG_ps_10pct = case_when(
          
          dayofweek == "Sunday" & window > 10.5 & window < 16 ~ im.kwh.mean + (0.1*(im_saturday/10)),
          dayofweek == "Sunday" & (window < 11 | window > 15.5 ) ~ im.kwh.mean,
          dayofweek == "Saturday" ~ 0.9 * im.kwh.mean,
          dayofweek != "Saturday" & dayofweek != "Sunday"~ im.kwh.mean
          
        ),
        
        ## 20% of saturday import shifted to peaksave window
        
        im.kwh.BG_ps_20pct = case_when(
          
          dayofweek == "Sunday" & window > 10.5 & window < 16 ~ im.kwh.mean + (0.2*(im_saturday/10)),
          dayofweek == "Sunday" & (window < 11 | window > 15.5 ) ~ im.kwh.mean,
          dayofweek == "Saturday" ~ 0.8 * im.kwh.mean,
          dayofweek != "Saturday" & dayofweek != "Sunday"~ im.kwh.mean
          
        ),
        
        ## 30% of saturday import shifted to peaksave window
        
        im.kwh.BG_ps_30pct = case_when(
          
          dayofweek == "Sunday" & window > 10.5 & window < 16 ~ im.kwh.mean + (0.3*(im_saturday/10)),
          dayofweek == "Sunday" & (window < 11 | window > 15.5 ) ~ im.kwh.mean,
          dayofweek == "Saturday" ~ 0.7 * im.kwh.mean,
          dayofweek != "Saturday" & dayofweek != "Sunday"~ im.kwh.mean
          
        ),
        
        ## 50% of saturday import shifted to peaksave window
        
        im.kwh.BG_ps_50pct = case_when(
          
          dayofweek == "Sunday" & window > 10.5 & window < 16 ~ im.kwh.mean + (0.5*(im_saturday/10)),
          dayofweek == "Sunday" & (window < 11 | window > 15.5 ) ~ im.kwh.mean,
          dayofweek == "Saturday" ~ 0.5 * im.kwh.mean,
          dayofweek != "Saturday" & dayofweek != "Sunday"~ im.kwh.mean
          
        )
        
      )
      
      
        
    }
    
    
  }
  names(bills_24_shift)
  
  ## Calculate bills
  bills_24_shift_tab <- bills_24_shift %>% group_by(archetype) %>%
    summarise(
      mean.fixed = round(sum(im.kwh.mean * oct.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.fixed.e7 = round(sum(im.kwh.mean * oct.e7.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.e7.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      mean.cosy = round(sum(im.kwh.mean * cosy_unit, na.rm=T) /100, 2) + round(sum(cosy_stand, na.rm=T) /(100*48), 2),
      mean.go = round(sum(im.kwh.mean * go_unit, na.rm=T) /100, 2) + round(sum(go_stand, na.rm=T) /(100*48), 2),
      mean.flux = round(sum(im.kwh.mean * flux.import_unit, na.rm=T) /100, 2) + round(sum(flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intel_flux = round(sum(im.kwh.mean * intel.flux.import_unit, na.rm=T) /100, 2) + round(sum(intel.flux.import_stand, na.rm=T) /(100*48), 2),
      mean.PAYG = round(sum(im.kwh.mean * PAYG_unit, na.rm=T) /100, 2) + round(sum(PAYG_stand, na.rm=T) /(100*48), 2),
      mean.flexPAYG = round(sum(im.kwh.mean * flex.PAYG_unit, na.rm=T) /100, 2) + round(sum(flex.PAYG_stand, na.rm=T) /(100*48), 2),
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave = round(sum(im.kwh.mean * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      
      mean.SEG_out  = round(sum(ex.kwh.mean * SEG_unit, na.rm=T) /100, 2),
      mean.flux_out  = round(sum(ex.kwh.mean * flux.out_unit, na.rm=T) /100, 2),
      mean.intel_flux_out = round(sum(ex.kwh.mean * intel.flux.out_unit, na.rm=T) /100, 2),
      mean.agile_out  = round(sum(ex.kwh.mean * agile.out_unit, na.rm=T) /100, 2),
      mean.outgoing_oct = round(sum(ex.kwh.mean * outgoing.oct_unit, na.rm=T) /100, 2),
      mean.outgoing_oct_lite = round(sum(ex.kwh.mean * outgoing.oct.lite_unit, na.rm=T) /100, 2),
    )
  
  ## Calculate bills for specific personas
  bills_24_shift_per2_4 <- bills_24_shift %>% #
    filter(archetype %in% c("G22", "G10p")) %>%
    group_by(archetype) %>%
    summarise(
      
      ## Baseline tariffs for comparison
      mean.fixed = round(sum(im.kwh.mean * oct.fix.jan24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      
      ## Persona specific tariffs
      mean.PAYG = round(sum(im.kwh.mean * PAYG_unit, na.rm=T) /100, 2) + round(sum(PAYG_stand, na.rm=T) /(100*48), 2),
      mean.flexPAYG = round(sum(im.kwh.mean * flex.PAYG_unit, na.rm=T) /100, 2) + round(sum(flex.PAYG_stand, na.rm=T) /(100*48), 2),
      
      ## British Gas PeakSave for comparison
      mean.BG.peaksave = round(sum(im.kwh.mean * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave_10pct = round(sum(im.kwh.BG_ps_10pct * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave_20pct = round(sum(im.kwh.BG_ps_20pct * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave_30pct = round(sum(im.kwh.BG_ps_30pct * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      mean.BG.peaksave_50pct = round(sum(im.kwh.BG_ps_50pct * bg.peaksave_unit, na.rm=T) /100, 2) + round(sum(oct.fix.jan24_stand, na.rm=T) /(100*48), 2),
      
      ## Agile, import == mean
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      
    )
  
  

}

## Calculate bills for nov 2024 -> mar 2025
#   Required for Snug Octopus & Octopus Intelligent Go
{
  ## Filter out prices from prior to nov 2024
  prices_nov24 <- prices %>% filter(win.start > as.POSIXct("2024-10-31 00:00:00"))
  
  ## Join with consumption profiles
  bills_nov24  <- left_join(consumption, prices_nov24, by = c("month", "window","weekend"))
  
  ## Calculate bills for all personas, for all tariffs
  bills_nov24_tab <- bills_nov24 %>% group_by(archetype) %>%
    summarise(
      sum.im.kwh = round(sum(im.kwh.mean),2),
      sum.ex.kwh = round(sum(ex.kwh.mean),2),
      
      mean.fixed = round(sum(im.kwh.mean * oct.fix.oct24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.oct24_stand, na.rm=T) /(100*48), 2),
      mean.fixed.e7 = round(sum(im.kwh.mean * oct.e7.fix.nov24_unit, na.rm=T) /100, 2) + round(sum(oct.e7.fix.nov24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      mean.cosy = round(sum(im.kwh.mean * cosy_unit, na.rm=T) /100, 2) + round(sum(cosy_stand, na.rm=T) /(100*48), 2),
      mean.go = round(sum(im.kwh.mean * go_unit, na.rm=T) /100, 2) + round(sum(go_stand, na.rm=T) /(100*48), 2),
      mean.flux = round(sum(im.kwh.mean * flux.import_unit, na.rm=T) /100, 2) + round(sum(flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intel_flux = round(sum(im.kwh.mean * intel.flux.import_unit, na.rm=T) /100, 2) + round(sum(intel.flux.import_stand, na.rm=T) /(100*48), 2),
      mean.PAYG = round(sum(im.kwh.mean * PAYG_unit, na.rm=T) /100, 2) + round(sum(PAYG_stand, na.rm=T) /(100*48), 2),
      mean.flexPAYG = round(sum(im.kwh.mean * flex.PAYG_unit, na.rm=T) /100, 2) + round(sum(flex.PAYG_stand, na.rm=T) /(100*48), 2),
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      
      mean.snug  = round(sum(im.kwh.mean * snug_unit, na.rm=T) /100, 2) + round(sum(snug_stand, na.rm=T) /(100*48), 2),
      mean.intelgo  = round(sum(im.kwh.mean * intelligent_go_unit, na.rm=T) /100, 2) + round(sum(intelligent_go_stand, na.rm=T) /(100*48), 2),
      mean.oev_intelgo  = round(sum(im.kwh.mean * oev_intelligent_go_unit, na.rm=T) /100, 2) + round(sum(oev_intelligent_go_stand, na.rm=T) /(100*48), 2),
      
      
      mean.SEG_out  = round(sum(ex.kwh.mean * SEG_unit, na.rm=T) /100, 2),
      mean.flux_out  = round(sum(ex.kwh.mean * flux.out_unit, na.rm=T) /100, 2),
      mean.intel_flux_out = round(sum(ex.kwh.mean * intel.flux.out_unit, na.rm=T) /100, 2),
      mean.agile_out  = round(sum(ex.kwh.mean * agile.out_unit, na.rm=T) /100, 2),
      mean.outgoing_oct = round(sum(ex.kwh.mean * outgoing.oct_unit, na.rm=T) /100, 2),
      mean.outgoing_oct_lite = round(sum(ex.kwh.mean * outgoing.oct.lite_unit, na.rm=T) /100, 2),
      
      
    )
 
  
  
  ## Calculate bills for specific archetypes
  bills_nov24_tab_per1 <- bills_nov24 %>% 
    filter(archetype == "ES") %>%
    group_by(archetype) %>%
    summarise(
      ## Baselines for comparison
      mean.fixed = round(sum(im.kwh.mean * oct.fix.oct24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.oct24_stand, na.rm=T) /(100*48), 2),
      mean.fixed.e7 = round(sum(im.kwh.mean * oct.e7.fix.nov24_unit, na.rm=T) /100, 2) + round(sum(oct.e7.fix.nov24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      
      ## Persona specific tariffs
      mean.snug  = round(sum(im.kwh.mean * snug_unit, na.rm=T) /100, 2) + round(sum(snug_stand, na.rm=T) /(100*48), 2),
      mean.BG.dimplex = round(sum(im.kwh.mean * bg.dimplex_unit, na.rm=T) /100, 2) + round(sum(oct.e7.fix.nov24_stand, na.rm=T) /(100*48), 2),
      
      ## Agile, import = mean
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
    )
  
  bills_nov24_tab_per3_5 <- bills_nov24 %>% 
    filter(archetype %in% c("VX", "HP_only")) %>%
    group_by(archetype) %>%
    summarise(
      ## Total import and export over the period
      sum.im.kwh = round(sum(im.kwh.mean),2),
      sum.im.heat.kwh = round(sum(mean.Q_heat_elec_in),2),
      sum.ex.kwh = round(sum(ex.kwh.mean),2),
      
      
      ## Baselines for comparison
      mean.fixed = round(sum(im.kwh.mean * oct.fix.oct24_unit, na.rm=T) /100, 2) + round(sum(oct.fix.oct24_stand, na.rm=T) /(100*48), 2),
      mean.flex = round(sum(im.kwh.mean * flex.oct_unit, na.rm=T) /100, 2) + round(sum(flex.oct_stand, na.rm=T) /(100*48), 2),
      
      ## Persona specific tariffs
      mean.cosy = round(sum(im.kwh.mean * cosy_unit, na.rm=T) /100, 2) + round(sum(cosy_stand, na.rm=T) /(100*48), 2),
      mean.go = round(sum(im.kwh.mean * go_unit, na.rm=T) /100, 2) + round(sum(go_stand, na.rm=T) /(100*48), 2),
      mean.flux = round(sum(im.kwh.mean * flux.import_unit, na.rm=T) /100, 2) + round(sum(flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intel_flux = round(sum(im.kwh.mean * intel.flux.import_unit, na.rm=T) /100, 2) + round(sum(intel.flux.import_stand, na.rm=T) /(100*48), 2),
      mean.intelgo  = round(sum(im.kwh.mean * intelligent_go_unit, na.rm=T) /100, 2) + round(sum(intelligent_go_stand, na.rm=T) /(100*48), 2),
      mean.oev_intelgo  = round(sum(im.kwh.mean * oev_intelligent_go_unit, na.rm=T) /100, 2) + round(sum(oev_intelligent_go_stand, na.rm=T) /(100*48), 2),
      
      ## Agile, import = mean
      mean.agile_noflex  = round(sum(im.kwh.mean * agile.in_unit, na.rm=T) /100, 2) + round(sum(agile.in_stand, na.rm=T) /(100*48), 2),
      
      ## Export tariffs
      mean.SEG_out  = round(sum(ex.kwh.mean * SEG_unit, na.rm=T) /100, 2),
      mean.flux_out  = round(sum(ex.kwh.mean * flux.out_unit, na.rm=T) /100, 2),
      mean.intel_flux_out = round(sum(ex.kwh.mean * intel.flux.out_unit, na.rm=T) /100, 2),
      mean.agile_out  = round(sum(ex.kwh.mean * agile.out_unit, na.rm=T) /100, 2),
      mean.outgoing_oct = round(sum(ex.kwh.mean * outgoing.oct_unit, na.rm=T) /100, 2),
      mean.outgoing_oct_lite = round(sum(ex.kwh.mean * outgoing.oct.lite_unit, na.rm=T) /100, 2),
      
      
    )
  
   
}


## Agile
{
  ## Filter out prices from 2025
  prices_fl2 <- prices %>% filter(win.start< as.POSIXct("2025-01-01 00:00:00"))
  
  ## Join prices and consumption
  bills_fl2  <- left_join(consumption, prices_fl2, by = c("month", "window", "weekend"))
  
  ## Summarise percentiles of agile prices within months & windows
  agile_sum <- prices_fl2 %>% group_by(month, window) %>%
    summarise(
      mean.agile = mean(agile.in_unit, na.rm = T),
      min.agile = min(agile.in_unit, na.rm = T),
      max.agile = max(agile.in_unit, na.rm = T),
      p10.agile = quantile(agile.in_unit, 0.1, na.rm= T),
      p25.agile = quantile(agile.in_unit, 0.25, na.rm= T),
      p50.agile = quantile(agile.in_unit, 0.5, na.rm= T),
      p75.agile = quantile(agile.in_unit, 0.75, na.rm= T),
      p90.agile = quantile(agile.in_unit, 0.9, na.rm= T),
    )
  
  # Join this with consumption and prices
  bills_fl2 <- left_join(bills_fl2, agile_sum, by = c("month", "window"))
  
  # Identify what quantile band (of agile prices) each window falls into
  bills_fl2 <- bills_fl2 %>%
    mutate(
      agile_pct = case_when(
        agile.in_unit <= p10.agile ~ "under_p10",
        agile.in_unit > p10.agile & agile.in_unit <= p25.agile ~ "p10_p25",
        agile.in_unit > p25.agile & agile.in_unit <= p50.agile ~ "p25_p50",
        agile.in_unit > p50.agile & agile.in_unit <= p75.agile ~ "p50_p75",
        agile.in_unit > p75.agile & agile.in_unit <= p90.agile ~ "p75_p90",
        agile.in_unit > p90.agile ~ "above_p90",
      )
    )
  
  # Create dummy variables == 1 depending on the percentile band of the agile price
  bills_fl2$under_p10 <- as.integer(bills_fl2$agile.in_unit <= bills_fl2$p10.agile)
  bills_fl2$p10_p25   <- as.integer(bills_fl2$agile.in_unit <= bills_fl2$p25.agile & bills_fl2$agile.in_unit > bills_fl2$p10.agile)
  bills_fl2$p25_p50   <- as.integer(bills_fl2$agile.in_unit <= bills_fl2$p50.agile & bills_fl2$agile.in_unit > bills_fl2$p25.agile)
  bills_fl2$p50_p75   <- as.integer(bills_fl2$agile.in_unit <= bills_fl2$p75.agile & bills_fl2$agile.in_unit > bills_fl2$p50.agile)
  bills_fl2$p75_p90   <- as.integer(bills_fl2$agile.in_unit <= bills_fl2$p90.agile & bills_fl2$agile.in_unit > bills_fl2$p75.agile)
  bills_fl2$above_p90 <- as.integer(bills_fl2$agile.in_unit >  bills_fl2$p90.agile)
  
  
  
  ## Flexibility algorithms
  {
    ## Anti-flex (i.e., higher consumption when agile is high),
    ## with total consumption preserved within months
    
    ##    Process:
    ##    1a. Anti-flex
    ##        If agile price is between 50th and 75th pctl,
    ##        import = 62.5th pctl (= imp.50th + ((imp.75th - imp.50th)/2))
    ##        If agile price is between 75th and 90th pctl, import = 82.5th
    ##        If agile price is >90th pctl, import = 90th pctl
    ##        Low price windows have decreased demand to compensate
    ##
    ##    1b. Flex
    ##        If agile price is between 50th and 75th pctl, import = 37.5th pctl
    ##        If agile price is between 75th and 90th pctl, import = 17.5th pctl
    ##        If agile price is above 90th pctl, import = 10th pctl
    ##        Low price windows have increased demand to compensate
    ##
    ##    1c. Turn-down
    ##        If agile price is between 50th and 75th pctl, import = 37.5th pctl
    ##        If agile price is between 75th and 90th pctl, import = 17.5th pctl
    ##        If agile price is above 90th pctl, import = 10th pctl
    ##        Low price window demand is not increased to compensate
    ##
    ##    1d. Turn-up
    ##        If agile price is between 50th and 25th pctl,import = 62.5th pctl
    ##        If agile price is between 10th and 25th pctl, import = 82.5th
    ##        If agile price is <10th pctl, import = 90th pctl
    ##        High price window demand is not decreased to compensate
    ##
    ##    2.  Within months (for each archetype), calculate the sum kwh prior 
    ##        to this change and after. calculate the difference between the two
    ##
    ##    3.  Calculate the total import during windows in that month with agile price 
    ##        < 50th pctl. Divide the difference in monthly import by this 
    ##        number to create a proportion.
    ##        For each window where agile price < 50th pctl, multiply import 
    ##        by this proportion.
    ##
    ##    4.  Check to see if this new value adds up to pre-antiflex monthly totals

    
    
    
    ## 1: Set agile response prices
    {
      # Calculate fractional import percentiles
      bills_fl2$im.kwh_17.5th <- bills_fl2$im.kwh.10th + ((bills_fl2$im.kwh.25th - bills_fl2$im.kwh.10th)/2)
      bills_fl2$im.kwh_37.5th <- bills_fl2$im.kwh.25th + ((bills_fl2$im.kwh.50th - bills_fl2$im.kwh.25th)/2)
      bills_fl2$im.kwh_62.5th <- bills_fl2$im.kwh.50th + ((bills_fl2$im.kwh.75th - bills_fl2$im.kwh.50th)/2)
      bills_fl2$im.kwh_82.5th <- bills_fl2$im.kwh.75th + ((bills_fl2$im.kwh.90th - bills_fl2$im.kwh.75th)/2)
      
      # Baseline import = mean
      bills_fl2$s1.antiflex <- bills_fl2$im.kwh.mean
      bills_fl2$s2.flex <- bills_fl2$im.kwh.mean
      
      
      # If agile is between 50th and 75th percentiles, 
      # s1.antiflex: import = 62.5th
      # s2.flex: import = 37.5th
      
      bills_fl2$s1.antiflex[bills_fl2$agile_pct == "p50_p75"] <- bills_fl2$im.kwh_62.5th[bills_fl2$agile_pct == "p50_p75"]
      bills_fl2$s2.flex[bills_fl2$agile_pct == "p50_p75"] <- bills_fl2$im.kwh_37.5th[bills_fl2$agile_pct == "p50_p75"]
      
      
      # If agile is between 75th and 90th percentiles, 
      # s1.antiflex: import = 82.5th
      # s2.flex: import = 17.5th
      
      bills_fl2$s1.antiflex[bills_fl2$agile_pct == "p75_p90"] <- bills_fl2$im.kwh_82.5th[bills_fl2$agile_pct == "p75_p90"]
      bills_fl2$s2.flex[bills_fl2$agile_pct == "p75_p90"] <- bills_fl2$im.kwh_17.5th[bills_fl2$agile_pct == "p75_p90"]
      
      
      # If agile is above 90th percentile, 
      # s1.antiflex: import = 90th percentile
      # s2.flex: import = 10th percentile
      
      bills_fl2$s1.antiflex[bills_fl2$agile_pct == "above.p90"] <- bills_fl2$im.kwh.90th[bills_fl2$agile_pct == "above.p90"]
      bills_fl2$s2.flex[bills_fl2$agile_pct == "above.p90"] <- bills_fl2$im.kwh.10th[bills_fl2$agile_pct == "above.p90"]
      
    }
    
    
    ## 2: Calculate monthly sums and differences
    {
      bills_fl2 <- bills_fl2 %>% 
        group_by(month, archetype) %>%
        mutate(
          median_mth_sum = sum(im.kwh.mean),
          antiflex_mth_sum = sum(s1.antiflex),
          antiflex_diff_mth_sum = sum(s1.antiflex) - sum(im.kwh.mean),
          
          flex_mth_sum = sum(s2.flex),
          flex_diff_mth_sum = sum(s2.flex) - sum(im.kwh.mean)
        )
    }
    
    ## 3: Spread additional/lesser consumption across low-cost periods
    {
      bills_fl2 <- bills_fl2 %>% 
        group_by(month, archetype) %>%
        mutate(
          imp.sub.p50_mth = sum(under_p10 * im.kwh.mean) + sum(p10_p25 * im.kwh.mean) + sum(p25_p50 * im.kwh.mean)
          
        )
      
      ## If, for any archetype or month, this value is lower than the additional 
      ## import created by anti-flexing, print a warning
      
      if (nrow(bills_fl2 %>% filter(imp.sub.p50_mth < antiflex_diff_mth_sum)) > 0){
        print(nrow(bills_fl2 %>% filter(imp.sub.p50_mth < antiflex_diff_mth_sum)))
      }
      
      ## Represent additional high-cost period consumption as a proportion 
      ## of total low-cost period consumption
      
      bills_fl2$antiflex_pct_shift <- bills_fl2$antiflex_diff_mth_sum / bills_fl2$imp.sub.p50_mth
      
      bills_fl2$flex_pct_shift <- -1* bills_fl2$flex_diff_mth_sum / bills_fl2$imp.sub.p50_mth
      
      ## Calculate low-cost period consumption, by multiplying pre-antiflex
      ## consumption by (1-proportion of additional high-cost period consumption
      ## compared to total pre-antiflex low-cost period consumption)
      
      bills_fl2 <- bills_fl2 %>%
        mutate(
          s1.antiflex_adj = case_when(
            under_p10 == 1 | p10_p25 == 1 | p25_p50   == 1 ~ s1.antiflex * (1-antiflex_pct_shift),
            p50_p75 == 1   | p75_p90 == 1 | above_p90 == 1 ~ s1.antiflex
          ),
          s2.flex_adj = case_when(
            under_p10 == 1 | p10_p25 == 1 | p25_p50   == 1 ~ s2.flex * (1+flex_pct_shift),
            p50_p75 == 1   | p75_p90 == 1 | above_p90 == 1 ~ s2.flex
          ),
          ## Calculate turndown (i.e., reduced demand in expensive periods, 
          ## but no increased demand during cheap periods)
          s3.turndown = case_when(
            under_p10 == 1 | p10_p25 == 1 | p25_p50   == 1 ~ s1.antiflex * (1-antiflex_pct_shift),
            p50_p75 == 1   | p75_p90 == 1 | above_p90 == 1 ~ s2.flex
          ),
          ## Calculate turnup (i.e., increased demand during cheap periods,
          ## but no decreased demand during expensive periods)
          s4.turnup = case_when(
            under_p10 == 1 | p10_p25 == 1 | p25_p50   == 1 ~ s2.flex * (1+flex_pct_shift),
            p50_p75 == 1   | p75_p90 == 1 | above_p90 == 1 ~ s1.antiflex
          )
        )
    }
    
    ## 4: Check to see if monthly sums add up to the pre-flex total
    {
      bills_fl2 <- bills_fl2 %>% 
        group_by(month, archetype) %>%
        mutate(
          antiflex.adj_mth_sum = sum(s1.antiflex_adj),
          antiflex.adj_diff_mth_sum = sum(s1.antiflex_adj) - sum(im.kwh.mean),
          
          flex.adj_mth_sum = sum(s2.flex_adj),
          flex.adj_diff_mth_sum = sum(s2.flex_adj) - sum(im.kwh.mean)
        )
      
      if( sum(bills_fl2$antiflex.adj_diff_mth_sum) > 1 | sum(bills_fl2$flex.adj_diff_mth_sum) > 1){
        print("concerning difference between adjusted monthly sum and real monthly sum" )
      }
    }
    
    ## Plot a random day for G22
    {
      ## Choose a random day
      rand.day <- unique(bills_fl2$date)[round(runif(1)*length(unique(bills_fl2$date)))]
      
      ## Extract data for G22 on that day
      bills_day <- bills_fl2 %>% filter(date == rand.day & archetype == "G22")
      
      ## Plot import at different percentiles
      test.imp <- ggplot(bills_day %>% select(window, 
                                              im.kwh.mean, im.kwh.10th, im.kwh.25th, im.kwh.50th, im.kwh.75th, im.kwh.90th
      ) %>% pivot_longer(cols = im.kwh.mean:im.kwh.90th, names_to = "pctile"),
      aes(x = window, y = value, colour = pctile)) + 
        labs(title = paste0("G22 consumption, percentile bands: ",rand.day)) +
        geom_line() +
        theme_bw()
      #print(test.imp)
      
      ## Plot agile price (real) and different percentiles for that window
      test.agile <- ggplot(bills_day %>% select(window, agile.in_unit, p50.agile) %>% ##, p10.agile, p25.agile, p75.agile, p90.agile) %>% 
                             pivot_longer(cols = agile.in_unit:p50.agile, names_to = "agile_price"),
                           aes(x = window, y = value, colour = agile_price)) + 
        labs(title = paste0("Agile price, real vs percentile bands, " ,rand.day)) +
        geom_line(aes(linetype = agile_price %in% c("agile.in_unit", "p50.agile"))) +
        scale_linetype_manual(values = c("TRUE" = "solid", "FALSE" = "dashed"), guide = "none")+
        theme_bw()
      
      #print(test.agile)
      
      #test.grid <- grid.arrange(test.imp, test.agile, nrow = 2)
      
      
      ## Choose consumption that relates to the nearest agile percentile band
      ##    if agile < 10th percentile, choose 10th percentile
      ##    if agile < 25th percentile, choose 25th percentile
      ##    vice versa for 75th and 90th
      {
      
        ## Plot this consumption 
        bills_day_flex <- bills_day %>% 
          select(window, im.kwh.mean, s1.antiflex_adj, s2.flex_adj) %>% 
          pivot_longer(cols = im.kwh.mean:s2.flex_adj, names_to = "type")
        
        #bills_day_flex$type[bills_day_flex$type == "im.kwh.50th"] <- "real_median_import"
        #bills_day_flex$type[bills_day_flex$type == "imp.strat1"]  <- "hypothetical_flex_import"
        
        test.imp_flex <- ggplot(bills_day_flex, aes(x = window, y = value, colour = type)) + 
          labs(title = paste0("Median import vs hypothetical 'anti-flex' and 'flex' import: ", rand.day)) +
          geom_line() +
          theme_bw()
        #print(test.imp_flex)
        
      }
      
      test.grid <- grid.arrange(test.imp, test.agile, test.imp_flex, nrow = 3)
      # print(test.grid)
      
      #mean.agile, p10.agile, p25.agile, p50.agile, p75.agile, p90.agile
    }
    
    
  }
  
  
  ## Calculate yearly bills
  {
    ## Under baseline non-flexing consumption
    bills_fl2$s0.noflex_bills <- bills_fl2$im.kwh.mean * bills_fl2$agile.in_unit
    
    ## Under  algorithm s1.antiflex
    bills_fl2$s1.antiflex_bills <- bills_fl2$s1.antiflex_adj * bills_fl2$agile.in_unit
    
    ## Under algorithm s2.flex
    bills_fl2$s2.flex_bills <- bills_fl2$s2.flex_adj * bills_fl2$agile.in_unit
    
    ## Under algorithm s3.turndown
    bills_fl2$s3.turndown_bills <- bills_fl2$s3.turndown * bills_fl2$agile.in_unit
    
    ## Under algorithm s4.turnup
    bills_fl2$s4.turnup_bills <- bills_fl2$s4.turnup * bills_fl2$agile.in_unit
    
    ## Agile summary
    {
      bills_agile_sum <- bills_fl2 %>%
        group_by(archetype) %>%
        summarise(
          ## Import and Export
          total.im.kwh = round((sum(im.kwh.mean)),2),
          total.ex.kwh = round((sum(ex.kwh.mean)),2),
          
          ## Baseline for comparison
          oct.fix.jan24_bills = round( (sum(oct.fix.jan24_stand) / (100*48)) + (sum(oct.fix.jan24_unit * im.kwh.mean)/100)  ,2),
          oct.e7.fix.jan24_bills = round( (sum(oct.e7.fix.jan24_stand) / (100*48)) + (sum(oct.e7.fix.jan24_unit * im.kwh.mean)/100)  ,2),
          
          ## Agile under different strategies
          no_flex_bills = round( (sum(agile.in_stand) / (100*48)) + (sum(s0.noflex_bills)/100)  ,2),
          s1_antiflex_bills = round( (sum(agile.in_stand) / (100*48)) + (sum(s1.antiflex_bills)/100)  ,2),
          s2.flex_bills = round( (sum(agile.in_stand) / (100*48)) + (sum(s2.flex_bills)/100)  ,2),
          s3.turndown_bills = round( (sum(agile.in_stand) / (100*48)) + (sum(s3.turndown_bills)/100)  ,2),
          s4.turnup_bills = round( (sum(agile.in_stand) / (100*48)) + (sum(s4.turnup_bills)/100)  ,2),
          
          ## Export tariffs
          ex.oct_outgoing_bills = round( (sum(ex.kwh.mean * outgoing.oct_unit) / 100)  ,2),
          ex.agile_bills = round( (sum(ex.kwh.mean * agile.out_unit) / 100)  ,2),
          
        )
      
    }
    
    
  }
}

## Write summary tables to csv
{
  # ## All Personas: jan 24 - dec 24
  # write.csv(bills_24_tab, "out/per1_optimal_bills.csv", row.names = F)
  # 
  # ## All Personas: Nov 24 - Mar 24
  # write.csv(bills_nov24_tab, "out/per1_optimal_bills.csv", row.names = F)
  # 
  # 
  # ## Persona 1: Nov 24 - Mar 25
  # write.csv(bills_nov24_tab_per1, "out/per1_optimal_bills.csv", row.names = F)
  # 
  # ## Persona 2 & 4: Jan 24 - Dec 24 (w/ demand shifting)
  # write.csv(bills_24_shift_per2_4, "out/per2_4_optimal_bills.csv", row.names = F)
  # 
  # ## Persona 3 & 5: Jan 24 - Dec 24 (w/ demand shifting)
   write.csv(bills_24_tab_per3_5, "out/per3_5_optimal_bills.csv", row.names = F)
  # 
  # ## Persona 3: Nov 24 - Mar 25
  # write.csv(bills_nov24_tab_per3_5, "out/per3_nov_optimal_bills.csv", row.names = F)
  # 
  # ## Agile
  # write.csv(bills_agile_sum, "out/agile_bills_table.csv", row.names = F)
  
  
}





