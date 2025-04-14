## Read prices from Octopus API


## Set up
{
  #setwd("C:/Users/julianw/OneDrive - Centre for Sustainable Energy/Projects/Market monitoring financial analysis/clean_project")
  
  library(dplyr)
  library(tidyr)
  library(httr)
  library(jsonlite)
  library(lubridate)
  library(data.table)
}

## Read in profiles data
{
  profiles <- read.csv("in_clean/cons_profiles_w_heatpump.csv")
  
}

## Connect to octopus API, read in product descriptions
{

  # Get content from product description URL
  res = GET("https://api.octopus.energy/v1/products/")
  
  # Parse JSON into table of products and descriptions
  products = fromJSON(rawToChar(res$content))
  
  # force variables to character, as some stored as list
  products <- as.data.frame(apply(products$results, 2, as.character)) 

  # Write products table to csv
  write.csv(products, "in_raw/octopus_api_products_list.csv", row.names=FALSE)
  
  # identify the dates each product was introduced / discontinued 
  #   note: none have been discontinued (all products have NA for their "available to")
  products$date_from <- as.POSIXct(products$available_from)
  #products$date_to <- as.POSIXct(products$available_to)
  
  # flag if the product was not available for calendar year 2024
  products$flag <- as.integer(products$date_from < as.POSIXct("2024-01-01"))
  
  # filter out products not available for calendar year 2024
  #products <- products %>% filter(flag == 1)
}

## User defined function for reading price data over several pages of API calls
price.scraper <- function(prod.name, start.date, end.date){
  
  # Convert the product name into a URL
  page1.url <- paste0("https://api.octopus.energy/v1/products/",
                      prod.name,
                      "/electricity-tariffs/E-1R-",
                      prod.name,
                      "-L/standard-unit-rates/?period_from=", 
                      start.date,
                      "T00:00Z&period_to=",
                      end.date,
                      "T00:00Z")
  print(page1.url)
  
  pagenum <- 0
  i <- 0
  while (i == 0){
    
    # If first page, use the first-page url, otherwise use the next-page url
    # taken from the api call
    if (pagenum == 0){
      url.in <- page1.url
    } else{
      url.in <- new.url
    }
    
    # Call the API, extract the price data
    res = GET(url.in)
    read.in_JSON = fromJSON(rawToChar(res$content))
    prices.out <- as.data.frame(apply(read.in_JSON$results, 2, as.character))
    
    if (ncol(prices.out) ==1) {
      ## If there is only one observation for price, it needs to be transposed as 
      ## R automatically reads the single row as a single column
      prices.out <- transpose(prices.out)
      colnames(prices.out) <- c("value_exc_vat", "value_inc_vat", "valid_from", "valid_to", "payment_method")
   
    }
    
    # If the first page, save the price data as the start of the concatenated
    # dataset, otherwise, append this price data to the previously extracted set
    if (pagenum == 0){
      prices.concat <- prices.out
    } else{
      prices.concat <- bind_rows(prices.concat, prices.out)
    }
    
    # If the API provides a URL for the next page, 
    if("next" %in% names(read.in_JSON) & is.character(read.in_JSON$`next`)){
      new.url <- read.in_JSON$`next`
      print(new.url)
    } else if (ncol(prices.concat) ==1 & pagenum == 0) {
      ## If there is only one observation for price, it needs to be transposed as 
      ## R automatically reads the single row as a single column
      prices.concat <- transpose(prices.concat)
      colnames(prices.concat) <- c("value_exc_vat", "value_inc_vat", "valid_from", "valid_to", "payment_method")
      i <- 1
    } else{
      i <- 1
    }
    
    ## Wait two seconds to avoid going above rate limit
    Sys.sleep(0.5)
    
    ## Increase pagenumber variable by 1,
    pagenum <- pagenum + 1
  }
  
  #write.csv(prices.concat,"./out/test.csv")
  
  return(prices.concat)
  
}

## User defined function for reading standing charge info
stcharge.scraper <- function(prod.name, start.date, end.date){
  
  # Turn product name into url
  page1.url <- paste0("https://api.octopus.energy/v1/products/",
                      prod.name,
                      "/electricity-tariffs/E-1R-",
                      prod.name,
                      "-L/standing-charges/?period_from=",
                      start.date,
                      "T00:00Z&period_to=",
                      end.date
                      ,"T00:00Z")
  print(page1.url)
  
  pagenum <- 0
  i <- 0
  while (i == 0){
    
    # If first page, use the first-page url, otherwise use the next-page url
    # taken from the api call
    if (pagenum == 0){
      url.in <- page1.url
    } else{
      url.in <- new.url
    }
    
    # Call the API, extract the price data
    res = GET(url.in)
    read.in_JSON = fromJSON(rawToChar(res$content))
    prices.out <- as.data.frame(apply(read.in_JSON$results, 2, as.character))
    
    
    if (ncol(prices.out) ==1) {
      ## If there is only one observation for price, it needs to be transposed as 
      ## R automatically reads the single row as a single column
      prices.out <- transpose(prices.out)
      colnames(prices.out) <- c("value_exc_vat", "value_inc_vat", "valid_from", "valid_to", "payment_method")
      
    }
    
    # If the first page, save the price data as the start of the concatenated
    # dataset, otherwise, append this price data to the previously extracted set
    if (pagenum == 0){
      prices.concat <- prices.out
    } else{
      prices.concat <- bind_rows(prices.concat, prices.out)
    }
    
    # If the API provides a URL for the next page, 
    if("next" %in% names(read.in_JSON) & is.character(read.in_JSON$`next`)){
      new.url <- read.in_JSON$`next`
      #print(new.url)
    } else if (ncol(prices.concat) ==1 & pagenum == 0) {
      
      ## If there is only one observation for price, it needs to be transposed as 
      ## R automatically reads the single row as a single column
      prices.concat <- transpose(prices.concat)
      colnames(prices.concat) <- c("value_exc_vat", "value_inc_vat", "valid_from", "valid_to", "payment_method")
      i <- 1
    } else {
      i <- 1
    }
    
    ## Wait two seconds to avoid going above rate limit
    Sys.sleep(0.5)
    
    ## Increase pagenumber variable by 1,
    pagenum <- pagenum + 1
  }
  
  #print(prices.concat)
  
  return(prices.concat)
}

## User defined function for getting price data into halfhourly format
prices.2.calendar <- function(prices, start.times, end.times, start.date, end.date){
  
  ## Turn price and time variables into dataframe
  df.in <- data.frame(price = prices,  
                      start.times = as.POSIXct(start.times, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ"),
                      end.times = as.POSIXct(end.times, tz = "UTC", format = "%Y-%m-%dT%H:%M:%SZ")) #, format = "%Y-%m-%dT%H:%M:%SZ",
  
  ## Sort to ascending order (i.e., earliest dates first)
  df.in <- df.in %>% arrange(start.times)
  
  ## Create variable for "end of window", which is the first lead of the "start time" variable
  #df.in$end.times <- lead(df.in$start.times) 
  
  ## Filter out any rows for which end.times is na (as the last row will have no "lead", it will be NA) 
  df.in <- df.in %>% filter(!is.na(end.times))
  
  ## Create a dataframe with a row for each half hour window in the year
  df.out <- data.frame(win.start = seq.POSIXt(
    as.POSIXct(paste0(start.date," 00:00:00")),
    as.POSIXct(paste0(end.date," 23:30:00")),
    by = "30 min"
  ),
  price = NA)
  
  for (w in 1:(nrow(df.out)-48)){
    
    if (length(df.in$price[ (df.in$end.times > df.out$win.start[w] & df.in$start.times <= df.out$win.start[w])]) > 0){
      df.out$price[w] <- df.in$price[ (df.in$end.times > df.out$win.start[w] & df.in$start.times <= df.out$win.start[w])]
    } else{
      print(paste0(w, " , ", df.out$win.start[w]))
      break
    }
  }

  return(df.out)
}   

## User defined function to run through the previously described functions for a tariff
## For tariffs covering the 2024 period
prices.process <- function(prod.name, nickname){
  
  unit.prices.concat <- price.scraper(prod.name, "2024-01-01", "2025-03-25")
  unit.prices.calendar <- prices.2.calendar(unit.prices.concat$value_inc_vat, unit.prices.concat$valid_from, unit.prices.concat$valid_to, "2024-01-01", "2025-03-25")
  
  stand.charge.concat <- stcharge.scraper(prod.name, "2024-01-01", "2025-03-25")
  stand.charge.calendar <- prices.2.calendar(stand.charge.concat$value_inc_vat, stand.charge.concat$valid_from, stand.charge.concat$valid_to, "2024-01-01", "2025-03-25")
  
  both_calendar <- unit.prices.calendar
  both_calendar$standing_charge <- stand.charge.calendar$price
  
  colnames(both_calendar) <- c("win.start", paste0(nickname,"_unit"), paste0(nickname,"_stand"))
  
  return(both_calendar)
  
}

## The same, but for set dates
prices.process_dates <- function(prod.name, nickname, start.date, end.date){
  
  unit.prices.concat <- price.scraper(prod.name, start.date, end.date)
  unit.prices.calendar <- prices.2.calendar(unit.prices.concat$value_inc_vat, unit.prices.concat$valid_from, unit.prices.concat$valid_to,  start.date, end.date)
  
  stand.charge.concat <- stcharge.scraper(prod.name,  start.date, end.date)
  stand.charge.calendar <- prices.2.calendar(stand.charge.concat$value_inc_vat, stand.charge.concat$valid_from, stand.charge.concat$valid_to,  start.date, end.date)
  
  both_calendar <- unit.prices.calendar
  both_calendar$standing_charge <- stand.charge.calendar$price
  
  colnames(both_calendar) <- c("win.start", paste0(nickname,"_unit"), paste0(nickname,"_stand"))
  
  return(both_calendar)
  
}



## From API, call prices for specific products
{
  
  # Export tariffs
  {
    ## Agile Outgoing Octopus
    agileout.concat <- price.scraper("AGILE-OUTGOING-BB-23-02-28", "2024-01-01", "2025-03-25")
    agileout.calendar <- prices.2.calendar(agileout.concat$value_inc_vat, agileout.concat$valid_from, agileout.concat$valid_to,  "2024-01-01", "2025-03-25")
    colnames(agileout.calendar)[2] <- "agile.out_unit"
    
    
    ## Octopus SEG (does not vary w/ time)
    SEG.calendar <- data.frame(win.start = agileout.calendar$win.start, SEG_unit = 4.1)
    #SEG.concat <- price.scraper("OUTGOING-SEG-FIX-12M-BB-23-02-09")
    #SEG.concat <- prices.2.calendar(SEG.concat$value_inc_vat, SEG.concat$valid_from)
    
    ## Flux Export 
    flux.exp.concat <- price.scraper("FLUX-EXPORT-23-02-14", "2024-01-01", "2025-03-25")
    flux.exp.calendar <- prices.2.calendar(flux.exp.concat$value_inc_vat, flux.exp.concat$valid_from, flux.exp.concat$valid_to,  "2024-01-01", "2025-03-25")
    colnames(flux.exp.calendar)[2] <- "flux.out_unit"
    
    ## Intelligent Flux Export
    
    intel.flux.exp.concat <- price.scraper("INTELLI-FLUX-EXPORT-23-07-14", "2024-01-01", "2025-03-25")
    intel.flux.exp.calendar <- prices.2.calendar(intel.flux.exp.concat$value_inc_vat, intel.flux.exp.concat$valid_from, intel.flux.exp.concat$valid_to,  "2024-01-01", "2025-03-25")
    colnames(intel.flux.exp.calendar)[2] <- "intel.flux.out_unit"
  }
  
  # Import tariffs that cover entire of 2024
  {
    ## Agile Octopus
    agile.prices <- prices.process("AGILE-FLEX-22-11-25", "agile.in")
    
    ## Cosy Octopus
    cosy.prices <- prices.process("COSY-22-12-08", "cosy")
    
    ## Flexible PAYG
    flexPAYG.prices <- prices.process("SMART-PREPAY-VAR-22-10-22", "flex.PAYG")
    
    ## PAYG
    PAYG.prices <- prices.process("PREPAY-VAR-18-09-21", "PAYG")
    
    ## Flexible Octopus
    flexoct.prices <- prices.process("VAR-BB-23-04-01", "flex.oct")
    
    ## Octopus Go
    go.prices <- prices.process("GO-VAR-22-10-14", "go")
    
    ## Flux import
    fluximp.prices <- prices.process("FLUX-IMPORT-23-02-14", "flux.import")
    
    ## Intelligent flux import
    intfluximp.prices <- prices.process("INTELLI-FLUX-IMPORT-23-07-14", "intel.flux.import")
  }
  
  ## Consider tariffs that do not necessarily cover the entire 2024 period
  {
    ## Snug octopus
    snug.prices <- prices.process_dates("SNUG-24-11-07", "snug", "2024-11-08", "2025-03-25")
   
    ## Intelligent octopus go
    intelgo.prices <- prices.process_dates("INTELLI-VAR-24-10-29", "intelligent_go", "2024-10-30", "2025-03-25")
    
    ## OEV (Octopus Electric Vehicle) Intelligent octopus go
    oevintelgo.prices <- prices.process_dates("INTELLI-VAR-OEV-24-07-17", "oev_intelligent_go", "2024-07-18", "2025-03-25")
     
  }
}


## Concatenate prices into one dataframe
{
  # Window start times
  all.prices <- as.data.frame(agileout.calendar$win.start)
  colnames(all.prices) <- c("win.start")
  
  ## Extract window details (for matching w/ consumption profiles) from datetime
  {
    ## Dummy variable for weekends
    all.prices$dayofweek <- weekdays(all.prices$win.start)
    all.prices$weekend <- as.integer(all.prices$dayofweek %in% c("Saturday", "Sunday"))
    
    ## Month
    all.prices$month <- month(all.prices$win.start)
    
    ## Time
    {
      all.prices$hour <- hour(all.prices$win.start)
      all.prices$minute <- minute(all.prices$win.start)
      
      all.prices$window <- all.prices$hour + (all.prices$minute / 60)
    }
    
    ## Remove variables just needed for construction of other variables
    all.prices <- all.prices %>% select(win.start, month, window, weekend)
  }
  
  # Append prices from each tariff
  all.prices <- merge(all.prices, 
                      flexoct.prices) 
  all.prices <- merge(all.prices,
                      agile.prices)
  all.prices <- merge(all.prices, 
                      fluximp.prices)
  all.prices <- merge(all.prices, 
                      intfluximp.prices)
  all.prices <- merge(all.prices, 
                      cosy.prices)
  all.prices <- merge(all.prices, 
                      go.prices)
  all.prices <- merge(all.prices, 
                      PAYG.prices)
  all.prices <- merge(all.prices, 
                      flexPAYG.prices)
  all.prices <- merge(all.prices, 
                      SEG.calendar)
  all.prices <- merge(all.prices, 
                      agileout.calendar)
  all.prices <- merge(all.prices,
                      flux.exp.calendar)
  all.prices <- merge(all.prices,
                      intel.flux.exp.calendar)
  
  ## Attach prices for tariffs that did not cover the entire of 2024
  all.prices <- merge(all.prices,
                      snug.prices,
                      by = "win.start", all.x= TRUE)
  
  all.prices <- merge(all.prices,
                      intelgo.prices,
                      by = "win.start", all.x= TRUE)
  
  all.prices <- merge(all.prices,
                      oevintelgo.prices,
                      by = "win.start", all.x= TRUE)
  
  
  
  ## Add fixed tariffs
  {
    ## Source:
    #   https://octopus.energy/tariffs/
    
    ## Octopus 12M Fixed January 2024 v2
    # Standard
    all.prices$oct.fix.jan24_unit <- NA  
    all.prices$oct.fix.jan24_stand <- NA  
    all.prices$oct.fix.jan24_unit[all.prices$win.start < as.POSIXct("2025-01-01 00:00:00")] <- 27.3  
    all.prices$oct.fix.jan24_stand[all.prices$win.start < as.POSIXct("2025-01-01 00:00:00")] <- 56.63 
    
    # Economy 7
    #   For baselining persona 1 against agile/etc...
    all.prices$oct.e7.fix.jan24_unit <- NA  
    all.prices$oct.e7.fix.jan24_stand <- NA  
    all.prices$oct.e7.fix.jan24_unit[all.prices$win.start < as.POSIXct("2025-01-01 00:00:00") & all.prices$window < 7] <- 13.84 
    all.prices$oct.e7.fix.jan24_unit[all.prices$win.start < as.POSIXct("2025-01-01 00:00:00") & all.prices$window > 6.5] <- 33.04  
    all.prices$oct.e7.fix.jan24_stand[all.prices$win.start < as.POSIXct("2025-01-01 00:00:00")] <- 56.72 
    
    ## Octopus 12M Fixed November 2024 v3 
    # Economy 7
    #   For baselining persona 1 against snug octopus
    all.prices$oct.e7.fix.nov24_unit <- NA  
    all.prices$oct.e7.fix.nov24_stand <- NA  
    all.prices$oct.e7.fix.nov24_unit[all.prices$win.start > as.POSIXct("2024-10-31 00:00:00") & all.prices$window < 7] <- 11.96 
    all.prices$oct.e7.fix.nov24_unit[all.prices$win.start > as.POSIXct("2024-10-31 00:00:00") & all.prices$window > 6.5] <- 28.54
    all.prices$oct.e7.fix.nov24_stand[all.prices$win.start > as.POSIXct("2024-10-31 00:00:00")] <- 66.63
    
    ## Octopus 12M Fixed October 2024 v2
    # Standard
    all.prices$oct.fix.oct24_unit <- NA  
    all.prices$oct.fix.oct24_stand <- NA  
    all.prices$oct.fix.oct24_unit[all.prices$win.start > as.POSIXct("2024-10-01 00:00:00")] <- 22.81  
    all.prices$oct.fix.oct24_stand[all.prices$win.start > as.POSIXct("2024-10-01 00:00:00")] <- 66.07 
    
    ## Octopus 12M Fixed July 2024 v1
    # Standard
    all.prices$oct.fix.jul24_unit <- NA  
    all.prices$oct.fix.jul24_stand <- NA  
    all.prices$oct.fix.jul24_unit[all.prices$win.start > as.POSIXct("2024-07-01 00:00:00")] <- 22.43  
    all.prices$oct.fix.jul24_stand[all.prices$win.start > as.POSIXct("2024-07-01 00:00:00")] <- 65.87 
    
  }
  
  
  
}

## Save prices to csv
{
  write.csv(all.prices, "./in_clean/all_prices.csv", row.names = FALSE)
  
}



