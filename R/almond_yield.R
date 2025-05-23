#' Almond Yield Anomaly 
#' 
#' computes the yearly almond yield anomaly given daily timeseries of daily minimum temperature and daily precipitation
#' @param climate_data dataframe that contains day, month, year, daily minimum temperature (Celsius), and daily precipitation (mm)
#' @param tmin_month the numerical month corresponding to the month that Lobell et al 2006 denoted for the mean minimum temperature in the transfer equation 
#' @param precip_month the numerical month corresponding to the month that Lobell et al 2006 denoted for the total precipitation in the transfer equation 
#' @author Jaden Orli
#' @return annual_yield dataframe which contains the year and annual yield anomaly (ton/acre)
#' @return return_summary will return a summary dataframe with maximum yield anomaly (ton/acre), mean yield anaomaly (ton/acre), and minimum yield anomaly (ton/acre)

#write a function to calculate the annual almond yield anomaly 
almond_yield <- function(climate_df, tmin_month, precip_month, return_summary = NULL){
  
  #create a dataframe with the monthly mean minimum temperature and monthly total precipitation 
  climate_month <- climate_df %>% #start with the unprocessed climate dataframe
    group_by(month, year) %>% #group the dataframe by month and year
    summarise(tmin_mean = mean(tmin_c), #calculate the monthly mean min temp
              precip_sum = sum(precip), #calculate the monthly mean precip 
              .groups = "drop") #ungroup the dataframe 
  
  #create a dataframe for the monthly mean minimum temperatures from each year for the selected month (the tmin_month)
  tmin_month_df <- climate_month %>% 
    filter(month == tmin_month) %>% 
    select(year, tmin_mean) 
  
  #create a dataframe for the total monthly precipitation from each year for the selected month (the precip_month)
  precip_month_df <- climate_month %>% 
    filter(month == precip_month) %>% 
    select(year, precip_sum)
  
  #combine the dataframes with the yearly means into one dataframe
  climate_input <- inner_join(tmin_month_df, precip_month_df,
                              by = "year") #join by year
  
  
  #write a function to calculate the almond yield 
  almond_yield_func <- function(tmin, precip) {
    
    #define the function from Lobell 2006 
    yield <- -0.015*tmin - 0.0046*(tmin)^2 - 0.07*precip + 0.0043*(precip)^2 + 0.28
    
    #return the almond yield
    return(yield)
    
  }
  
  #create a dataframe with the almond yield in a new column 
  annual_yield <- climate_input %>% #start with the climate_input df
    mutate(yield_anomaly = almond_yield_func(tmin_mean, precip_sum)) #use the almond yield function to calculate the yield anomaly and save it as a new column in df
  
  #if return_summary = TRUE, 
  if(!is.null(return_summary)) {
    
    #then symmary this annual yield and return that as a df
    yield_summary <- annual_yield %>%  #start with the annual_yield df 
      summarise(mean_yield = mean(yield_anomaly), #calculate the mean yield anomaly
                max_yield = max(yield_anomaly), #calculate the maximum yield anomaly
                min_yield = min(yield_anomaly)) #calculate the minimum yield anomaly 
    
    #then return the yield_summmary dataframe with the yield anomaly summary
    return(yield_summary)
    
    #if return_summary = FALSE,  
  } else {
    
    #then return the annual_yield dataframe with the yield anomaly 
    return(annual_yield)
  }
}

