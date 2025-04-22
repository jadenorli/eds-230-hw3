#' Compute Almond Profit from Almond Yield Anomaly 
#'
#' @param climate_data dataframe that contains day, month, year, daily minimum temperature (Celsius), and daily precipitation (mm)
#' @param tmin_month the numerical month corresponding to the month that Lobell et al 2006 denoted for the mean minimum temperature in the transfer equation 
#' @param precip_month the numerical month corresponding to the month that Lobell et al 2006 denoted for the total precipitation in the transfer equation 
#' @param baseline_yield the baseline annual almond yield (lb/acre)
#' @param baseline_price the baseline almond price (US $/lb)
#' @author Jaden Orli
#' @return profit_df dataframe which contains year, annual yield anomaly (ton/acre), actual yield (ton/acre), and profit (US $/acre)

#write a function to calculate the annual profit based on the actual yield calculated from the Lobell et al 2006 equation 
almond_yield_profit <- function(climate_df, tmin_month, precip_month, baseline_yield, baseline_price) {
  
  #use the almond_yield function to calculate the annual almond yield anomaly (ton/acre)
  yield_df <- almond_yield(climate_df, tmin_month, precip_month) %>% #calculate the anomaly for the given years
    select(year, yield_anomaly) #only keep the year and anomaly column
  
  #convert the baseline yield from pounds/acre to ton/acre
  baseline_yield_tn <- baseline_yield*0.0005
  
  #convert the baseline price from US $/lb to US $/ton 
  baseline_price_tn <- baseline_price/0.0005
  
  #start with the yield dataframe which contains annual almond yields
  profit_df <- yield_df %>% #start with the yield dataframe input 
    mutate(actual_yield = baseline_yield_tn + yield_anomaly, #add a column with the actual yield (baseline yield + yield anomaly)
           profit = actual_yield*baseline_price_tn)  #add a final column with the profit from the actual yield and adjusted price
  
  #return the profit_df with annual yield anomaly, actual yield, and profit
  return(profit_df)
}
