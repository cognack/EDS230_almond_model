


#' Almond Yield Anomaly Model
#' Function to calculate the yield (tons/acre) of perennial crops
#' takes coefficient inputs from Lobell et al. 2006 (crop growth) and climate data
#' outputs profit from almond sales for each year of climate data
#' note that default values are for almonds, would need to add to model for crops that include max temps as predictor
#'
#' @param climate_data dataframe, climate data input with temperature (C) and precipitation (mm) 
#' @param temp_min_month factorial number month where average temp (C)is min for model (per Lobell, 2006)
#' @param precip_month factorial number month where precipitation (mm) is predictor of yield (per Lobell, 2006)
#' @param a first coefficient (minimum temperature; per Lobell, 2006) 
#' @param b second coefficient (minimum temperature; per Lobell, 2006)
#' @param c third coefficient (precipitation; per Lobell, 2006)
#' @param d fourth coefficient (precipitation; per Lobell, 2006)
#' @param intercept intercept (per Lobell, 2006)
#' @param avg_yield average yield per year in tons
#' @param avg_cost average cost in dollars per ton
#' @param acres_used acres of crop
#' 
#'
#' @return profit form almond sales for each year of climate data ($)
#' @export
#'
#' @examples
almond_profit = function(climate_data, temp_min_month = 2, precip_month = 1, 
                             a = -0.015, b = -0.0046, 
                             c = -0.07, d = 0.0043, intercept = 0.28,
                             avg_yield = 1500000, avg_cost = 4000, 
                             acres_used = 1600000) {
  
  # manipulation of inputs to create a dataframe of minimum temperatures
  tn_2_values <- climate_data %>% 
    filter(month == temp_min_month) %>% 
    group_by(year) %>% 
    summarise(tn_2 = mean(tmin_c))
  
  # manipulation of inputs to create a dataframe of precipitation values
  p_1_values <- climate_data %>% 
    filter(month == precip_month) %>% 
    group_by(year) %>% 
    summarise(p_1 = sum(precip))
  
  # extract years included in data
  year_range <- seq(from = min(tn_2_values$year), to = max(tn_2_values$year), by = 1)
  
  # create empty vector for model output
  almond_profit <- vector(mode = 'numeric', length = length(year_range))
  
  almond_yield_tons_per_acre <- vector(mode = 'numeric', length = length(year_range))
  
  almond_profit_anom <- vector(mode = 'numeric', length = length(year_range))
  
  # calculate almond yield anomaly
  
  for (i in 1:length(year_range)) {
    tn_2 <- tn_2_values$tn_2[tn_2_values$year == year_range[i]]
    p_1 <- p_1_values$p_1[p_1_values$year == year_range[i]]
    anom_calc_tons_per_acre <- (a * tn_2) + (b * tn_2 ^ 2) + (c * p_1) + (d * p_1 ^ 2) + intercept
    yield <- anom_calc_tons_per_acre + avg_yield 
    profit_anom_calc <- anom_calc_tons_per_acre * avg_cost * acres_used
    profit_calc <- (yield) * avg_cost * acres_used
    
    
    almond_profit[i] <- profit_calc
    almond_yield_tons_per_acre[i] <- yield
    almond_profit_anom[i] <- profit_anom_calc

  }
  # create dataframe of model output
  results <- data.frame(year_range, almond_profit, almond_profit_anom,
                        almond_yield_tons_per_acre,
                        tn_2_values$tn_2, p_1_values$p_1) %>% 
    rename(year = year_range) %>% 
    rename(tn_2_values = tn_2_values.tn_2) %>% 
    rename(p_1_values = p_1_values.p_1)
}
