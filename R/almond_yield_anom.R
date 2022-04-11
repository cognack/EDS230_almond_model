


#' Almond Yield Anomaly Model 1
#' Function to caluclate the yield of perennial crops
#' takes coefficient inputs from Lobell et al. 2006 (crop growth) and clim data
#' outputs yield anomoly for each year of clim data
#' note that default values are for almonds, would need to add to model for crops that include max temps as predictor
#'
#' @param climate_data dataframe, climate data input with temperature (C) and precipitation (mm) 
#' @param temp_min_month factoral number month where average temp (C)is min for model (per Lobell, 2006)
#' @param precip_month factoral number month where precip (mm) is predictor of yield (per Lobell, 2006)
#' @param a first coefficient (min temp; per Lobell, 2006) 
#' @param b second coefficient (min temp; per Lobell, 2006)
#' @param c third coefficient (precip; per Lobell, 2006)
#' @param d fourth coefficent (precip; per Lobell, 2006)
#' @param intercept intercept (per Lobell, 2006)
#'
#' @return
#' @export
#'
#' @examples
almond_yield_anom = function(climate_data, temp_min_month = 2, precip_month = 1, a = -0.015, b = -0.0046, c = -0.07, d = 0.0043, intercept = 0.28) {

  # manipulation of inputs
  tn_2_values <- climate_data %>% 
    filter(month == temp_min_month) %>% 
    group_by(year) %>% 
    summarise(tn_2 = mean(tmin_c))
  
  p_1_values <- climate_data %>% 
    filter(month == precip_month) %>% 
    group_by(year) %>% 
    summarise(p_1 = sum(precip))
  
  year_range <- seq(from = min(tn_2_values$year), to = max(tn_2_values$year), by = 1)
  
  almond_yield_anom_tons_per_acre <- vector(mode = 'numeric', length = length(year_range))
  
  # calculate almond yield anomaly
  
  for (i in 1:length(year_range)) {
    tn_2 <- tn_2_values$tn_2[tn_2_values$year == year_range[i]]
    p_1 <- p_1_values$p_1[p_1_values$year == year_range[i]]
    anom_calc <- (a * tn_2) + (b * tn_2 ^ 2) + (c * p_1) + (d * p_1 ^ 2) + intercept
    almond_yield_anom_tons_per_acre[i] <- anom_calc
  }
  results <- data.frame(year_range, almond_yield_anom_tons_per_acre, tn_2_values$tn_2, p_1_values$p_1) %>% 
    rename(year = year_range) %>% 
    rename(tn_2_values = tn_2_values.tn_2) %>% 
    rename(p_1_values = p_1_values.p_1)
}
