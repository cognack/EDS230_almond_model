


#' Almond Yield Anomaly Model 1
#'
#' @param climate_data 
#' @param a 
#' @param b 
#' @param c 
#' @param d 
#' @param intercept 
#'
#' @return
#' @export
#'
#' @examples
almond_yield_anom = function(climate_data, a = -0.015, b = -0.0046, c = -0.07, d = 0.0043, intercept = 0.28) {

  # manipulation of inputs
  tn_2_values <- climate_data %>% 
    filter(month == 2) %>% 
    group_by(year) %>% 
    summarise(tn_2 = mean(tmin_c))
  
  p_1_values <- climate_data %>% 
    filter(month == 1) %>% 
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
