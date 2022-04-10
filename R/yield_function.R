## Function to caluclate the yield of perennial crops
## takes coefficient inputs from Lobell et al. 2006 (crop growth) and clim data
## outputs yield anomoly for year of clim data
## default coefficients are for almonds
## @param: clim_temp_max; maximum temp deg C; month varies per Lobell
## @param: clim_temp_min; minimum temp deg C; month varies per Lobell
## @param: precip; precipitation in mm
## @param: temp_max_coeff1; per Lobell (2006)
## @param: temp_max_coeff2; per Lobell (2006)
## @param: temp_min_coeff1; per Lobell (2006)
## @param: temp_min_coeff2; per Lobell (2006)
## @param: precip_coeff1; per Lobell (2006)
## @param: precip_coeff1; per Lobell (2006)
## @param: intercept; per Lobell (2006)

yield_anomaly <- function(clim_temp_min,
                   clim_precip,
                   temp_max_coeff1 = 0,
                   temp_max_coeff2 = 0,
                   temp_min_coeff1 = -0.015,
                   temp_min_coeff2 = -0.0046,
                   precip_coeff1 = -0.07,
                   precip_coeff2 = 0.0043,
                   intercept = 0.28)
{
  
  # yield function
  yield = 
    (temp_max_coeff1 * clim_temp_max) + 
    (temp_max_coeff2 * clim_temp_max**2) + 
    (temp_min_coeff1 * clim_temp_min) + 
    (temp_min_coeff2 * clim_temp_min**2) + 
    (precip_coeff1 * precip) + 
    (precip_coeff2 * precip**2) + 
    intercept
  
  return(yield)
  
  
}