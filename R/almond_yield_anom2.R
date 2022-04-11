


#' Almond Yield Anomaly Model 2
#' made as a potential model example; went with other model
#'
#' @param temp_min_feb 
#' @param precip_total_jan 
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
almond_yield_anom2 = function(temp_min_feb, precip_total_jan, a = -0.015, b = -0.0046, c = -0.07, d = 0.0043, intercept = 0.28) {
  
  # calculate almond yield anomaly
  anom_calc <- (a * temp_min_feb) + (b * temp_min_feb ^ 2) + (c * precip_total_jan) + (d * precip_total_jan ^ 2) + intercept
  
  return(anom_calc)
}
