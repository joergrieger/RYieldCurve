#' Generic method for forecasting
#' @param estimObj yield curve model estimated by EstimYieldCurve
#' @param nhor forecast horizon
#' @param ylddata additional data to compute forecast errors. If NULL, no forecast errors are computed. If not NULL the user has to provide the maturity of the yields. Users should provide ylddata for the full forecast horizon.
#' @param maturity maturity for forecasted yields. If not provided maturity data from model will be used. Not optional if ylddata is not NULL.
#' @export

forecast <- function(estimObj,nhor=12,ylddata = NULL,maturity = NULL) UseMethod("forecast")
