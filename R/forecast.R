#' Forecasting yield curves
#' @param estimObj yield curve model estimated by EstimYieldCurve
#' @param nhor forecast horizon
#' @param ylddata additional data to compute forecast errors. If NULL, no forecast errors are computed. If not NULL the user has to provide the maturity of the yields. Users should provide ylddata for the full forecast horizon.
#' @param maturity maturity for forecasted yields. If not provided maturity data from model will be used. Not optional if ylddata is not NULL.
#' @export
forecast.yldcurve <- function(estimObj,nhor=12,ylddata = NULL,maturity = NULL){

  # ------- Start Input Check ----------

  # check if ylddata and maturities are provided
  datinput = 0
  if(!is.null(ylddata)) datinput = datinput + 1
  if(!is.null(maturity)) datinput = datinput + 2

  # only ylddata provided
  if(datinput == 1){

    stop("Please provide maturities in addition to providing additional ylddata")

  }

  # check if forecast horizon and length of time series are the same
  if(datinput == 3){

    yldrows <- nrow(ylddata)

    if(yldrows < nhor){

      warning("ylddata series shorter than forecast horizon. Will use length of ylddata series as the forecast horizon\n")
      nhor <- yldrows

    }

  }



  # check if number of maturities are the same as the number of yields
  if(datinput >= 2){

    yldcols <- ncol(ylddata)
    matcols <- length(maturity)

    if(yldcols != matcols){

      stop("Number of maturities provided in maturity does not equal the number of maturities in ylddata.")

    }

  }

  # ----------- Input Check end -----------------

  # ----------- Preliminary work: get relevant info for easier reading and declare storage variables -------------

  # get data

  fdata    <- estimObj$model$factors # get factor series
  nfactors <- estimObj$model$nfactors

  if(datinput == 0){

    maturity <- estimObj$model$maturity
    matcols  <- length(maturity)

  }

  nObs <- nrow(fdata)
  fstart <- matrix(fdata[nObs,],nrow=1) # get last factor

  mPhi <- estimObj$varmodel$mPhi[1:nfactors,1:nfactors] # get VAR coefficients

  # Declare matrix for forecasted factor data
  fc_factors <- array(NA,dim=c(nhor,nfactors))
  fc_ylds    <- array(NA,dim=c(nhor,matcols))
  msfe       <- array(NA,dim=c(nhor,1))

  # predict factor movement

  for(ii in 1:nhor){

    new_factors <- fstart %*% mPhi
    fstart <- new_factors
    fc_factors[ii,] <- fstart # save factors

  }

  # map factors into yields
  for(ii in 1:nhor){

    if( estimObj$yldmodel == "DNS"){

      # Dynamic Nelson-Siegel
      fc_ylds[ii,] <- map_yields_dns(fc_factors[ii,],maturity,estimObj$model$lambda)

    }
    else if( estimObj$yldmodel == "DSS"){

      # Dynamic Söderlin-Svensson
      fc_ylds[ii,] <- map_yields_dss(fc_factors[ii,],maturity,estimObj$model$lambda[1],estimObj$model$lambda[2])

    }

  }

  # Calculate Forecast Errors
  for(ii in 1:nhor){

    fc_error <- (fc_ylds[ii,] - ylddata[ii,])^2
    msfe[ii,] <- sum(fc_error)

  }

  retlist <- list(fc_factors = fc_factors,
                  fc_yields = fc_ylds,
                  MSFE = msfe)

  return(retlist)

}
