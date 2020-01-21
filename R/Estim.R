#' Estimate Yield Curve Models
#' @description This function estimates yield curve models and computes term premia. Users have the choice between several yield curve models such as the Dynamic-Nelson-Siegel model introduced by (), the Dynamic Svensson-Söderlind model or Joslin-Singleton-Zhu-model. For the DNS and DSS model the parameter lambda is automatically computed using a grid search.
#' @param yields A Txn panel of yields. Has to be a ts-objects
#' @param exogen Exogeneous Variables for the VAR-Model
#' @param maturity An nx1-vector of maturities.
#' @param frequency data frequency (12=monthly, 52 = weekly, 360 = daily)
#' @param method The method which is used to extract the factors. (DNS = Dynamic Nelson Siegel)
#' @export

EstimYieldCurve <- function(yields,exogen = NULL,maturity, frequency = 12, method="DNS"){

  # Check input


  # Estimate factors
  if(toupper(method) == "DNS"){
    # Use Dynamic Nelson - Siegel Factors
    cat("Estimating yield curve factors using Dynamic Nelson-Siegel Model\n")

    # Get optimal Lambda
    cat("Finding optimal lambda\n")
    lambda <- find_lambda(yields,maturity)

    # estimate the factors
    estim_model <- estimDNS(yields,maturity,lambda)
    print(estim_model$rho)

    # Estimate the VAR(1) - Model
    cat("Estimating VAR(1) model\n")
    model_var <- varm(y = estim_model$factors, exo = exogen)

    # Estimate the Termpremium
    cat("Estimating Term-Premium\n")
    TP <- TermPremium(objVar = model_var, objModel = estim_model)

    retlist <- structure(list(yldmodel = "DNS",
                              model = estim_model,
                              varmodel = model_var,
                              term_premia = TP),
                         class = "yldcurve")

  }

  return(retlist)

}

