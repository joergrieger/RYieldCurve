#' Estimate Yield Curve Models
#' @description This function estimates yield curve models and computes term premia. Users have the choice between several yield curve models such as the Dynamic-Nelson-Siegel model introduced by (), the Dynamic Svensson-Söderlind model or Joslin-Singleton-Zhu-model. For the DNS and DSS model the decay factors is computed using a grid search over the range [0.001,0.300]. The program choses the decay factors that minimizes the sum of squared errors between model yields and yields supplied by the user. Alternatively the user can provide decay factor(s).
#' @param yields A Txn panel of yields. Has to be a ts-objects
#' @param exogen Exogeneous Variables for the VAR-Model
#' @param maturity An nx1-vector of maturities.
#' @param frequency data frequency (12=monthly, 52 = weekly, 360 = daily)
#' @param method The method which is used to extract the factors. (DNS = Dynamic Nelson-Siegel, DSS = Dynamic Söderlind-Svensson Model)
#' @param lambda Decay factor(s). If decay factors are not provided by the user the code will automatically search for the optimal decay factors.
#' @export

EstimYieldCurve <- function(yields,exogen = NULL,maturity, frequency = 12, method="DNS",lambda=NULL){

  # Check input
  if(toupper(method) == "DNS"){
    # check for correct lambda in the DNS-Model
    if(is.null(lambda) | length(lambda) == 1 ){

      inputcheck = TRUE

    }
    else{

      stop("For the Dynamic Nelson-Siegel Model please supply only one value or NULL for the decay factor lambda.")

    }
  }
  else if(toupper(method) == "DSS"){

    # Check for correct lambda in the DSS-Model
    if(is.null(lambda) | length(lambda) == 2){

      inputcheck = TRUE

    }
    else{

      stop("For the Dynamic Soederlind-Svensson Model please supply two values or NULL for the decay factor lambda.")

    }

  }


  # Estimate factors
  if(toupper(method) == "DNS"){
    # Use Dynamic Nelson - Siegel Factors
    cat("Estimating yield curve factors using Dynamic Nelson-Siegel Model\n")

    # Get optimal Lambda
    if(is.null(lambda)){

      cat("Finding optimal lambda\n")
      lambda <- find_lambda(yields = yields, maturity = maturity, frequency = frequency)

    }

    # estimate the factors
    estim_model <- estimDNS(yields,maturity,lambda)

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
  else if(toupper(method) == "DSS"){
    # Use Dynamic Söderlind-Svensson model
    cat("Estimating yield curve factors using Dynamic Soederlind-Svensson model\n")

    # Get optimal lambdas
    if(is.null(lambda)){

      cat("Finding optimal decay factors\n")
      lambda <- find_dss_lambda(yields = yields,maturity = maturity, frequency = frequency)
      lambda1 = lambda$lambda1
      lambda2 = lambda$lambda2

    }
    else{

      lambda1 = lambda[1]
      lambda2 = lambda[2]


    }

    # Estimate factors
    estim_model <- estimDSS(yields=yields,maturity=maturity,lambda1 =  lambda1,lambda2 = lambda2,frequency=frequency)

    # Estimate VAR(1) - model
    cat("Estimating VAR(1) model\n")
    model_var <- varm(y=estim_model$factors,exo=exogen)

    # Estimate Term Premium
    cat("Estimating Term Premia\n")
    TP <- TermPremium(objVar = model_var, objModel = estim_model)

    retlist <- structure(list(yldmodel = "DSS",
                              model = estim_model,
                              varmodel = model_var,
                              term_premia = TP),
                         class = "yldcurve")
  }

  return(retlist)

}

