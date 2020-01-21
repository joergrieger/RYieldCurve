#' Estimate Dynamic Nelson-Siegel Model
#' @param yields ts-object with yields
#' @param maturity vector with maturities in years
#' @param lambda parameter in Nelson-Siegel
#' @param frequency frequency of the data (12 = monthly, 52 = weekly, 360 = daily)
#' @export

estimDNS <- function(yields,maturity,lambda,frequency = 12){

  x        <- dns_helper(maturity,lambda)
  x1       <- cbind(x$b1,x$b2,x$b3)
  nl       <- dim(yields)[1]
  beta     <- array(NA,dim=c(nl,3))
  yldfit   <- array(NA,dim=c(nl,length(maturity)))
  ylderror <- array(NA,dim=c(nl,length(maturity)))
  SSE      <- 0

  # Estimate the factors

  for(ii in 1:nl){

    ylds_tmp    <- as.matrix(yields[ii,],ncol=1)
    beta[ii,]   <- solve( t(x1) %*% x1 ) %*% t(x1) %*% (ylds_tmp)
    yldfit[ii,] <- t(x1 %*% beta[ii,])
    ylderror[ii,]    <- ylds_tmp - yldfit[ii,]
    SSE <- SSE + sum(ylderror[ii,]^2)

  }

  # return results
  retlist <- list(
    factors = beta,
    ylderror = ylderror,
    yldfit = yldfit,
    SSE = SSE,
    nfactors = 3,
    rho = matrix(c(1,1,0),ncol=1),
    maturity = maturity,
    frequency = frequency
  )


 return(retlist)
}

#' helper function for Dynamic Nelson-Siegel Model
#' @param maturity vector with maturities in months
#' @param lambda the lambda

dns_helper <- function(maturity,lambda){

  b1 <- 1
  b2 <- (1-exp(- lambda * maturity)) / (lambda/maturity)
  b3 <- b2-exp(- maturity * lambda)
  return(list(b1=b1,b2=b2,b3=b3))

}

#' Find lambda that minimizes the sum of squared errors using a grid search
#' @param yields txn-panel with yields
#' @param maturity vector with maturities in months

find_lambda <- function(yields,maturity){
  SSE <- array(NA,dim=c(1000))
  lambdaGrid <- seq(1:300)/1000
  for(ii in 1:1000){
    restemp <- estimDNS(yields,maturity,lambdaGrid[ii])
    SSE[ii] <- restemp$SSE
  }
  indx <- which.min(SSE)
  minlambda <- lambdaGrid[indx]
  return(minlambda)

}

