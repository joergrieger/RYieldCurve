# Dynamic Svensson-Söderlind Model
# @param yields Txn series of yields
# @param maturity vector of length n of maturity dates
# @param lambda1,lambda2 decay parameters
# @param frequency frequency of the data
# @importFrom pracma mldivide
estimDSS <- function(yields,maturity,lambda1,lambda2,frequency=12){

  x        <- dss_helper(maturity,lambda1,lambda2)
  x1       <- cbind(x$b1,x$b2,x$b3,x$b4)
  nl       <- dim(yields)[1]
  beta     <- array(NA,dim=c(nl,4))
  yldfit   <- array(NA,dim=c(nl,length(maturity)))
  ylderror <- array(NA,dim=c(nl,length(maturity)))
  SSE      <- 0

  # Estimate the factors

  for(ii in 1:nl){

    ylds_tmp    <- as.matrix(yields[ii,],ncol=1)
    #beta[ii,]   <- solve( t(x1) %*% x1 ) %*% t(x1) %*% (ylds_tmp)
    beta[ii,] <- pracma::mldivide(x1,ylds_tmp)
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
    nfactors = 4,
    rho = matrix(c(1,1,0,0),ncol=1),
    maturity = maturity,
    frequency = frequency,
    lambda = c(lambda1,lambda2)
  )

  return(retlist)
}

dss_helper <- function(maturity,lambda1,lambda2){

    b1 <- 1
    b2 <- (1-exp(- lambda1 * maturity)) / (lambda1/maturity)
    b3 <- b2-exp(- maturity * lambda1)
    b4 <- (1-exp(- lambda2 * maturity)) / (lambda2/maturity)
    b5 <- b4-exp(- maturity * lambda2)
    return(list(b1=b1,b2=b2,b3=b3,b4=b5))

}

find_dss_lambda <- function(maturity,yields,frequency = 12){

  tmp <- optimx::optimx(par = c(0.25,0.6), fn = dss_sse, gr = NULL, hess = NULL,
                        lower = c(0.0001,0.0001), upper = c(0.9,0.9),
                        method = c("L-BFGS-B"),itnmax = NULL, control = list(),
                        yields = yields, maturity = maturity, frequency = frequency)

  return(list(lambda1 = tmp$p1, lambda2 = tmp$p2))

}

dss_sse <- function(x,yields,maturity,frequency){

  tmp <- estimDSS(yields = yields, maturity = maturity, lambda1 = x[1], lambda2 = x[2], frequency = frequency)
  return(tmp$SSE)

}

# map factors into ylds for Söderlind-Svensson
# @param factors factors
# @param maturity maturities
# @param lambda1,lambda2 decay factors

map_yields_dss <- function(factors,maturity,lambda1,lambda2){

  tmp <- dss_helper(maturity,lambda1,lambda2)
  x1  <- cbind(tmp$b1,tmp$b2,tmp$b3,tmp$b4)

  ylds <- x1 %*% factors

  return(ylds)

}
