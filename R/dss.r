#' Dynamic Svensson-Söderlind Model
#' @param yields Txn series of yields
#' @param maturity vector of length n of maturity dates
#' @param lambda1,lambda2 decay parameters
#' @param frequency frequency of the data
#' @export
#' @importFrom pracma mldivide
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

find_dss_lambda <- function(maturity,yields){

  sse <- array(NA,dim=c(300,300))
  lambdagrid <- seq(1:300)/1000

  for(ii in 1:300){
    for(jj in 1:300){

      restemp <- estimDSS(yields = yields, maturity = maturity,lambda1 = lambdagrid[ii], lambda2=lambdagrid[jj])
      sse[ii,jj] <- restemp$SSE

    }
  }

  lmin <- which(sse == min(sse),arr.ind=TRUE)
  lambda1 = lambdagrid[lmin[1]]
  lambda2 = lambdagrid[lmin[2]]
  return(list(lambda1=lambda1,lambda2=lambda2))
}
