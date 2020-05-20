# Estimate Dynamic Nelson-Siegel Model
# @param yields ts-object with yields
# @param maturity vector with maturities in years
# @param lambda parameter in Nelson-Siegel
# @param frequency frequency of the data (12 = monthly, 52 = weekly, 360 = daily)

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
    ylderror[ii,]    <-  yldfit[ii,] - ylds_tmp
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
    frequency = frequency,
    lambda = lambda
  )


 return(retlist)
}

# helper function for Dynamic Nelson-Siegel Model
# @param maturity vector with maturities in months
# @param lambda the lambda

dns_helper <- function(maturity,lambda){

  b1 <- 1
  b2 <- (1-exp(- lambda * maturity)) / (lambda/maturity)
  b3 <- b2-exp(- maturity * lambda)
  return(list(b1=b1,b2=b2,b3=b3))

}

# Find lambda that minimizes the sum of squared errors using a grid search
# @param yields txn-panel with yields
# @param maturity vector with maturities in months

find_lambda <- function(yields,maturity,frequency = 12){

  tmp <- optimx::optimx(par = 0.05, fn = dns_sse, gr = NULL, hess = NULL, lower = 0.0001, upper = 0.9,
                        method = c("L-BFGS-B"),itnmax = NULL, control = list(),
                        yields = yields, maturity = maturity, frequency = frequency)

  return(tmp$p1)

}

dns_sse <- function(x,yields,maturity,frequency){

  tmp <- estimDNS(yields = yields, maturity = maturity, lambda = x, frequency = frequency)
  return(tmp$SSE)

}

# map factors into yields for a given set of maturities
# @param factors factors
# @param maturities maturities
# @param lambda decay factor


map_yields_dns <- function(factors,maturity,lambda){

  tmp <- dns_helper(maturity,lambda)
  x1  <- cbind(tmp$b1,tmp$b2,tmp$b3)
  ylds <- x1 %*% factors

  return(ylds)

}

