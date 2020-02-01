# Calculate Term Premia
# @param objVar estimate varm-model
# @param objModel estimated Model
# @export
#
TermPremium <- function(objVar, objModel){

  # Declare Variables
  ndim <- dim(objVar$x)
  nf   <- objModel$nfactors
  rho  <- objModel$rho
  nObs <- dim(objVar$y)[1]
  nmaturity <- length(objModel$maturity)
  id <- diag(1,ndim[1])

  # Declare variables
  term_premium <- array(NA,dim=c(nObs,nmaturity))
  risk_free <- array(NA,dim=c(nObs,nmaturity))

  # Eigenvalue decomposition of VAR-coefficients
  eig <- eigen(t(objVar$mPhi))
  eigen_values <- diag(eig$values)
  eigen_vectors <- eig$vectors

  for(tt in 1:nObs){

    for(ii in 1:nmaturity){

      n = objModel$maturity[ii]*objModel$frequency/12

      # Calculate risk-free interest rate
      risk_free[tt,ii] <- Expr(V = eigen_vectors[1:nf,1:nf],
                               D = eigen_values[1:nf,1:nf],
                               n = n,
                               X = objVar$x[tt,1:nf],
                               mp = objVar$mp,
                               rho = objModel$rho,
                               I = id[1:nf,1:nf])

      # Calculate Term-Premium
      term_premium[tt,ii] <- objModel$yldfit[tt,ii] - risk_free[tt,ii]

    }
  }

  # Return results
  retlist <- structure(list(risk_free = risk_free,
                            term_premium = term_premium),
                       class = "premium")
  return(retlist)
}

# Function to calculate the risk-free rate (Expectations component)
# @param V eigenvectors of VAR-coefficient
# @param D eigenvalues of VAR-coefficient
# @param n maturity in months
# @param X current yield factors
# @param mp mean value of factors
# @param rho helper
# @param I identity matrix
Expr <- function(V,D,n,X,mp,rho,I){

  # Declare used functions
  `%^%` <- expm::`%^%`

  trho <- t(X) %*% rho
  mphin <- V %*% (D %^% n) %*% solve(V)
  mphi  <- V %*% D %*% solve(V)
  mphii <- solve(V %*% D %*% solve(V) - I)

  mx <- t( (n - 1) * mp + (( mphin - mphi ) %*% mphii) %*% (X - mp)) %*% rho

  ret <- 1/n * ( trho + mx)
  return(ret)

}

