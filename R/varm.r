# Estimate a VAR(1)-Model
# The function varm estimates the following VAR(1)-Model:
# Y_t = mp + beta * (Y_{t-1} - mp) +e_t
# @param y data
# @param exo exogeneous variables

varm <- function(y,exo=NULL){

  if(is.null(exo)){

    yt = y

  }
  else{

    yt = cbind(y,exo)

  }
  ndim <- dim(yt)
  nObs <- ndim[1]
  xt   <- yt

  mp <- colMeans(yt)

  for(ii in 1:nObs){

    xt[ii,] <- xt[ii,] - mp

  }

  # Estimate the parameters
  x1 <- xt[1:(nObs-1),]
  y1 <- xt[2:nObs,]
  beta <- pracma::mldivide(x1,y1)

  # return everything

  ret <- structure(list(mPhi = beta,
                        mp = mp,
                        y = y,
                        exo = exo,
                        x = xt), class = "varm")

  return(ret)
}
