#' @title plots estimated factor series
#' @param estimObj an estimated yield curve model
#' @param ... not used
#' @rdname plot_factors
#' @export
plot_factors <- function(estimObj,...) UseMethod("plot_factors")

#' @export
#' @rdname plot_factors
#'
plot_factors.yldcurve <- function(estimObj,...){

  factor_series <- estimObj$model$factors
  if(estimObj$yldmodel == "DNS"){nfactors = 3}
  if(estimObj$yldmodel == "DSS"){nfactors = 4}

  nlength <- dim(factor_series)[1]
  nseq <- 1:nlength

  pltlist <- list()
  if(estimObj$yldmodel == "DNS"){

    factornames <- c("Level","Slope","Curvature")

  }
  else if(estimObj$yldmodel == "DSS"){

    factornames <- c("Level","Slope","Curvature","Curvature 2")

  }

  for(ii in 1:nfactors){
    dffactors <- data.frame(factors = factor_series[,ii], stime = nseq)
    p1 <- ggplot2::ggplot(data=dffactors) + ggplot2::geom_line(mapping = ggplot2::aes_(x=~stime,y=~factors))
    p1 <- p1 + ggplot2::ylab(factornames[ii])
    if(ii == 1){
      p1 <- p1 + ggplot2::ggtitle("Estimated factors")
    }
    if(ii == nfactors){

      p1 <- p1 + ggplot2::xlab("Date")

    }
    else {

      p1 <- p1 + ggplot2::xlab(ggplot2::element_blank())

    }

    # title for each graph

    # Save graph
    pltlist[[ii]] <- p1

  }

  do.call("grid.arrange",c(pltlist,nrow=nfactors))

}

#' @title plot fitted yield series
#' @param estimObj an estimated yield curve model
#' @param ... not used
#' @rdname plot_fitted
#' @export
plot_fitted <- function(estimObj,...) UseMethod("plot_fitted")


#' @rdname plot_fitted
#' @param maturities the maturities that should be plotted. The array refers to the columns of the maturities and the maximum number of maturities that can be plotted at the same time is 5. There will be a warning if the user wants to plot more than 5 series and the function uses only the first 5 elements in the array.
#' @export
plot_fitted.yldcurve <- function(estimObj,maturities,...){

  # Check for correct input

  if(length(maturities) > 5){
    warning("The maximum number of maturities that can be plotted is limited to 5. Only the first 5 maturities will be plotted")
    maturities <- maturities[1:5]
  }

  nmax <- dim(estimObj$model$yldfit)[2]
  if(max(maturities)>nmax){

   stop("Highest maturity not available.")

  }

  # define variables
  yldfit <- estimObj$model$yldfit
  nlength <- dim(yldfit)[1]
  nseq <- 1:nlength

  pltlist <- list()

  # prepare for y axis
  yldmaturities <- estimObj$model$maturity
  yldfrequency  <- estimObj$model$frequency
  if(yldfrequency == 12){

    freqname <- "months"

  }
  else if(yldfrequency == 1){

    freqname <- "years"

  }
  else if(yldfrequency == 4){

    freqname <- "quarters"

  }
  else{

    freqname <- ggplot2::element_blank()

  }

  # Start plotting
  ij <- 0
  for(ii in maturities){
    ij <- ij + 1

    dftemp <- data.frame(yield = yldfit[,ii],stime=nseq)
    # Baseline plot
    p1 <- ggplot2::ggplot(data=dftemp) + ggplot2::geom_line(mapping = ggplot2::aes_(x=~stime,y=~yield))
    # Y-Axis
    p1 <- p1 + ggplot2::ylab(paste(yldmaturities[ii],freqname))
    # X-Axis
    if(ij == length(maturities)){
      p1 <- p1 + ggplot2::xlab("Date")

    }
    else{

      p1 <- p1 + ggplot2::xlab(ggplot2::element_blank())

    }


    pltlist[[ij]] <- p1

  }

  do.call("grid.arrange",c(pltlist,nrow=length(maturities)))
}
