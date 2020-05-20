# load data
library("RYieldCurve")
data("US_Yield_Curve")

# get data into

Yield <- as.matrix(data.frame(
  SVENY01 = US_Yield_Curve[,01], SVENY02 = US_Yield_Curve[,02],
  SVENY03 = US_Yield_Curve[,03], SVENY04 = US_Yield_Curve[,04],
  SVENY05 = US_Yield_Curve[,05], SVENY06 = US_Yield_Curve[,06],
  SVENY07 = US_Yield_Curve[,07], SVENY08 = US_Yield_Curve[,08],
  SVENY09 = US_Yield_Curve[,09], SVENY10 = US_Yield_Curve[,10],
  SVENY11 = US_Yield_Curve[,11], SVENY12 = US_Yield_Curve[,12],
  SVENY13 = US_Yield_Curve[,13], SVENY14 = US_Yield_Curve[,14],
  SVENY15 = US_Yield_Curve[,15], SVENY16 = US_Yield_Curve[,16],
  SVENY17 = US_Yield_Curve[,17], SVENY18 = US_Yield_Curve[,18],
  SVENY19 = US_Yield_Curve[,19], SVENY20 = US_Yield_Curve[,20],
  SVENY21 = US_Yield_Curve[,21], SVENY22 = US_Yield_Curve[,22],
  SVENY23 = US_Yield_Curve[,23], SVENY24 = US_Yield_Curve[,24],
  SVENY25 = US_Yield_Curve[,25], SVENY26 = US_Yield_Curve[,26],
  SVENY27 = US_Yield_Curve[,27], SVENY28 = US_Yield_Curve[,28],
  SVENY29 = US_Yield_Curve[,29], SVENY30 = US_Yield_Curve[,30]
),ncol=30)

maturity <- seq(1:30)*12

# Split data set into "training" and "test" set
nobs <- nrow(Yield)
Yield_training <- Yield[1:(nobs-12),]
Yield_test <- Yield[(nobs-11):nobs,]

# Estimate Dynamic Nelson-Siegel Model and find optimal decay factors using grid search

test <- estimate_yield_model(yields = Yield_training,exogen=NULL,maturity = maturity,method="DNS")
plot_errors(test)
fc_test1 <- forecast(test,nhor=12,ylddata=Yield_test,maturity = maturity)


# Estimate Dynamic Söderlind-Svensson Model using user-supplied decay factors

test2 <- estimate_yield_model(yields = US_Yield_Curve, exogen = NULL, maturity = maturity, method = "DSS", lambda = c(0.25, 0.6))
plot_errors(test2)
fc_test2 <- forecast(test2,nhor=12,ylddata=Yield_test,maturity = maturity)
