#lib
library(forecast)
library(tseries)

dailyForecastArimaTraining<-function(trainingdata, numberOfDays){
	#Create training data
	training <- ts (trainingdata)
  
	#Display training data
	#tsdisplay(training)
  
	#Step 1.
	#Remove go-up by diff 1
	#diff1 <- diff(training, 1)
	#tsdisplay(diff1)
  
	#Remove seasional by another diff
	#diff1a <- diff(diff1, 7)
	#tsdisplay(diff1a)

  
	#########################################################
	#option #1
	#auto.arima
	#fit <- auto.arima(training, trace=T) #it desn't work well
	#return (fit)
  
	#########################################################
	#option #2
	#Get best p,q and P, Q (find MIN AIC) by ourself
	a = 1
	if(a > 0){
	    best_p = 7; # Give -1 to run the search, and it takes time.
		best_d = 1;
	  	best_q = 0;
	  	best_P = 0;
	  	best_D = 1;
	  	best_Q = 2;
	  	best_frequency = 7;
	  	best_AIC = NA;
	
		if (best_p < 0){ #if best_p is not given, search them
			for (d in c(0:1)) {
				for (q in c(0:2)) {
					for (P in c(0:2)) {
						for(D in c(0:1)){
							for (Q in c(0:2)) {
								#print(paste(best_p,d,q,P,D,Q))
								suppressWarnings(f <- try(arima(training, order=c(best_p,d,q), seasonal=list(order=c(P,D,Q), period=best_frequency)), silent = TRUE))
								if (!is.element("try-error", class(f))){
									aic = f$aic
									print(paste(best_p,d,q,P,D,Q,aic))
									if (is.na(best_AIC) || aic < best_AIC){
										best_AIC = aic
										best_d = d
										best_q = q
										best_P = P
										best_D = D
										best_Q = Q
									}
								}
							}
						}
					}
				}
			}
		}
	
		print(paste('Best arima model is: (', best_p, ',', best_d, ',', best_q,')(', best_P, ',', best_D,',', best_Q,')[', best_frequency,']'))
	
		#Create arima model
		suppressWarnings(fit <- arima(training, order=c(best_p,best_d,best_q), seasonal=list(order=c(best_P,best_D,best_Q), period=best_frequency)))
		
		fit$x = training
		return(fit)
  }
}

#############################
# Forecast function for ARIMA
# Input the ARIMA model and forecast
# "forecastData = dailyForecastArima(fit, numberOfDays)"
# Where fit is ARIMA model (from dailyForecastArimaTraining),
# And numberOfDays is the number of days to forcast, e.g. 7
# And forecastData is vector of forecast data
#############################
dailyForecastArima<-function(fit, numberOfDays){
	forecastData = forecast(fit, h = numberOfDays, level = c(99.5))
	return(forecastData)
}


