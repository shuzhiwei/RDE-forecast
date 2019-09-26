#使用训练数据构建模型
library(forecast)
hourForecastArimaTraining = function(trainingData){
	trainingData = ts(trainingData, frequency=7*24)
	#fit <- auto.arima(trainingData, trace=T)
	#return(fit)
	
	flag1 = 1
	if(flag1 > 0){
		best_p = 3
		d = 0
		best_q = 2
		best_AIC = NA
		for(p in c(0:3)){
			for(d in c(0:1)){
				for(q in c(0:2)){
					suppressWarnings(f <- try(arima(trainingData, order=c(p,d,q)), silent=TRUE))
					if(!is.element("try-error", class(f))){
						aic = f$aic
						print(paste(p,d,q))
						if(is.na(best_AIC) || aic < best_AIC){
							best_AIC = aic
							best_p = p
							best_d = d
							best_q = q
						}
					}
				}
			}
		}
		print(paste("best model is: ", best_p, best_d, best_q))
		fit <- arima(trainingData, order=c(best_p, d, best_q))
		return(fit)
	}
	
	flag2 = -1
	if(flag2 > 0){
		best_p = 1
		d = 1
		best_q = 0
		best_P = 0
		D = 1
		best_Q = 1
		best_frequency = 168
		best_AIC = NA
		#fit <- arima(trainingData, order=c(best_p, d, best_q))
		for(p in c(0:2)){
			for(q in c(0:2)){
				for(P in c(0:2)){
					for(Q in c(0:2)){
						suppressWarnings(f <- try(arima(trainingData, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=7*24)), silent=TRUE))
						if(!is.element("try-error", class(f))){
							aic = f$aic
							print(paste(p, d, q, aic))
							if(is.na(best_AIC) || aic < best_AIC){
								best_AIC = aic
								best_p = p
								best_q = q
								best_P = P
								best_Q = Q
							}
						}
					}
				}
			}
		}
		fit <- arima(trainingData, order=c(best_p, d, best_q), seasonal=list(order=c(best_P,D,best_Q), period=best_frequency))
		return(fit)
	}
	
	flag3 = -1
	if(flag3 > 0){
		best_p = 1
		best_d = 0
		best_q = 0
		best_P = 1
		best_D = 0
		best_Q = 0
		best_AIC = NA
		best_frequency = 7*24
		
		fit <- arima(order=c(best_p, best_d, best_q), seasonal=list(order=c(best_P,best_D,best_Q),period=best_frequency))
		return(fit)
	}
	
	
}

#使用构建好的模型进行预测
hourForecastArima = function(fit, numbersOfHours){
	forecastData = forecast(fit, h = numbersOfHours, level = c(99.5))
	return(forecastData)
}