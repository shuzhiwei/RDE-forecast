
RDE_month_forecast = function(d){
	
	# 加载包
	library(forecast)

	# 参数数据character 转换成 list
	d <- d
	ccc<- unlist(strsplit(d,split=","))
	ddd <- list()
	for(i in 1:length(ccc)){
		o <- ccc[i]
		o <- gsub(",","",o)
		o <- as.numeric(as.character(o))
		ddd[i] <- o
	}

	# list转换为data.frame
	dataY<- data.frame(matrix(unlist(ddd), nrow=length(ddd)))


	startDate = c(2017,2)
	monthsForTesting = 1  

	data <- ts(dataY, frequency = 12, start = startDate)


	training <- ts (as.vector(data[1:(dim(as.vector(dataY))[1] - monthsForTesting)]),
                frequency = 12, start = startDate)
	
	
	#tsdisplay(training)
	
	#diff1 <- diff(training, 1)
	#tsdisplay(diff1)
	
	#diff1a <- diff(diff1, 12)
	#tsdisplay(diff1a)
		
	a = 1
	if(a > 0){
		
		best_p = 0;
		d = 1;
		best_q = 0;
		best_P = 0;
		D = 1;
		best_Q = 0;
		best_AIC = NA;
		
		if(best_p < 0){
			for (p in c(0:1)) {
				for (q in c(0:2)) {
					for (P in c(0:2)) {
						for (Q in c(0:2)) {
							suppressWarnings(f <- try(arima(training, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=12)), silent = TRUE)) #print(f)
							if (!is.element("try-error", class(f)))
							{
								aic = f$aic #print(aic)
								print(paste(p, d, q, P, D, Q))
								if (is.na(best_AIC) || aic < best_AIC)
								{
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
		}
		
		
		fit <- arima(training, order=c(best_p,1,best_q), seasonal=list(order=c(best_P,1,best_Q), period=12))
		print(paste(best_p, d, best_q, best_P, D, best_Q, 12))
		forecastData = forecast(fit, h = monthsForTesting, level = c(99.5))
		
		v3 = as.vector(unlist(forecastData[4]))
		
		#计算准确率
		values = unlist(ddd)
		realValue = values[length(values)]
		print(paste("realValue: ",realValue, "forecastValue: ", v3[1]))
		accuracyPercent = floor((1-(abs(v3[1] - realValue) / realValue))*100)
		print(paste("accuracyPercent(%): ", accuracyPercent, "%"))
		
		v3
	}
		
	
	
	
}                                       

#初始
#d = "38060.200000000004, 38042.32, 62156.0, 86940.0, 145551.015625, 123270.03125, 82537.0, 56628.0, 128310.0, 189713.03125, 231730.0, 166173.0, 141904.99000000022, 83430.0, 99750.0, 126870.0, 159330.0, 160453.0, 96210.0, 77430.0, 137772.99, 245250.0, 242040.0"

d = "38060.200000000004, 38042.32, 62156.0, 86940.0, 145551.015625, 123270.03125, 82537.0, 56628.0, 128310.0, 189713.03125, 231730.0, 166173.0, 141904.99000000022, 83430.0, 99750.0, 126870.0, 159330.0, 160453.0, 96210.0, 77430.0, 137772.99, 245250.0, 242040.0"

#d="4112,38060.2,38042.32,62156,86940,145551.0156,123270.0313,82537,56628,128310,189713.0313,231730,166173,141904.99,83430,99750,126870,159330,160453,96210,77430,137772.99,245250,242040,183570,107310,84180,100320,131340,165150"
RDE_month_forecast(d)


