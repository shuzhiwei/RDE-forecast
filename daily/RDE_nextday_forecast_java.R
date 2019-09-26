
RDE_nextday_forecast = function(d){

	startDate = "2018-01-01"
	daysForTesting = 1

	source("F:/ldy_workspace/RserveProject/src/jdbc_day/dailyForecastArima.R")

	#数据清洗
	d <- d
	ccc<- unlist(strsplit(d,split=","))
	ddd <- list()
	for(i in 1:length(ccc)){
		o <- ccc[i]
		o <- gsub(",","",o)
		o <- as.numeric(as.character(o))
		ddd[i] <- o
	}

	# list转换为data.frame,并获取所有行第一列的数据,并封装成一个向量
	dataY<- data.frame(matrix(unlist(ddd), nrow=length(ddd)))
	dataDaily = as.vector(dataY[,1])
	#dataDaily = unlist(ddd)
	
	# 构建原始数据的时间序列数据
	data <- ts(dataDaily, frequency = 1)
	plot(data, ylab = "Daily electricity usage (Orignal)", xlab = paste("Number of days starting from", startDate))

flag = 1
if(flag > 0){
	#预测nextday的所有结果,并用向量forecastValues封装
	#Search all next day
	#从最后一天开始预测,依次向前,并将预测值填充forecastValues向量
	forecastValues = rep(0, daysForTesting)
	forecastStart = length(dataDaily) - daysForTesting + 1
	
	for (days in c(1:daysForTesting)) {
	  #Create training data (without last 4 days)
	  trainingData <- as.vector(dataDaily[1:(length(dataDaily) - days)])

	  #Create ARIMA model
	  fit <- dailyForecastArimaTraining(trainingData, 1)
	  #Forecase
	  forecastData = dailyForecastArima(fit, 1)
	  forecastValues[daysForTesting-days+1] = forecastData$mean
	}
	

	#Calculate average error %
	#计算错误率
	totalErrorPercent = 0;
	for (days in c(1:daysForTesting)) {
	  forecastValue = forecastValues[daysForTesting-days+1]
	  realValue = dataDaily[length(dataDaily)-days+1]
	  totalErrorPercent = totalErrorPercent + abs(realValue - forecastValue) / realValue
	  print(paste("realValue: ", realValue))
	  print(paste("forecastValue: ", forecastValue))
	}
	totalErrorPercent = totalErrorPercent / daysForTesting
	print(paste("average-percent: ", floor((1 - totalErrorPercent) * 100), "%"))
	
	#Plot
	plot(data,
			xlab = paste("Days starting from", startDate),
			ylab = "Daily electricity usage", 
			main = paste("Next day Forecasts - RDE Group\r\n",
					"Accuracy(%) ",  floor((1 - totalErrorPercent) * 100) , "%"),
			sub = "Red - Actual, Blue - Forecast"
	)
	lines(data,col="red")
	
	forecastData = ts(forecastValues, start=forecastStart)
	if (daysForTesting == 1) {
		points(forecastData,col="Blue",lwd=2)
	} else {
		lines(forecastData,col="Blue",lwd=2)
	}
	
	v3 = as.vector(unlist(forecastValues[1]))
	v3
	}
}

d=read.table('F:/龙德缘电力科技/data/data.txt', head=FALSE)
d=as.vector(t(d))
RDE_nextday_forecast(d)
	


