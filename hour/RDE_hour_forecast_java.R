
RDE_hour_forecast = function(d){
	
	#构建训练集和测试集所需的一些参数
	startDate = "2019-01-01"
	hoursForTesting = 1 #预测未来一小时
	
	#导包
	library(forecast)
	source("F:\\ldy_workspace\\RDE_forecast\\hour\\hourForecastArima.R")
	
	#获取数据方式一:
	getData1 = -1
	if(getData1 > 0){
		d <- d
		ccc<- unlist(strsplit(d,split=","))
		ddd <- list()
		for(i in 1:length(ccc)){
			o <- ccc[i]
			#o <- gsub(",","",o)
			o <- as.numeric(as.character(o))
			ddd[i] <- o
		}
		values = unlist(ddd)
		flag = 1
		if(flag > 0){
			#构建训练数据
			forecastValues = rep(0,hoursForTesting)
			forecastStart = length(values) - hoursForTesting + 1
			for(hour in c(1:hoursForTesting)){
				trainingData = as.vector(values[1:(length(values)-hour)])
				fit <- hourForecastArimaTraining(trainingData)
				forecastData <- hourForecastArima(fit,1)
				forecastValues[hoursForTesting - hour + 1] = forecastData$mean
			}
			
			#计算准确率
			totalErrorPercent = 0
			for(hour in c(1:hoursForTesting)){
				forecastValue = forecastValues[hoursForTesting - hour + 1]
				realValue = values[length(values) - hour + 1]	
				print(paste("f:",class(forecastValue), "  r:", class(realValue)))
				totalErrorPercent = totalErrorPercent + abs(forecastValue - realValue)/realValue
				print(paste("realValue: ", realValue))	
				print(paste("forecastValue: ", forecastValue))
			}
			avgErrorPercent = totalErrorPercent / hoursForTesting
			accuracyPercent = floor((1 - avgErrorPercent)*100)
			print(paste("accuracyPercent: ", accuracyPercent, "%"))
			
			
			
		}
	}
	
	#获取数据方式二:
	getData2 = 1
	if(getData2 > 0){
		dataRaw = read.csv("C:\\Users\\Administrator\\Desktop\\测试数据\\zzdj_2019-07-01_2019-08-21每小时数据.csv")
		#dataRaw = read.csv("F:\\龙德缘电力科技\\week03\\数据源\\智造大街2019_01_01-2019_08_21每小时数据.csv")
		#dataRaw = read.csv("F:\\龙德缘电力科技\\week03\\数据源\\中关村东升科技园D区配电室2019_01_01-2019_08_21每小时数据.csv")
		#dataRaw = read.csv("F:\\龙德缘电力科技\\week03\\数据源\\中关村东升科技园B区配电室2019_01_01-2019_08_21每小时数据.csv")
		
		#dataRaw = read.csv("C:\\Users\\Administrator\\Desktop\\111.csv")
		
		print(lengths(dataRaw))
		
		values = as.vector(dataRaw$value)
		print(class(values))
		
		#观察原始数据ts图形,acf图拖尾=>q=0,pacf图截尾=>p的范围(0:30),当时原始图形并不稳定
		data <- ts(values, frequency=7*24)
		diff1 <- diff(data, 1)
		diff1a <- diff(diff1, 7*24)
		tsdisplay(data)
		
		flag = 1
		if(flag > 0){
			#构建训练数据
			forecastValues = rep(0,hoursForTesting)
			forecastStart = length(values) - hoursForTesting + 1
			for(hour in c(1:hoursForTesting)){
				trainingData = as.vector(values[1:(length(values)-hour)])
				fit <- hourForecastArimaTraining(trainingData)
				forecastData <- hourForecastArima(fit,1)
				forecastValues[hoursForTesting - hour + 1] = forecastData$mean
			}
			
			#计算准确率
			totalErrorPercent = 0
			for(hour in c(1:hoursForTesting)){
				forecastValue = forecastValues[hoursForTesting - hour + 1]
				realValue = values[length(values) - hour + 1]	
				print(paste("f:",class(forecastValue), "  r:", class(realValue)))
				totalErrorPercent = totalErrorPercent + abs(forecastValue - realValue)/realValue
				print(paste("realValue: ", realValue))	
				print(paste("forecastValue: ", forecastValue))
			}
			avgErrorPercent = totalErrorPercent / hoursForTesting
			accuracyPercent = floor((1 - avgErrorPercent)*100)
			print(paste("accuracyPercent: ", accuracyPercent, "%"))
			
			#绘图
			plot(data, xlab=paste("从",startDate, "开始的小时"), 
					ylab = "每小时用电量 ",
					main = paste("次小时预测 - 龙德缘电力\r\n","准确率(%)", accuracyPercent, "%"),
					sub = "红色--真实值, 蓝色--预测值")
			lines(data, col="red")
			
			forecastDatas = ts(forecastValues, start=forecastStart)
			if(hoursForTesting == 1){
				points(forecastDatas, col="blue", lwd=10)
			}else{
				lines(forecastDatas, col="blue", lwd=2)
			}
			
			
		}
	}
	
	
}

#智造大街2019-01-01 00:00:00到2019-08-21 00:00:00之间每小时的数据
d = ""
RDE_hour_forecast(d)



