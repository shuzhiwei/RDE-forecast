
RDE_hour_forecast = function(d){
	
	#����ѵ�����Ͳ��Լ������һЩ����
	startDate = "2019-01-01"
	hoursForTesting = 1 #Ԥ��δ��һСʱ
	
	#����
	library(forecast)
	source("F:\\ldy_workspace\\RDE_forecast\\hour\\hourForecastArima.R")
	
	#��ȡ���ݷ�ʽһ:
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
			#����ѵ������
			forecastValues = rep(0,hoursForTesting)
			forecastStart = length(values) - hoursForTesting + 1
			for(hour in c(1:hoursForTesting)){
				trainingData = as.vector(values[1:(length(values)-hour)])
				fit <- hourForecastArimaTraining(trainingData)
				forecastData <- hourForecastArima(fit,1)
				forecastValues[hoursForTesting - hour + 1] = forecastData$mean
			}
			
			#����׼ȷ��
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
	
	#��ȡ���ݷ�ʽ��:
	getData2 = 1
	if(getData2 > 0){
		dataRaw = read.csv("C:\\Users\\Administrator\\Desktop\\��������\\zzdj_2019-07-01_2019-08-21ÿСʱ����.csv")
		#dataRaw = read.csv("F:\\����Ե�����Ƽ�\\week03\\����Դ\\������2019_01_01-2019_08_21ÿСʱ����.csv")
		#dataRaw = read.csv("F:\\����Ե�����Ƽ�\\week03\\����Դ\\�йش嶫���Ƽ�԰D�������2019_01_01-2019_08_21ÿСʱ����.csv")
		#dataRaw = read.csv("F:\\����Ե�����Ƽ�\\week03\\����Դ\\�йش嶫���Ƽ�԰B�������2019_01_01-2019_08_21ÿСʱ����.csv")
		
		#dataRaw = read.csv("C:\\Users\\Administrator\\Desktop\\111.csv")
		
		print(lengths(dataRaw))
		
		values = as.vector(dataRaw$value)
		print(class(values))
		
		#�۲�ԭʼ����tsͼ��,acfͼ��β=>q=0,pacfͼ��β=>p�ķ�Χ(0:30),��ʱԭʼͼ�β����ȶ�
		data <- ts(values, frequency=7*24)
		diff1 <- diff(data, 1)
		diff1a <- diff(diff1, 7*24)
		tsdisplay(data)
		
		flag = 1
		if(flag > 0){
			#����ѵ������
			forecastValues = rep(0,hoursForTesting)
			forecastStart = length(values) - hoursForTesting + 1
			for(hour in c(1:hoursForTesting)){
				trainingData = as.vector(values[1:(length(values)-hour)])
				fit <- hourForecastArimaTraining(trainingData)
				forecastData <- hourForecastArima(fit,1)
				forecastValues[hoursForTesting - hour + 1] = forecastData$mean
			}
			
			#����׼ȷ��
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
			
			#��ͼ
			plot(data, xlab=paste("��",startDate, "��ʼ��Сʱ"), 
					ylab = "ÿСʱ�õ��� ",
					main = paste("��СʱԤ�� - ����Ե����\r\n","׼ȷ��(%)", accuracyPercent, "%"),
					sub = "��ɫ--��ʵֵ, ��ɫ--Ԥ��ֵ")
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

#������2019-01-01 00:00:00��2019-08-21 00:00:00֮��ÿСʱ������
d = ""
RDE_hour_forecast(d)


