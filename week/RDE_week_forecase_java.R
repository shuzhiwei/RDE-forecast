
RDE_week_forecast = function(d){
	#导包
	library(forecast)
	library(lubridate)

	#设置训练集,测试集参数
	weeksForTesting = 2  #Use last 4 weeks for testing
	weeksToIgnore = 18 #Ignore the some begining weeks, where data is not good
	beginOfWeek = 1	#The begin day of week [1,7]

	# 参数数据character 转换成 数值型list
	d <- d
	ccc<- unlist(strsplit(d,split=","))
	ddd <- list()
	for(i in 1:length(ccc)){
		o <- ccc[i]
		o <- gsub(",","",o)
		o <- as.numeric(as.character(o))
		ddd[i] <- o
	}

	# list转换为data.frame,并获取数据帧里每一行,第一列的值
	dataY<- data.frame(matrix(unlist(ddd), nrow=length(ddd)))
	dataDaily = as.vector(dataY[,1])
	

	# 将每天数据转换成每周数据所需的一些参数
	beginIndex = 1 #Search begin index by beginOfWeek
	totalLength = length(dataDaily)
	startDate = "2016-10-08"
	
	#根据开始日期和每天数据的长度,计算起始一周的第一天的下标
	for(index in c(0:totalLength)) {
	  date = as.Date(startDate) + days(index)
	  if (wday(date) == beginOfWeek)
	  {
	    beginIndex = index
	    break
	  }
	}
	
	#得出起始一周的第一天所对应的开始日期
	startDate = as.Date(startDate) + days(beginIndex)
	print(paste('The start date is:', startDate))
	
	#计算一共有多少周, 并将每周数据计算出来封装进dataWeekly向量中
	dataWeekly = rep(0, floor((totalLength - (beginIndex - 1))/7))
	daySum = 0
	valueSum = 0
	weekIndex = 0
	for(index in c(beginIndex:totalLength)) {
	  daySum = daySum + 1
	  valueSum = valueSum + dataDaily[index]
	  if (daySum >= 7)
	  {
	    weekIndex = weekIndex + 1
	    dataWeekly[weekIndex] = valueSum
	    daySum = 0
	    valueSum = 0
	  }
	}

	print(paste('The total number of weeks converted is:', length(dataWeekly)))
	
	#构建时间序列数据
	data <- ts(dataWeekly, frequency = 1)

	#绘制原始数据的周用电量的散点图
	plot(data, ylab = "Weeky electricity usage (Orignal)", 
     xlab = paste("Number of weeks starting from", startDate))
	

	#如果有不好数据, 则忽略
	if (weeksToIgnore > 0)
	{
	  startDate = as.Date(startDate) + weeks(weeksToIgnore)
	  dataWeekly = dataWeekly[(1+weeksToIgnore):length(dataWeekly)]
	  data <- ts(dataWeekly, frequency = 1)
	  plot(data, ylab = "Weeky electricity usage", 
	       xlab = paste("Number of weeks starting from", startDate))
	}
	
	#创建训练数据
	training <- ts (as.vector(data[1:(length(dataWeekly) - weeksForTesting)]),
                frequency = 1)

	#展示训练数据
	tsdisplay(training)
	
	#一阶差分化
	diff1 <- diff(training, 1)
	tsdisplay(diff1)

	#选取ARIMA模型的参数
	best_p = 4 # -1; # Give -1 to run the search, and it takes time.
	d = 1;
	best_q = 0;
	best_P = 0;
	D = 1;
	best_Q = 0;
	best_frequency = 52;
	best_AIC = NA;

	if (best_p < 0) #if best_p is not given, search them
	{
	  for (frequency in c(4,36,48,52)) {
	    for (p in c(4,9,12)) {
	      for (q in c(0:1)) {
		for (P in c(0:1)) {
		  for (Q in c(0:1)) {
		    suppressWarnings(f <- try(arima(training, order=c(p,d,q), seasonal=list(order=c(P,D,Q), period=frequency)), silent = TRUE)) #print(f)
		    if (!is.element("try-error", class(f)))
		    {
		      aic = f$aic #print(aic)
		      if (is.na(best_AIC) || aic < best_AIC)
		      {
			best_AIC = aic
			best_frequency = frequency
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
	}

	print(paste('Best arima model is: (', best_p, ',', d, ',', best_q,')(', best_P, ',', D,',', best_Q,')[', best_frequency,']'))

	#创建模型
	suppressWarnings(fit <- arima(training, order=c(best_p,d,best_q), seasonal=list(order=c(best_P,D,best_Q), period=best_frequency)))

	#使用模型进行预测
	forecastData = forecast(fit, h = weeksForTesting, level = c(99.5))

	#绘制预测数据的散点图
	plot(forecastData, 
	     xlab = paste("Weeks starting from", startDate, "/ 从", startDate, "开始的周数"),
	     ylab = "Weekly electricity usage / 周用电总量", 
	     main = "Weekly Forecasts - RDE Group / 周用电量预测 - 龙德缘电力",
	     sub = "Red - Actual, Blue - Forecast / 红色 - 实际用电量, 蓝色 - 预测值"
	)
	#lines(forecastData$fitted,col="green")
	#添加原始数据的散点图
	lines(data,col="red")

	v3 = as.vector(unlist(forecastData[4]))
	v3
	
}

d="232,364.8,712.8,634.4,945.8,946.82,941.8,1022.4,236,136.8,305.6,440,587.2,56.8,388.8,148.8,853.6,630.4,576.8,806.4,753.6,624.8,621.6,664.8,766.4,788,100,1459.2,1432,1077.6,816,961.6,1222.4,1124.8,1777.6,1138.4,909.6,642.4,825.6,1128,1012,1104,659.2,748.8,902.4,1012.8,833.6,880.8,748,871.2,903.2,972,1861.6,486.4,1055.2,2570.4,486.4,1055.2,1583.2,1172.8,1684.8,691.2,1422.4,1673.6,1654.4,1494.4,1445.6,1215.2,1047.2,635.2,1242.6,1032.8,1677.6,1361.6,912,906.4,1210.4,1032,1211.2,1536,1752.8,1053.6,943.2,1232.8,1364.8,1280,1366.4,1224,1053.6,770.4,1182.87,1068.65,516.8,1470,1170,1050,1260,1350,1170,1350,1350,2460,1230,1350,1200,1530,1470,1320,1050,930,1170,1470,1290,1230,1350,1230,1950,1050,1470,1590,1440,1260,1140,1110,1470,1656,1350,1590,1560,1383,831,672,1392,1800,2235,2859,3596,3279,2670,2430,2640,2679,2604,2559,2877,2361,2205,1887,3561,3072,2196,1779,1920,2832,2295,2223,2664,3069,1872,2124,1395,2517,3045,3897,3465,3261,2919,3759,2517,2502,3645,2712,1983,2298,3939,3405,4113,4284,5238,4281,3951,5694,5202,5862,4470,4800,3444,4377,5715,5805,6903.02,6105,5721,3294,2994,6219,5463,5781,6066,4089,3129,3210,5469,4917,3732,4191,4665,2511,2316,5175,5844,5628,5550,5637,2979,2814,5433,3651,5070,5496,5385,2730,2472,3480,4410,4752,4491,3222,2484,2358,5112,4731,4941,4356,3699,2205,1902.03,3132,2859,3006,3441,3417,2007,1764,3111,3561,3525,3909,4299,2112,1677,1687,3204,3711,3801,3336,2106,1734,3276,3075,3039,3306,2757,1845,1518,3282,2544,2640,2193,2166,1935,2232,1368,1122,1071,1119,1212,1449,1527,2124,2346,1983,2385,1998,1710,1569,2322,2511,3306,2484,2439,1965,1695,2349,2433,2583,2690,2367,2269,2658,2646,2082,1896,1830,2544,2709,2670,2781,2823,2016,2148,6546,5070,4842,4926,5928,4017,4182,6162,5883,5658,5835,5364,3792,4326,6462,6147,7563,6804,6318,3765,3738,7836,5316,2382,6444,5811,4134,4584,7329,7503,8181,8127,7995,6423,5727,6969,7587,6837,6771,6603,5232.03,4875,7152,4430,8676,7872,7452,3765,3879,4215,8625,8796,8796,7563,5616,5418,8850,8001,9414,9558,8346,6117,5151,8274,6753,7050,7224,6252,4998,6450,6376,10086,9801,9600,6333,7170,6174,9120,7599,8004,8517,8472,7107,6426,9720,8769,7944,8466,4372,7020,7710,6390,4770,3660,3780,3540,4260,3750,3720,3360,3570,4530,5370,5820,6390,5360,6960,6420,5370,5376,4590,3990,5910,7410,7710,6540,6930,4680,5430,5379,5329.99,3960,4200,5520,5940,4740,5130,4800,4590,3090,3240,2310,2220,3060,3060,3120,2970,2940,2370,2220,2970,3720,3030,2370,2220,2250,2880,3090,2970,2970,2910,3210,2370,2160,2940,2940,2970,3090,3000,2640,2520,3090,2940,2880,2910,2880,2820,2280,2190,2340,2910,2880,1440,1500,2220,3030,3060,3000,3120,3240,2730,2550,4140,4620,4650,3780,3720,2460,2550,3270,3060,3240,3960,3990,2850,2400,4140,4050,4350,4500,3750,2850,2490,5400,5910,5400,5130,4650,2970,2430,4440,4620,3900,4200,4260,2430,2310,2430,5160,5190,5250,5130,2370,2550,6270,6150,5700,5520,5100,2910,2580,6300,5730,6060,6240,5430,2910,2370,5910,5220,4410,4530,5100,2970,2880,5700,5310,5910,5820,6690,3390,2790,7320,6150,6420,6090,6180,3360,2820,8610,8130,7980,6763,8040,3750,2790,6780,6750,5880,6330,6420,3270,2760,6300,5940,5970,5460,5550,2880,2580,6270,5370,5100,5040,4950,2610,2550,6120,5520,5130,4680,4920,3060,2370,4920,4500,4680,4080,3900,2400,2310,3990,3750,3840,4110,3540,2370,2010,3630,3570,3450,3450,3060,2070,1560,1830,2970,3060,3210,3090,2820,2610,1830,1800,1770,1890,1860,1800,1860,2730,2670,2730,2760,2790,2250,2160,3030,2850,2760,2760,2670,2160,2160,2970,2940,2910,3150,2880,2220,2160,2970,2940,3000,3090,3210,2550,2520,3330,5100,6060,5670,4620,3660,3120,4200,4620,4650,4740,5220,3960,3330,4740,4470,5040,5190,6570,5202.99,4950,5760,3810,6360,6180,5850,5730,5550,6150,6870,7530,8640,9810,9120,8490,9060"
RDE_week_forecast(d)
