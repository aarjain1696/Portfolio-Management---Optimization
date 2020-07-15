objects()
head(myxts)

install.packages("PerformanceAnalytics")
install.packages("fPortfolio")
library(PerformanceAnalytics)
library(fPortfolio)
str(myxts)
tail(myxts,10)
summary(myxts)


## Converting XTS to timeseries 
as.timeSeries(myxts) -> mytimeSeries
str(mytimeSeries)
head(mytimeSeries)
plot(mytimeSeries)

## Computing Minimum risk portfolio
minvariancePortfolio(mytimeSeries) -> minrisk
plot(minrisk)
minrisk
0.0956*100000


## Maximum Risk Portfolio
portfolioSpec() -> rf
0.0017 -> setRiskFreeRate(rf)
rf

tangencyPortfolio(mytimeSeries, rf) -> maxSharpe
maxSharpe
sr = (0.0147-0.0017)/0.0312
sr
round(sr,3)


## plotting the efficiency frontier of the portfolio
portfolioFrontier(mytimeSeries, rf)-> myfrontier
myfrontier
plot(myfrontier)
1
2
3
5
4
6
7
8
0


## exporting the data from frontier : 
frontierPoints(myfrontier) -> mypoints
head(mypoints)

## getting the weights 
getWeights(myfrontier) -> myweights
myweights

tail(myweights)

## exporting into CSV file
cbind(mypoints, myweights) -> myexport
write.csv(myexport, file = "myexport.csv")
