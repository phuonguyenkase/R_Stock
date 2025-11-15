library(fPortfolio)
library(timeSeries)
library(quantmod)
library(dplyr)
library(ggplot2)
library(PerformanceAnalytics)

setwd("C:/Users/Admin/Downloads/Finance/FRM/4port")
getwd() 
return.matrix <-read.table("4port.csv", header = TRUE, sep = ",", row.names = "Date")

#return.matrix <- price.matrix[apply(price.matrix, 1, function(row) all(row != " ")), ]
#return.matrix <- price.matrix[complete.cases(price.matrix), ]

# Kiểm tra dữ liệu `return.matrix`
#class(return.matrix)
#print(return.matrix)
#str(return.matrix)
#anyNA(return.matrix)
#any(is.infinite(return.matrix))

#Loại giá trị Inf
#return.matrix <- return.matrix[apply(return.matrix, 1, function(row) all(is.finite(row))), ]

assets.returns = colMeans(return.matrix)
equal.weights = rep(1/4,4)
portfolio.return = t(equal.weights)%*%assets.returns
var.covar = cov(return.matrix)
portfolio.var = t(equal.weights)%*%(var.covar%*%equal.weights)
sd.portfolio = sqrt(portfolio.var)

return.matrix = as.timeSeries(return.matrix)

efficient.frontier = portfolioFrontier(return.matrix, `setRiskFreeRate<-`(portfolioSpec(),0.03/250), constraints = 'LongOnly')

plot(efficient.frontier, c(1,2,3,7,8))