library(ggplot2)
library(quantmod)
library(rugarch)
library(tseries)
library(FinTS)
library(forecast)
library(xts)
library(zoo)

setwd("C:/Users/Admin/Downloads/Finance/FRM/GARCH VAR/Port4")
getwd() 
return.stock <-read.table("Port4.csv", header = TRUE, sep = ",")
return.stock$Date <- as.Date(return.stock$Date, format = "%m/%d/%Y")

#Define và coi
returns.ORP <- xts(return.stock$ORP, order.by = return.stock$Date)
plot(returns.MVP, main = "MVP Port Daily Return ", 
     major.ticks = "years", minor.ticks = FALSE,
     col = "lightblue", lwd = 1.5, ylim = c(-0.1,0.1))

# GARCH ======================================================================
spec.ORP <- ugarchspec(variance.model = list(model = "sGARCH", garchOrder = c(1, 1)), mean.model = list(armaOrder = c(0, 0)))
model.ORP <- ugarchfit(spec.ORP, data = returns.ORP)

# Rolling forecast ============================================================
n_options <- c(
  floor(1877 * 0.60),
  floor(1877 * 0.70),
  floor(1877 * 0.75),
  floor(1877 * 0.80))
results <- list()

for(i in seq_along(n_options)) {
  cat("Đang chạy với n.start =", n_options[i], "\n")
  roll <- ugarchroll(
    spec = spec.ORP,
    data = returns.ORP,
    n.start = n_options[i],
    refit.every = 15,
    calculate.VaR = TRUE,
    VaR.alpha = c(0.01, 0.05))
  results[[i]] <- roll
  cat("Hoàn thành!\n\n")}
names(results) <- paste0("nstart_", c("60pct", "70pct", "75pct", "80pct"))

#VaR
calculate_VaR_metrics.ORP <- function(roll_object) {
  daily_VaR_1pct <- roll_object@forecast[["VaR"]][["alpha(1%)"]]
  daily_VaR_5pct <- roll_object@forecast[["VaR"]][["alpha(5%)"]]
  annual_VaR_1pct <- daily_VaR_1pct * sqrt(250)
  annual_VaR_5pct <- daily_VaR_5pct * sqrt(250)
  
  VaR_summary <- data.frame(
    Metric = c("Daily VaR 1%", "Daily VaR 5%", "Annual VaR 1%", "Annual VaR 5%"),
    VaR = c(
      mean(daily_VaR_1pct, na.rm = TRUE),
      mean(daily_VaR_5pct, na.rm = TRUE),
      mean(annual_VaR_1pct, na.rm = TRUE),
      mean(annual_VaR_5pct, na.rm = TRUE)))
  return(list(
    daily_VaR_1pct = daily_VaR_1pct,
    daily_VaR_5pct = daily_VaR_5pct,
    annual_VaR_1pct = annual_VaR_1pct,
    annual_VaR_5pct = annual_VaR_5pct,
    summary = VaR_summary
  ))
}
VaR_results <- calculate_VaR_metrics.ORP(roll)
print(VaR_results$summary)

# Check với VaR còn lại =======================================================
print(cat('Var Para 1% là: ', mean(returns.ORP)-sd(returns.ORP)*2.326))
print(cat('Var Para 5% là: ' , mean(returns.ORP)-sd(returns.ORP)*1.645))
print(cat('Var Histor 1% là: ', quantile(returns.ORP, 0.01)))
print(cat('Var Histor 5% là: ', quantile(returns.ORP, 0.05)))