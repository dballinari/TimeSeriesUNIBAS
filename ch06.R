# R Examples - Chapter 6: Forecasting
rm(list=ls())
library("forecast")

# Forecast US DGP ---------------------------------------------------------

data("USMacroG", package = "AER")
gdp <- USMacroG[, "gdp"]

print(gdp)

plot(ts.union(levels = gdp, logs = log(gdp),
              growth = 100*diff(log(gdp))), main = "")

dlgdp <- 100 * diff(log(gdp))

acf(dlgdp)
pacf(dlgdp)

auto.arima(dlgdp, ic = "aic", stationary = TRUE,
           max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
           stepwise = FALSE, trace = TRUE, approximation = FALSE)


dlgdp_ar1 <- arima(dlgdp, c(1, 0, 0))
dlgdp_ar1

# Forecast manually
cf <- coef(dlgdp_ar1)
cf[2] + cf[1]^(1:4) * as.numeric(tail(dlgdp, 1) - cf[2])

# Use built-in function
pred <- predict(dlgdp_ar1, n.ahead = 4)
pred

plot(dlgdp, xlim = c(1991, 2001.75))
abline(h = cf[2], col = "slategray")
lines(pred$pred, col = 4, lwd=2)
lines(pred$pred + qnorm(0.025) * pred$se, col = 4, lty = 2, lwd=2)
lines(pred$pred + qnorm(0.975) * pred$se, col = 4, lty = 2, lwd=2)

# check long-run forecast
predict(dlgdp_ar1, n.ahead = 100)

# use external library: library("forecast")
dlgdp_fc <- forecast(dlgdp_ar1, h = 4)
dlgdp_fc
plot(dlgdp_fc, shaded = FALSE, xlim = c(1991, 2001.75))
plot(dlgdp_fc, shadecols = gray(c(0.8, 0.6)), xlim = c(1991, 2001.75))



# Oil forecasting ---------------------------------------------------------
data("oil.price", package = "TSA")
oil.ret <- diff(log(oil.price))


auto.arima(oil.ret, ic = "aic", stationary = TRUE,
           max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
           stepwise = FALSE, trace = TRUE, approximation = FALSE)

# Fit an MA(1) without intercept to the data:
oil_ma1 <- arima(oil.ret, order = c(0, 0, 1), include.mean = FALSE)
# The last available observation is for January 2006
tail(oil.ret)

# Forecast the remaining 11 months of 2006
oil_ret_fc <- forecast(oil_ma1, h = 11)
oil_ret_fc
plot(oil_ret_fc, shaded = FALSE, xlim = c(2004, 2007))
abline(h=0)


# Forecast Oil level ------------------------------------------------------

log_oil <- log(oil.price)
plot(log_oil)
# NOTE: the function 'arima' does not include a constant if d>0! For example,
# when fitting an ARIMA(0,1,1) to the log oil prices, we obtain a model without
# constant
oil_level_model <- arima(log_oil, order = c(0, 1, 1))

# Forecast the remaining 11 months of 2006 for the log oil price
oil_level_fc <- forecast(oil_level_model, h = 11)
oil_level_fc
plot(oil_level_fc, shaded = FALSE, xlim = c(2005, 2007))


# Produce same forecasts with Arima: with the Arima function, when inclduing a constat, 
# we implicitly include a polynomial of order 'd'. For the ARIMA(0,1,1) on the oil prices
# this means that we include a linear time trend
oil_level_model_2 <- Arima(log_oil, c(0,1,1), include.constant = TRUE)
oil_level_fc_2 <- forecast(oil_level_model_2, h = 11)
oil_level_fc_2
plot(oil_level_fc_2, shaded = FALSE, xlim = c(2005, 2007))
