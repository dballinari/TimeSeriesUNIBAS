# R Examples - Chapter 6: Forecasting
rm(list=ls())


# Forecast US DGP ---------------------------------------------------------

data("USMacroG", package = "AER")
gdp <- USMacroG[, "gdp"]

plot(ts.union(levels = gdp, logs = log(gdp),
              growth = 100*diff(log(gdp))), main = "")

dlgdp <- 100 * diff(log(gdp))

acf(dlgdp)
pacf(dlgdp)

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

# use external library
library("forecast")
dlgdp_fc <- forecast(dlgdp_ar1, h = 4)
dlgdp_fc