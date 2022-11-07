set.seed(4002)
n <- 200
wn_sim <- rnorm(n)

plot(ts(wn_sim))

acf(wn_sim)


ma1_sim <- arima.sim(n = n, model = list(ma = -0.8))
acf(ma1_sim)
abline(h = 1.96 * sqrt(1 + 2 * ((-0.8)/(1 + 0.8^2))^2) * c(1,-1)/sqrt(n), lty = 2, col = 2)


Box.test(wn_sim, lag = 10)
qchisq(p=0.95, df=10)


Box.test(ma1_sim, lag = 10)
qchisq(p=0.95, df=10)


# Example AR(2): y_t = 0.2*y_(t-1) + 0.25*y_(t-2) + e_t
z <- seq(-3, 3, by=0.1)
plot(z, 1-0.2*z-0.25*z^2, type="l")
grid()
abline(h=0)

y <- arima.sim(n=200, model =list(ar=c(0.2, 0.25)))
plot(y)
grid()
acf(y)

# Redundant ARMA(1,1)
y <- arima.sim(n=200, model =list(ar=c(0.5), ma=-0.5))
acf(y)

# install.packages("TSA")
data("oil.price", package = "TSA")
plot(oil.price)
grid()

log_oil_price <- log(oil.price)
plot(log_oil_price)
grid()

log_oil_price_diff <- diff(log_oil_price)
plot(log_oil_price_diff)
grid()


set.seed(1)
y <- arima.sim(list(order = c(0, 1, 1), ma = 0.6), n = 60, n.start = 50)
plot(y, type = "o")
acf(y)
plot(diff(y), type = "o")
acf(diff(y))
