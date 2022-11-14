library(forecast)
rm(list=ls())


set.seed(4002)
ma1_sim <- arima.sim(n = 200, model = list(ma = -0.9))
tsdisplay(ma1_sim)

ar2_sim <- arima.sim(n = 200, model = list(ar = c(1, -0.6)))
tsdisplay(ar2_sim)

arma11_sim <- arima.sim(n = 200, model = list(ar = 0.6, ma=0.3))
tsdisplay(arma11_sim)




data("oil.price", package = "TSA")
oil <- log(oil.price)
oil.ret <- diff(oil)
plot(ts.union(exp(oil), oil, diff(oil)), main = "")
grid()
tsdisplay(oil.ret)

# AR1 - YW
set.seed(0)
ar1 <- arima.sim(model = list(ar = 0.9), n = 120)
acf(ar1, plot = FALSE)$acf[2]
ar(ar1, order.max = 1, aic = FALSE, method = "yule-walker")

ar1 <- ar1 - mean(ar1)
sum(ar1*lag(ar1,1))/sum(ar1^2)

# AR2 - YW
set.seed(1)
ar2 <- arima.sim(model = list(ar = c(1, -0.6)), n = 120)
ar(ar2, order.max = 2, aic = FALSE, method = "yule-walker")

ar2 <- ar2 - mean(ar2)
gamma0 <- sum(ar2^2)/120
gamma1 <- sum(ar2*lag(ar2,1))/120
gamma2 <- sum(ar2*lag(ar2,2))/120

G <- matrix(c(gamma0, gamma1, gamma1, gamma0), ncol=2)
g <- matrix(c(gamma1, gamma2), ncol=1)
phi <- solve(G)%*%g



# Least squares for MA(1) -------------------------------------------------
set.seed(111)
y <- arima.sim(model = list(ma=0.6), n=200)


fcn_A <- function(y, theta) {
  n <- length(y)
  A <- 0
  for (j in 0:(n-1)) {
    A <- A + (-theta)^j * y[n-j]
  }
  # A <- sum(y*(-theta)^((n-1):0))
  return(A)
}

fcn_B <- function(y, theta) {
  n <- length(y)
  B <- 0
  for (j in 1:(n-1)) {
    B <- B -j*(-theta)^(j-1) * y[n-j]
  }
  
  # B <- sum(-1*((n-1):1)*(-theta)^((n-2):0)*y)
  return(B)
}

fcn_A_dev <- function(y, theta) {
  A_dev <- fcn_B(y, theta)
  return(A_dev)
}

fcn_B_dev <- function(y, theta) {
  n <- length(y)
  B_dev <- 0
  for (j in 2:(n-1)) {
    B_dev <- B_dev + j*(j-1)*(-theta)^(j-2) * y[n-j]
  }
  return(B_dev)
}

get_h <- function(y, theta) {
  h <- 0
  for (t in 3:200) {
    h <- h + fcn_A(y[1:t], theta)*fcn_B(y[1:t], theta)
  }
  return(2*h)
}

get_h_dev <- function(y, theta) {
  h_dev <- 0
  for (t in 3:200) {
    h_dev <- h_dev + fcn_A_dev(y[1:t], theta)*fcn_B(y[1:t], theta) + fcn_A(y[1:t], theta)*fcn_B_dev(y[1:t], theta)
  }
  return(2*h_dev)
} 


theta <- rep(0, 10)
print(paste("Step", 0, ": theta =", 0, "h(theta) =", get_h(y, 0)))
for (step_i in 2:length(theta)) {
  theta_old <- theta[step_i-1]
  
  theta_new <- theta_old - get_h(y, theta_old)/get_h_dev(y, theta_old)
  
  print(paste("Step", step_i, ": theta =", theta_new, "h(theta) =", get_h(y, theta_new)))
  
  theta[step_i] <- theta_new
}

arima(y, order=c(0,0,1), method="CSS", include.mean = FALSE)



get_sse <- function(y, theta) {
  sse <- 0
  for (t in 2:200) {
    sse <- sse + fcn_A(y[1:t], theta)^2
  }
  return(sse)
}

thetas <- seq(-0.9, 0.9, 0.05)
sse_list <- rep(NA, length(thetas))
for (i in 1:length(thetas)) {
  sse_list[i] <- get_sse(y, thetas[i])
}

plot(thetas, sse_list, type="l")



# Example oil -------------------------------------------------------------

data("oil.price", package = "TSA")
oil.ret <- diff(log(oil.price))
mods <- expand.grid(ar = 0:2, ma = 0:2, mean = c(TRUE, FALSE))
for(i in 1:nrow(mods)) mods$aic[i] <- AIC(
  arima(oil.ret, order = c(mods$ar[i], 0, mods$ma[i]),include.mean = mods$mean[i]))

mods[which.min(mods$aic),]
oil_ma1 <- arima(oil.ret, order = c(0, 0, 1), include.mean = FALSE)


library("forecast")
auto.arima(oil.ret, ic = "aic", stationary = TRUE,
           max.p = 2, max.q = 2, max.P = 0, max.Q = 0,
           stepwise = FALSE, trace = TRUE, approximation = FALSE)


# get residuals:
oil_res <- residuals(oil_ma1) / sqrt(oil_ma1$sigma2)
plot(oil_res)
abline(h=0)

# Box-Pierce Test:
Box.test(oil_res, lag = 5, fitdf = 1)

# Write your own BP test:
n <- length(oil_res)
test_statistic <- n *sum(acf(oil_res, lag.max = 5, plot = FALSE)$acf[2:6]^2)
p_val <- 1-pchisq(q = test_statistic, df = 5 -1)

# Diagnostics for your model:
tsdiag(oil_ma1)

