set.seed(1) ## simulation is reproducible
e <- rnorm(60)
y <- cumsum(c(0, e))
y <- ts(y, start = 0)
plot(y, type = "o")
abline(h = 0, col = "lightgray")

set.seed(2) ## simulation is reproducible
e <- rnorm(61)
y <- rep(0, 60)
for(i in 1:60) y[i] <- e[i+1] + 0.5 * e[i]
y <- ts(y, start = 1)
plot(y, type = "o")
abline(h = 0, col = "lightgray")
