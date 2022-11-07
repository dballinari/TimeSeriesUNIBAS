# install.package("AER")

plot(AirPassengers)

plot(UKDriverDeaths)

data("bev", package="tseries")
plot(bev)

data("MarkPound", package="AER")
plot(MarkPound)


library("tseries")
smi <- get.hist.quote(instrument = "^ssmi",
                      start = "2014-01-01", end = "2022-08-31",
                      quote = "Close", provider = "yahoo", compression = "d",
                      retclass = "zoo", quiet = TRUE, drop = FALSE)
lsmi <- log(smi)
dlsmi <- diff(lsmi) # returns
rsmi <- 100 * dlsmi # percentage returns
names(rsmi) <- "rsmi"
plot(merge(smi, lsmi, rsmi), main = "",
     xlab = "Time", ylab = c("prices", "log-prices", "returns"))



tsp(AirPassengers)
diff(AirPassengers)
plot(AirPassengers)
plot(diff(AirPassengers))

library(zoo)
as.zoo(AirPassengers)
