library(RGoogleAnalytics)
library(lubridate)
library(RForcecom)

source('LeadsLoaderParisWebsite.R')

dat <- parisWeb[as.Date(parisWeb$CreatedDate) >= "2014-02-01",]

dat <- aggregate(Id~year(as.Date(CreatedDate))+sprintf("%02d",month(as.Date(CreatedDate))),dat, FUN = length)

dat$date <- paste(dat[,1],dat[,2],sep="")

dat <- dat[order(dat$date),]
 
# dat <- aggregate(Id~paste(
#   year(as.Date(CreatedDate)),
#   month(as.Date(CreatedDate)),
#   sep="-"),dat, FUN = length)

#dat <- dat[-max(dat$Id),]

tsessions <- ts(dat$Id,start = c(2014, 2), frequency = 12)

plot(tsessions)
start(tsessions)

library(forecast)

opar <- par(no.readonly=TRUE)
par(mfrow=c(2,2))
ylim <- c(min(tsessions), max(tsessions))
plot(tsessions, main="Raw time series")
plot(ma(tsessions, 3), main="Simple Moving Averages (k=3)", ylim=ylim)
plot(ma(tsessions, 7), main="Simple Moving Averages (k=7)", ylim=ylim)
plot(ma(tsessions, 15), main="Simple Moving Averages (k=15)", ylim=ylim)
par(opar)


ltsessions <- log(tsessions)
#plot(ltsessions, ylab="log(tsessions)")
fit <- stl(ltsessions, s.window="period")
plot(fit)
fit$time.series
exp(fit$time.series)



par(mfrow=c(2,1))

monthplot(tsessions, xlab="", ylab="")
seasonplot(tsessions, year.labels="TRUE", main="",col = c("red","blue","green","purple"))

