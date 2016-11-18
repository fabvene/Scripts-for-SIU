source('LeadsLoaderParis.R')
dat <- parisLeads

reg <- dat[dat$Status=="REG",]

library(lubridate)
mytable <- aggregate(Id ~ month(as.Date(reg$Start_Date__c),label=T) + year(as.Date(reg$Start_Date__c)), data=reg, FUN=length)

names(mytable) <- c("Month","Year","IntakeSize")

library(ggplot2)

p <- ggplot(mytable, aes(x=Month, y=IntakeSize, col=Year, group=Year)) +
  geom_line(size = 1) + geom_point() +
  scale_color_gradientn(colours = rainbow(10))


p1 <- ggplot(mytable, aes(x=Year, y=IntakeSize, fill=Year)) +
  geom_bar(stat="identity") +
  scale_fill_gradientn(colours = rainbow(4))

p

p1

aggregate(IntakeSize~Year,data=mytable,sum)
