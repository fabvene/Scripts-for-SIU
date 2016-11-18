Sys.setenv(LANG = "en") # make sure R speaks English

# Salesforce data ----------


source('LeadsLoaderAllWebsites.R')

# dat <- read.table('New Site - Sessions vs Leads.csv',sep=";")
# dat <- dat[,-4]
# colnames(dat) <- c(as.character(dat[1,1]), as.character(dat[1,2]),as.character(dat[1,3]))
# dat <- dat[-1,]
# row.names(dat) <- dat[,1]
# dat <- dat[,-1]

SF.data <- AllWeb[AllWeb$LeadSource == "New Schiller Site",]



SF.data <- aggregate(Id~as.Date(CreatedDate),SF.data,length)
names(SF.data) <- c("date","leads")


# Google Analytics --------------------------


library(RGoogleAnalytics)
# Auth('210315443788-gbjlu6kh0ms3bp1tjr24ovreidnt9dr1.apps.googleusercontent.com', 'fjnTQ0VnAUFmfQFhNo-iY9RR')

load("oauth_token")
# oauth_token$init_credentials()
profiles <- GetProfiles(oauth_token)


query.list <- Init(start.date = as.character('2016-05-15'),
                   end.date = as.character(today()),
                   dimensions = "ga:date",
                   #metrics = "ga:goalCompletionsAll",
                   #segments = "sessions::condition::ga:goalCompletionsAll>0",
                   metrics = "ga:sessions",
                   max.results = 90000,
                   table.id = "ga:119771357") # Choose appropriate table.id from GA profile

# Create the query object
ga.query <- QueryBuilder(query.list)

GA.data <- GetReportData(ga.query, oauth_token,paginate_query = F)



GA.data$date <- as.Date(GA.data$date, format='%Y%m%d')

# Merge ---------------------------

dat <- merge(GA.data,SF.data,by = 'date')


# dat$sessions <- as.character(dat$sessions)
# dat$leads <- as.character(dat$leads)
# 
# dat$sessions <- as.integer(dat$sessions)
# dat$leads <- as.integer(dat$leads)

dat$rate <- round((dat$leads / dat$sessions),2)


summary(dat)

cor(dat$sessions,dat$leads)

var(dat$sessions); sd(dat$sessions); 
var(dat$leads); sd(dat$leads)

hist(dat$sessions, main='Sessions')
hist(dat$leads, main='Leads')


dat.best <- subset(dat, sessions > median(sessions) & leads > median (leads))
summary(dat.best)

dat.recent <- dat[dat$date > today()-15,]
summary(dat.recent)

library(car)
scatterplotMatrix(dat)
scatterplotMatrix(dat.best)
scatterplotMatrix(dat.recent)


library(ggplot2)

p1 <- ggplot(dat, aes(x= as.Date(dat$date, format='%d/%m/%Y'), y = sessions)) + geom_line() +geom_smooth(color='green') + xlab(NULL)
p2 <- ggplot(dat, aes(x= as.Date(dat$date, format='%d/%m/%Y'), y = leads)) + geom_line() +geom_smooth() + xlab(NULL)

library(grid)

grid.newpage()
grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size = "last"))



x <- as.Date(dat$date,format='%d/%m/%Y')
y1 <- dat$sessions
y2 <- dat$leads
par(mar=c(5,4,4,5)+.1)
plot(x,y1,type="l",col="red", lwd=1.2)
par(new=TRUE)
plot(x, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="", lwd=1.2)
axis(4)
mtext("y2",side=4,line=3)
legend("bottomleft",col=c("red","blue"),lty=1,legend=c("Sessions","Leads"))
grid()



# Days -------


library(lubridate)

# dat$Day <- weekdays(as.Date(dat$date))

pippo <- aggregate(leads~weekdays(as.Date(dat$date,format='%d/%m/%Y')), dat,sum)
names(pippo) <- c("Day","Leads")
pippo$Day <- as.factor(ordered(pippo$Day, levels=c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")))
pippo <- pippo[order(pippo$Day, decreasing = F),]
pippo$CumSum <- cumsum(pippo$Leads)
pippo$CumPerc <- round(pippo$CumSum / sum(pippo$Leads),2)
plot(pippo$Day,pippo$Leads,type="l")



pluto <- aggregate(rate~weekdays(as.Date(dat$date,format='%d/%m/%Y')), dat,mean)
names(pluto) <- c("Day","Rate")
pluto$Day <- as.factor(ordered(pluto$Day, levels=c("lundi","mardi","mercredi","jeudi","vendredi","samedi","dimanche")))

plot(pluto,type="l")


# plot(aggregate(Leads~weekdays(as.Date(dat$date),format='%d/%m/%Y')), dat,sum)[,2],type="o")