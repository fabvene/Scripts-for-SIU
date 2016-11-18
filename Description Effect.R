
# Are leasd with Comment of a better quality than average?


#source('LeadsLoaderParis.R')
source('LeadsLoaderParisWebsite.R')

dat <- parisWeb

# Manipulation -------------

dat <- subset(dat, grepl("Website",dat$LeadSource, ignore.case = T))
#dat <- dat[as.Date(dat$CreatedDate)>="2015-01-01",]

dat$Qualy <- ifelse(dat$Status %in% c("REG","Accepted","App In","Paid Tuition"), "YES","NO")

# Description -----------------

table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)) # Qualy Leads vs. Leads with Comment

# round(prop.table(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))),3) # Qualy Leads vs. Leads with Comment - PROP

# round(prop.table(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))[,"FALSE"]),2) # PROP of leads with comments in Qualy [YES] status or not [NO]


# Analysis -----------------

summary(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))) 
# if the Chisq is low (compared to the Number of Cases) there is LOW or NONE association between the 2 variables (Qualy and Comment)

# You can also compare the Expected values...
chisq.test(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)))$expected

# ... with the actual ones
table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))
        

chisq.test(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))) 
# if p-value < 0.05 there is *some* association


# Direction of the association:
table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))[2,1] # Actual DescYES & QualyYES
chisq.test(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)))$expected[2,1] # Expected DescYES & QualyYES

table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))[2,1] - chisq.test(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)))$expected[2,1]
# their difference

# Plots -------------

plot(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)), main="Actual Distribution")

mosaicplot(chisq.test(table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description)))$expected, main="Expected Distribution")



# Notes --------------------------------

# (from J.Verzani, 2nd, p.147)


tbl <- table(Qualified=dat$Qualy,HasNoComment=is.na(dat$Description))

margin.table(tbl,1)

pbottom <- 763/sum(margin.table(tbl,1))  

margin.table(tbl,2)

pleft <- 464/sum(margin.table(tbl,2))

fe <- pbottom * pleft * sum(tbl)
fo <- tbl[1,1]

# fo = actual Desc+QualyYES, fe = expected Desc+QualyYES
c(fo=fo,fe=fe,residual=fo-fe)


fo.t <- tbl
fe.t <- chisq.test(tbl)$expected

# Table of Residuals
(fo.t - fe.t)^2 / fe.t

# Total Residuals
sum((fo.t - fe.t)^2 / fe.t)
