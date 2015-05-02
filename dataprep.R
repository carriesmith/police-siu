## Libraries ####
library(magrittr)
library(dplyr)

## Read Data ####

dat <- read.csv("siu.csv", na.strings=c(""))

head(dat, n = 12)

# One of the police services is duplicated!
dat %$% xtabs(~Police.Service) %>% unique
dat %$% Police.Service[duplicated(Police.Service)] %>% na.omit %>% as.character
grep("OPP Espanola Detachment", dat$Police.Service)

dat[613:648, 1:3]
# The block dat[631:639,] seems to be an erroneous duplication, "OPP Espanola Detachment" appears twice

dat <- dat[-c(631:639),]
dat %$% xtabs(~Police.Service) %>% unique
dat %$% Police.Service[duplicated(Police.Service)] %>% na.omit %>% as.character
dat %$% Police.Service %>% na.omit %>% unique # 244 unique police services

# Checked that every police service has all years in table (e.g. 244 occurrances of each year)
xtabs(~Period, data=dat)
unique(xtabs(~Period, data=dat))

# Repeat each police service name for each row (year)
dat$Police.Service %>% na.omit %>% rep(each=9) -> Police.Service

# Double check that Police Services names line up with original
# --> should be as many TRUE as there are unique police services = 244
sum(Police.Service==dat$Police.Service, na.rm=T)

# Replace column in data.frame, now police service name appears on every row
dat$Police.Service <- Police.Service

head(dat, n=20)

# Add a numeric year variable corresponding to the end of the reporting period
dat$Year <- substr(dat$Period, 6, 10)


## Alternative - Should be identical ####
dat <- read.csv("siu.csv", na.strings=c(""))
dat <- dat[-c(631:639),]
for (i in 2:length(dat$Police.Service)){
    if(is.na(dat$Police.Service[i])){
      dat$Police.Service[i] = dat$Police.Service[i-1] 
    }
}
dat$Year <- as.numeric(substr(dat$Period, 6, 10))

head(dat)

xtabs(~cut(Total.Cases, breaks=c(-1,0,1,2,5,10,20,50,100)) + Year, data=dat)

## Toronto Police Service ####
# Whoa. Toronto definitely stands out. Though our population is enourmous....per cap?
plot(Total.Cases ~ Year, data=dat)
dat[which.max(dat$Total.Cases),]

# Population of Toronto 2.5 million
# Normalize to per-million
dat.to <- dat[dat$Police.Service=="Toronto Police Service",]
dat.to[,3:11] <- dat.to[,3:11]/25

plot(Total.Cases ~ Year, data=dat.to)
plot(Firearm.Injuries ~ Year, data=dat.to)
plot(Firearm.Deaths ~ Year, data=dat.to)
plot(Custody.Injuries ~ Year, data=dat.to)
plot(Custody.Deaths ~ Year, data=dat.to)
plot(Vehicle.Injuries ~ Year, data=dat.to)
plot(Vehicle.Deaths ~ Year, data=dat.to)
plot(Sexual.Assault.Complaints ~ Year, data=dat.to)

fit <- lm(Total.Cases ~ Year, data=dat.to)
summary(fit)

fit <- lm(Custody.Injuries ~ Year, data=dat.to)
summary(fit)

fit <- lm(Sexual.Assault.Complaints ~ Year, data=dat.to)
summary(fit)

## Hamilton ####

grep("Hamilton", dat$Police.Service)
dat$Police.Service[grep("Hamilton", dat$Police.Service)]

# Population of Hamilton .5 million
# Normalize to per-million
dat.ha <- dat[dat$Police.Service=="Hamilton Police Service",]
dat.ha[,3:11] <- dat.ha[,3:11]/5

plot(Total.Cases ~ Year, data=dat.ha)
plot(Firearm.Injuries ~ Year, data=dat.ha)
plot(Firearm.Deaths ~ Year, data=dat.ha)
plot(Custody.Injuries ~ Year, data=dat.ha)
plot(Custody.Deaths ~ Year, data=dat.ha)
plot(Vehicle.Injuries ~ Year, data=dat.ha)
plot(Vehicle.Deaths ~ Year, data=dat.ha)
plot(Sexual.Assault.Complaints ~ Year, data=dat.ha)

fit <- lm(Total.Cases ~ Year, data=dat.ha)
summary(fit)



## London ####

unique(dat$Police.Service[grep("London", dat$Police.Service)])

# Population of London 350 thousand
# Normalize to per-million
dat.lo <- dat[dat$Police.Service=="London Police Service",]
dat.lo[,3:11] <- dat.lo[,3:11]/3.5
plot(Total.Cases ~ Year, data=dat.lo)
fit <- lm(Total.Cases ~ Year, data=dat.lo)
summary(fit)



## Windsor ####

unique(dat$Police.Service[grep("Windsor", dat$Police.Service)])

# Population of Windsor 220 thousand
# Normalize to per-million
dat.win <- dat[dat$Police.Service=="Windsor Police Service",]
dat.win[,3:11] <- dat.win[,3:11]/2.2
plot(Total.Cases ~ Year, data=dat.win)
fit <- lm(Total.Cases ~ Year, data=dat.win)
summary(fit)
