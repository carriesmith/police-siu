## Libraries ####
library(magrittr)


## Read Data ####

dat <- read.csv("siu.csv", na.strings=c(""))

head(dat, n = 12)

# One of the police services is duplicated!
dat %$% xtabs(~Police.Service) %>% unique
dat$Police.Service[duplicated(dat$Police.Service)] %>% na.omit
grep("OPP Espanola Detachment", dat$Police.Service)

dat[613:648, 1:3]
# The block dat[631:639,] seems to be an erroneous duplication, "OPP Espanola Detachment" appears twice

dat <- dat[-c(631:639),]

# Checked that every police service has all years in table
xtabs(~Period, data=dat)
unique(xtabs(~Period, data=dat))

# Repeat each police service name for each row (year)
dat$Police.Service %>% na.omit %>% rep(each=9) -> Police.Service

# Double check that they match --> as many TRUE as there are unique police services = 244
sum(Police.Service==dat$Police.Service, na.rm=T)

# Replace column in data.frame, now police service name appears on every row
dat$Police.Service <- Police.Service

head(dat, n=20)


# Add a numeric year variable corresponding to the end of the reporting period
dat$Year <- substr(dat$Period, 6, 10)
