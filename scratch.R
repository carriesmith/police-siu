source('dataprep.R')

unique(dat[, c("Police.Service", "muniName", "Population.2011")])




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







wikiSummary <- html("http://en.wikipedia.org/wiki/Ontario_Provincial_Police")
nodeSet <- getNodeSet(wikiSummary,"//ul/li/a")

data.frame(place=xmlSApply(nodeSet[1:197], xmlValue), 
           link=paste("http://en.wikipedia.org", xmlSApply(nodeSet[1:197], function(x) xmlGetAttr(x, "href")), sep=""))



xpathApply(wikiSummary, "//a[@href]", xmlGetAttr, "href")

nodeSet[1:100]

wikiSummary <- html("http://en.wikipedia.org/wiki/Leamington,_Ontario")
wikiSummary <- html("http://en.wikipedia.org/wiki/Greenstone,_Ontario")
wikiSummary <- html("http://en.wikipedia.org/wiki/Marathon,_Ontario")
wikiSummary <- html("http://en.wikipedia.org/wiki/Mattawa,_Ontario")

nodeSet <- getNodeSet(wikiSummary,"//table[@class='infobox geography vcard']/tr")

xmlSApply(nodeSet, xmlValue)[1] %>% gsub("\\n", "", .)

xmlSApply(nodeSet, xmlValue) %>% .[grep("Total",.)] %>% 
  substr(., regexpr("\\n", .), nchar(.)) %>%
  gsub("\\n", "", .)

xmlSApply(nodeSet, xmlValue) %>% .[grep("Municipality.*lower.*\\n.",.)] %>%
 substr(., regexpr("\\n", .), nchar(.)) %>%
  gsub("\\n", "", .)

xmlSApply(nodeSet, xmlValue) %>% .[grep("Density",.)] %>%
  substr(., regexpr("\\n", .), regexpr("..km", .)) %>%
  gsub("\\n", "", .)
  


   
  


wikiSummary <- html("http://en.wikipedia.org/wiki/Leamington,_Ontario")


