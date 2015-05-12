## Libraries ####
library(magrittr)
library(dplyr)

library(rvest)
library(XML)

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
# dat <- read.csv("siu.csv", na.strings=c(""))
# dat <- dat[-c(631:639),]
# for (i in 2:length(dat$Police.Service)){
#     if(is.na(dat$Police.Service[i])){
#       dat$Police.Service[i] = dat$Police.Service[i-1] 
#     }
# }
# dat$Year <- as.numeric(substr(dat$Period, 6, 10))

## Get city populations ####
# http://en.wikipedia.org/wiki/List_of_cities_in_Ontario
# class = wikitable

ontCities <- html("http://en.wikipedia.org/wiki/List_of_cities_in_Ontario")

# Wikipedia table has span elements (class = "sortkey") that need to be omitted
removeNodes(getNodeSet(ontCities,"//table/tr/td/span"))
# Wikipedia table has sup elements (class = "reference") that need to be omitted
removeNodes(getNodeSet(ontCities,"//table/tr/td/sup"))

ontCities %>% html_node("table") %>% html_table -> cities
head(cities)
str(cities)

names(cities) <- gsub("\\[.\\]", "", names(cities)) %>% 
  gsub("\\(2006\\)", "2006", .)%>%
  gsub("\\(2011\\)", "2011", .)%>%
  gsub("\\n", " ", .) %>% 
  gsub("\\(.*\\)", "", . ) %>%
  gsub(" $", "", .) %>% make.names

cities <- head(cities, -1)

## Get town populations ####

ontTowns <- html("http://en.wikipedia.org/wiki/List_of_towns_in_Ontario")

# Wikipedia table has span elements (class = "sortkey") that need to be omitted
removeNodes(getNodeSet(ontTowns,"//table/tr/td/span"))
# Wikipedia table has sup elements (class = "reference") that need to be omitted
removeNodes(getNodeSet(ontTowns,"//table/tr/td/sup"))

ontTowns %>% html_node("table") %>% html_table -> towns
head(towns)
str(towns)

names(towns) <- gsub("\\[.\\]", "", names(towns)) %>% 
  gsub("\\(2006\\)", "2006", .)%>%
  gsub("\\(2011\\)", "2011", .)%>%
  gsub("\\n", " ", .) %>% 
  gsub("\\(.*\\)", "", . ) %>%
  gsub(" $", "", .) %>% make.names

towns <- head(towns, -1)

munis <- merge(cities, towns, by=names(cities)[c(1:2, 4:8)], all=T)
head(munis)

nameMatch <- lapply(munis$Name, function(x) agrep(x, dat$Police.Service))

readkey <- function()
{
  cat ("Press [enter] to continue")
  line <- readline()
}

dat$muniName <- NA
dat$Population.2006 <- NA
dat$Population.2011 <- NA
dat$Pop.density <- NA

for(i in 1:length(nameMatch)){
  print(munis$Name[i])
  if(length(nameMatch[[i]])){
    print(paste(i, " ", munis$Name[i], " ", munis$Population.2006[i], " ", munis$Population.2011[i]))
    dat$muniName[nameMatch[[i]]] <- munis$Name[i]
    dat$Population.2006[nameMatch[[i]]] <- munis$Population.2006[i]
    dat$Population.2011[nameMatch[[i]]] <- munis$Population.2011[i]
    dat$Pop.density[nameMatch[[i]]] <- munis$Population.density[i]
    print(dat[nameMatch[[i]],c("Police.Service", "Total.Cases", "muniName", "Population.2011")])
  }
  # readkey()
}

# Hamilton matches to Hamilton and Milton. Change it back to Hamilton.
dat$muniName[nameMatch[[51]]] <- munis$Name[51]
dat$Population.2006[nameMatch[[51]]] <- munis$Population.2006[51]
dat$Population.2011[nameMatch[[51]]]] <- munis$Population.2011[51]

pop <- function(dat, Police.Service, muniName, pop2006, pop2011, area=NA){
  dat$muniName[dat$Police.Service==Police.Service] <- muniName
  dat$Population.2006[dat$Police.Service==Police.Service] <- pop2006
  dat$Population.2011[dat$Police.Service==Police.Service] <- pop2011
  
  if(!is.na(area)){
    dat$Pop.density[dat$Police.Service==Police.Service] <- as.numeric(gsub(",","",pop2011)) / area 
  }
  
  print(dat[dat$Police.Service==Police.Service,][1,])
  return(dat)
}

dat <- pop(dat, "Chatham-Kent Police Service", "Chatham-Kent", "103,671", "108,177", 2458.09 )
dat <- pop(dat, "Durham Regional Police Service", "Durham Region", "589,850", "589,850", 2523.62)
dat <- pop(dat, "Halton Regional Police Service", "Halton Region", "514,000", "514,000", 967)
dat <- pop(dat, "Leamington Police Service", "Leamington", "28,275", "28,403",261.92)
dat <- pop(dat, "Michipicoten Township Police Service", "Wawa", "3,204", "2,975", 417.78)
dat <- pop(dat, "Niagara Regional Police Service", "Niagara Region", "427,421", "431,346", 1854.25)
dat <- pop(dat, "OPP Grenville County Detachment", "Leeds and Grenville", "96,606", "99,206", 3383.92)
dat <- pop(dat, "OPP Alexandria Detachment", "North Glengarry", "10,635", "10,251", 643.69)
dat <- pop(dat, "OPP Almaguin Highlands Detachment", "Burk's Falls", "967", "967", 3.12)
dat <- pop(dat, "OPP Almaguin Highlands Detachment", "North Kawartha", "2342", "2,289", 776.04)
dat <- pop(dat, "OPP Armstrong Detachment", "Armstrong", "1,216", "1,216", 90.33)
dat <- pop(dat, "OPP Bala Detachment", "Bala", "1,216", "1,216", 90.33)
dat <- pop(dat, "OPP Dutton Detachment", "Dutton/Dunwich", "3821", "3,876", 294.64)
dat <- pop(dat, "OPP Ear Falls Detachment", "Ear Falls", "1153", "1026", 331.03)
dat <- pop(dat, "OPP Chatham-Kent Detachment", "Chatham-Kent", "103,671", "108,177", 2458.09)
dat <- pop(dat, "OPP Elgin County Detachment", "Elgin County", "49,241", "49,556", 1880.90)
dat <- pop(dat, "OPP Elgin County Detachment", "Elgin County", "49,241", "49,556", 1880.90)
dat <- pop(dat, "OPP Elgin Detachment", "Elgin County", "49,241", "49,556", 1880.90)
dat <- pop(dat, "OPP Elk Lake Detachment", "James", "414", "424", 86.19)
dat <- pop(dat, "OPP Emo Detachment", "Emo", "1305", "1252", 203.54)
dat <- pop(dat, "OPP Exeter Detachment", "Emo", "4,785", "4,785", 5.33)
dat <- pop(dat, "OPP Foleyet Detachment", "Foleyet", "193", "193", 11.92)
dat <- pop(dat, "OPP Frontenac Detachment", "Frontenac County", "143,865", "149,738", 3787.79)
dat <- pop(dat, "OPP Gogama Detachment", "Gogama", "394", "394")
dat <- pop(dat, "OPP Gore Bay Detachment", "Gore Bay", "924", "850", 5.27)
dat <- pop(dat, "OPP Gore Bay Detachment", "Gore Bay", "924", "850", 5.27)




# ## Check Unmathched Municipalities
# "Bradford West Gwillimbury"
# "Brampton"
# "Kitchener"
# "Niagara Falls"
# "Newmarket"
# "Oakville"
# "Pickering"
# "Richmond Hill"
# "St. Catharines"
# 
# ## Mismatched municipalities
# "Brant" -> Brantford Police Service
# "Erin" -> OPP Dufferin Detachment
# "Erin" -> OPP Prince Edward Detachment
# 
# "Milton" -> Hamilton Police Service
# "Milton" -> OPP Hamilton Detachment
# 
# "Mono" -> OPP Moosonee Detachment

write.csv(dat, "test.csv", row.names=F)
