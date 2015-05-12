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

nameMatch <- lapply(munis$Name, function(x) grep(x, dat$Police.Service))

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
# dat$muniName[nameMatch[[51]]] <- munis$Name[51]
# dat$Population.2006[nameMatch[[51]]] <- munis$Population.2006[51]
# dat$Population.2011[nameMatch[[51]]] <- munis$Population.2011[51]

write.csv(dat, "dat1.csv", row.names=F)

## PART II ####
dat <- read.csv("dat1.csv", stringsAsFactors = F)

## Assessing Municipality Matching So Far  ####

# All unique rows
dat %>% select(., Police.Service, muniName, Population.2006, Population.2011, Pop.density) %>%
  unique

# All unmatched Police.Services
dat %>% select(., Police.Service, muniName, Population.2006, Population.2011, Pop.density) %>%
  subset(., is.na(muniName)) %>%
  unique

# Note: Most unmatched are OPP. Scrape the OPP Wiki page (http://en.wikipedia.org/wiki/Ontario_Provincial_Police)
# by going to each link under "Detachments" and downloading population & pop density if available

oppSummary <- html("http://en.wikipedia.org/wiki/Ontario_Provincial_Police")
nodeSet <- getNodeSet(oppSummary,"//ul/li/a")

scrapeWiki <- data.frame(place=xmlSApply(nodeSet[28:197], xmlValue), 
                         link=paste("http://en.wikipedia.org", xmlSApply(nodeSet[28:197], function(x) xmlGetAttr(x, "href")), sep=""))

scrapeWiki$muniName <- NA
scrapeWiki$Population.2011 <- NA
scrapeWiki$Pop.density <- NA

for( i in 1:nrow(scrapeWiki)){
  wikiSummary <- tryCatch(html(as.character(scrapeWiki$link[i])), error = function(e) NA)
  nodeSet <- tryCatch(getNodeSet(wikiSummary,"//table[@class='infobox geography vcard']/tr"), error = function(e) NA)
  print(i)
  if(!is.null(nodeSet) && !is.na(nodeSet)){
    popIndex <- xmlSApply(nodeSet, xmlValue) %>% grep("Population",.)
    scrapeWiki$muniName[i] <- xmlSApply(nodeSet, xmlValue)[1] %>% gsub("\n", "", .)
    if(length(popIndex)>0) {
      # print(xmlSApply(nodeSet, xmlValue)[(popIndex+1)])
      xmlSApply(nodeSet, xmlValue)[popIndex+1] %>%
        gsub("\\([^)]*\\)", "", .) %>%
        gsub("^[^\n]*\n([^\n]*)\n.*$", "\\1", .) -> scrapeWiki$Population.2011[i]
      
      # print(xmlSApply(nodeSet, xmlValue)[(popIndex+2)])
      xmlSApply(nodeSet, xmlValue)[popIndex+2] %>%
        substr(., regexpr("\\n", .), regexpr("..km", .)) %>%
        gsub("\\n", "", .) -> scrapeWiki$Pop.density[i]
      # print(scrapeWiki[i,])
    }
  }
}

nameMatch <- lapply(scrapeWiki$place, function(x) grep(x, dat$Police.Service))

for(i in 1:length(nameMatch)){
#   print(scrapeWiki$place[i])
  if(length(nameMatch[[i]])){
    # If the population hasn't already been filled in (I like this scrape a little less)    
     if(dat$Population.2011[nameMatch[[i]]] %>% is.na %>% sum != 0){
       dat$muniName[nameMatch[[i]]] <- scrapeWiki$muniName[i]
       dat$Population.2011[nameMatch[[i]]] <- scrapeWiki$Population.2011[i]
       dat$Pop.density[nameMatch[[i]]] <- scrapeWiki$Pop.density[i]
     }
     print(dat[nameMatch[[i]],c("Police.Service", "Total.Cases", "muniName", "Population.2011")])
  }
}

## Assessing Municipality Matching So Far  ####

# All unique rows
dat %>% select(., Police.Service, muniName, Population.2006, Population.2011, Pop.density) %>%
  unique

# All unmatched Police.Services
dat %>% select(., Police.Service, muniName, Population.2006, Population.2011, Pop.density) %>%
  subset(., is.na(muniName)) %>%
  unique

write.csv(dat, "dat2.csv", row.names=F)

## PART II ####
dat <- read.csv("dat2.csv", stringsAsFactors = F)

## PART III ####
# For the rest we'll do it the hard way

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

dat <- pop(dat, "Peel Regional Police Service", "Peel Region", "1,296,814", "1,296,814", 1246.89)
dat <- pop(dat, "Durham Regional Police Service", "Durham Region", "589,850", "589,850", 2523.62)
dat <- pop(dat, "Halton Regional Police Service", "Halton Region", "514,000", "514,000", 967)
dat <- pop(dat, "York Regional Police Service", "York Region", "892,712", "1,032,524", 1762.17)

dat <- pop(dat, "Michipicoten Township Police Service", "Wawa", "3,204", "2,975", 417.78)
dat <- pop(dat, "OPP Grenville County Detachment", "Leeds and Grenville", "96,606", "99,206", 3383.92)
dat <- pop(dat, "Niagara Regional Police Service", "Niagara Region", "427,421", "431,346", 1854.25)
dat <- pop(dat, "OPP Bala Detachment", "Bala", "1,216", "1,216", 90.33)
dat <- pop(dat, "OPP Elgin Detachment", "Elgin County", "49,241", "49,556", 1880.90)
dat <- pop(dat, "West Grey Police Service", "West Grey", "12,193", "12,286", 876.02)

## GOOD ENOUGH! ####

write.csv(dat, "datFinal.csv", row.names=F)
