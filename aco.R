# Load SSP PUF files. Downloaded from https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html

# Libraries
library(RCurl)
library(ggplot2)

LoadPUF <- function(address) {
  # Helper function to downlaod PUF files from CMS
  x <- getURL(address)
  df <- read.csv(text = x)
  return (df)
}

# Load the PUF files from the web. Note: 2013 data has a different format than 2014 and 2015
aco.2013 <- LoadPUF("https://data.cms.gov/api/views/475s-fzi7/rows.csv?accessType=DOWNLOAD&bom=true")
aco.2014 <- LoadPUF("https://data.cms.gov/api/views/888h-akbg/rows.csv?accessType=DOWNLOAD&bom=true")
aco.2015 <- LoadPUF("https://data.cms.gov/api/views/7rrf-3gxr/rows.csv?accessType=DOWNLOAD&bom=true")

# Filter for track 1 ACOs
track1.2013 <- aco.2013[aco.2013$Track.2.ACO == 0,]
track1.2014 <- aco.2014[aco.2014$Track1 == 1,]

# ACO ID, Name, Admits, SNF, ED Visits. Add # Benes, Quality Score, and HCC from 2014
rates.2013 <- track1.2013[, c(1, 2, 52,  62, 63)]
rates.2014 <- track1.2014[, c(1, 2, 8, 14, 90, 100, 101)]

# Get SNF utilization trend
trend <- merge(rates.2013, rates.2014, by.x ='ACO.Identifier', by.y = 'ACO_Num')
trend[,6] <- NULL
colnames(trend) <- c("ID", "NAME", "IP.2013", "SNF.2013", "ED.2013", "2014 Benes", "Qual 2014",  "IP.2014", "SNF.2014", "ED.2014")
trend["SNF.trend"] <- trend["SNF.2014"] / trend["SNF.2013"] - 1
trend["IP.trend"] <- trend["IP.2014"] / trend["IP.2013"] - 1 
trend["ED.trend"] <- trend["ED.2014"] / trend["ED.2013"] -1 

# Re-order columns
# TODO: Add population size, rrs Per_Capita_Exp_TOTAL_PY

attach(trend)
hist(SNF.trend)
summary(SNF.trend)
summary(IP.trend)
summary(ED.trend)

plot(SNF.2013, SNF.trend)

plot(trend["SNF.2013"], trend["SNF.2014"])