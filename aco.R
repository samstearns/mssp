# Load SSP PUF files. Downloaded from https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html

setwd("/Users/sjs/Dropbox/dev/git/mssp/")

aco.2013 <-read.csv("./2013_Shared_Savings_Program_Accountable_Care_Organizations__ACO__PUF.csv")
aco.2014 <-read.csv("./2014_Shared_Savings_Program__SSP__Accountable_Care_Organizations__ACO__PUF.csv")

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
# TODO: Add population size, rrs
Per_Capita_Exp_TOTAL_PY

attach(trend)
hist(SNF.trend)
summary(SNF.trend)
summary(IP.trend)
summary(ED.trend)


plot(SNF.2013, SNF.trend)

plot(trend["SNF.2013"], trend["SNF.2014"])

library(ggplot2)


drawTurnipGraph <- function(input.data,  y_string, title) {
  ggplot(input.data, aes_string(x = "1", y = y_string)) +
    geom_dotplot(binaxis = "y", stackdir = "center", dotsize=0.75) + 
    ggtitle(title) + 
    theme(plot.title = element_text(lineheight=.8, face="bold")) + 
    labs(y = "Rate per 1,000") + 
    theme(axis.text.x = element_blank())
}

drawTurnipGraph(IP.2014, "Total.inpatient.spending.per.assigned.beneficiary", "Short Term Inpatient Utilization by ACO, \n2013 Performance Year")