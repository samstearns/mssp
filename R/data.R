# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

hello <- function() {
  print("Hello, world!")
}


# Load SSP PUF files from https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html

# Load Libraries -----------------------------------------------------------------------------------------------
library(RCurl)
library(ggplot2)

# Helper function to downlaod PUF files from CMS
LoadPUF <- function(address) {

  x <- getURL(address)
  df <- read.csv(text = x)
  return (df)
}

# Load the PUF files from the web. Note: 2013 data has a different format than 2014 and 2015
aco.2013 <- LoadPUF("https://data.cms.gov/api/views/475s-fzi7/rows.csv?accessType=DOWNLOAD&bom=true")
aco.2014 <- LoadPUF("https://data.cms.gov/api/views/888h-akbg/rows.csv?accessType=DOWNLOAD&bom=true")
aco.2015 <- LoadPUF("https://data.cms.gov/api/views/7rrf-3gxr/rows.csv?accessType=DOWNLOAD&bom=true")
aco.2016 <- LoadPUF("https://data.cms.gov/api/views/i3xi-3vkx/rows.csv?accessType=DOWNLOAD&bom=true")


# Add additional variables --------------------------------------------------------------------------------------

# Population-weighted HCC Score
aco.2015$CMS_HCC_RiskScore_PY <- (aco.2015$CMS_HCC_RiskScore_DIS_PY * aco.2015$N_AB_Year_DIS_PY +
                                    aco.2015$CMS_HCC_RiskScore_ESRD_PY * aco.2015$N_AB_Year_ESRD_PY +
                                    aco.2015$CMS_HCC_RiskScore_AGDU_PY * aco.2015$N_AB_Year_AGED_Dual_PY +
                                    aco.2015$CMS_HCC_RiskScore_AGND_PY * aco.2015$N_AB_Year_AGED_NonDual_PY) / aco.2015$N_AB;

aco.2014$CMS_HCC_RiskScore_PY <- (aco.2014$CMS_HCC_RiskScore_DIS_PY * aco.2014$N_AB_Year_DIS +
                                    aco.2014$CMS_HCC_RiskScore_ESRD_PY * aco.2014$N_AB_Year_ESRD +
                                    aco.2014$CMS_HCC_RiskScore_AGDU_PY * aco.2014$N_AB_Year_AGED_Dual +
                                    aco.2014$CMS_HCC_RiskScore_AGND_PY * aco.2014$N_AB_Year_AGED_NonDual) / aco.2014$N_AB;
