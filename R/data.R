#' Downloads PUF files from CMS website (https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html)
#'
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_puf_file <- function(year) {

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (year == 2013) {
    # Note 2013 has different data format
    address <- "https://data.cms.gov/api/views/475s-fzi7/rows.csv?accessType=DOWNLOAD&bom=true"
  } else if (year == 2014) {
    address <- "https://data.cms.gov/api/views/888h-akbg/rows.csv?accessType=DOWNLOAD&bom=true"
  } else if (year == 2015) {
    address <- "https://data.cms.gov/api/views/7rrf-3gxr/rows.csv?accessType=DOWNLOAD&bom=true"
  } else if (year == 2016) {
    address <- "https://data.cms.gov/api/views/i3xi-3vkx/rows.csv?accessType=DOWNLOAD&bom=true"
  } else {
    address <- "https://data.cms.gov/api/views/475s-fzi7/rows.csv?accessType=DOWNLOAD&bom=true"
  }

  df <- LoadPUF(address);

  if (year == 2013) {
    # Note: 2013 data has a different format than 2014 and 2015
  }

  df$Performance_Year <- year

  df$CMS_HCC_RiskScore_PY <- (df$CMS_HCC_RiskScore_DIS_PY * df$N_AB_Year_DIS_PY +
       df$CMS_HCC_RiskScore_ESRD_PY * df$N_AB_Year_ESRD_PY +
       df$CMS_HCC_RiskScore_AGDU_PY * df$N_AB_Year_AGED_Dual_PY +
       df$CMS_HCC_RiskScore_AGND_PY * df$N_AB_Year_AGED_NonDual_PY) / df$N_AB


  return (df)
}

# Load the PUF files from the web.
LoadPUF <- function(address) {
  x <- RCurl::getURL(address)
  df <- read.csv(text = x)
  return (df)
}




# Add additional variables --------------------------------------------------------------------------------------

# Population-weighted HCC Score
#aco.2015$CMS_HCC_RiskScore_PY <- (aco.2015$CMS_HCC_RiskScore_DIS_PY * aco.2015$N_AB_Year_DIS_PY +
#                                    aco.2015$CMS_HCC_RiskScore_ESRD_PY * aco.2015$N_AB_Year_ESRD_PY +
#                                    aco.2015$CMS_HCC_RiskScore_AGDU_PY * aco.2015$N_AB_Year_AGED_Dual_PY +
#                                    aco.2015$CMS_HCC_RiskScore_AGND_PY * aco.2015$N_AB_Year_AGED_NonDual_PY) / aco.2015$N_AB;

#aco.2014$CMS_HCC_RiskScore_PY <- (aco.2014$CMS_HCC_RiskScore_DIS_PY * aco.2014$N_AB_Year_DIS +
#                                    aco.2014$CMS_HCC_RiskScore_ESRD_PY * aco.2014$N_AB_Year_ESRD +
 #                                   aco.2014$CMS_HCC_RiskScore_AGDU_PY * aco.2014$N_AB_Year_AGED_Dual +
 #                                   aco.2014$CMS_HCC_RiskScore_AGND_PY * aco.2014$N_AB_Year_AGED_NonDual) / aco.2014$N_AB;
