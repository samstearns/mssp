
standard_fields_2013 <- c(
  "ACO_Num", "ACO_Name", "Start_Date", "Adv_Pay", "Adv_Pay_Amt", "QualScore", "QualPerfShare", "FinalShareRate",
  "MinSavPerc", "ABtotBnchmk", "ABtotExp", "BnchmkMinExp", "N_AB",
  "N_Ben_Age_0_64",
  "N_Ben_Age_65_74",
  "N_Ben_Age_75_84",
  "N_Ben_Age_85plus",
  "N_Ben_Female",
  "N_Ben_Male",
  "N_Ben_Race_White",
  "N_Ben_Race_Black",
  "N_Ben_Race_Asian",
  "N_Ben_Race_Hisp",
  "N_Ben_Race_Native",
  "N_Ben_Race_Other",
  "CapAnn_INP_All",
  "CapAnn_INP_S_trm",
  "CapAnn_INP_L_trm",
  "CapAnn_INP_Rehab",
  "CapAnn_INP_Psych",
  "CapAnn_HSP",
  "CapAnn_SNF",
  "CapAnn_INP_Other",
  "CapAnn_PB",
  "CapAnn_AmbPay",
  "CapAnn_HHA",
  "CapAnn_DME",
  "ADM",
  "ADM_S_Trm",
  "ADM_L_Trm",
  "ADM_Rehab",
  "ADM_Psych",
  "chf_adm",
  "copd_adm",
  "pneu_adm",
 "readm_Rate_1000",
 "prov_Rate_1000",
 "P_SNF_ADM",
 "P_EDV_Vis",
 "P_EDV_Vis_HOSP",
 "P_CT_VIS",
 "P_MRI_VIS",
 "P_EM_Total",
 "P_EM_PCP_Vis",
 "P_EM_SP_Vis",
 "P_Nurse_Vis",
 "P_FQHC_RHC_Vis",
 "N_CAH",
 "N_FQHC",
 "N_RHC",
 "N_ETA",
 "N_Fac_Other",
 "N_PCP",
 "N_Spec",
 "N_NP",
 "N_PA",
 "N_CNS",
 "CMS_HCC_RiskScore_ESRD_BY1",
 "CMS_HCC_RiskScore_DIS_BY1",
 "CMS_HCC_RiskScore_AGDU_BY1",
 "CMS_HCC_RiskScore_AGND_BY1",
 "CMS_HCC_RiskScore_ESRD_BY2",
 "CMS_HCC_RiskScore_DIS_BY2",
 "CMS_HCC_RiskScore_AGDU_BY2",
 "CMS_HCC_RiskScore_AGND_BY2",
 "CMS_HCC_RiskScore_ESRD_BY3",
 "CMS_HCC_RiskScore_DIS_BY3",
 "CMS_HCC_RiskScore_AGDU_BY3",
 "CMS_HCC_RiskScore_AGND_BY3",
 "CMS_HCC_RiskScore_ESRD_PY",
 "CMS_HCC_RiskScore_DIS_PY",
 "CMS_HCC_RiskScore_AGDU_PY",
 "CMS_HCC_RiskScore_AGND_PY"
)


#' Downloads PUF files from CMS website (https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html)
#'
#' @param year MSSP performance year.
#' @param enhance_data Flag to standardize variables.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_puf_file <- function(year, enhance_data=TRUE, standardize_data=FALSE) {

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (year == 2013) {
    # Note 2013 has different data format
    address <- "https://data.cms.gov/api/views/faep-t7cf/rows.csv?accessType=DOWNLOAD"
  } else if (year == 2014) {
    address <- "https://data.cms.gov/api/views/888h-akbg/rows.csv?accessType=DOWNLOAD"
  } else if (year == 2015) {
    address <- "https://data.cms.gov/api/views/7rrf-3gxr/rows.csv?accessType=DOWNLOAD"
  } else if (year == 2016) {
    address <- "https://data.cms.gov/api/views/3jk5-q6dr/rows.csv?accessType=DOWNLOAD"
  } else if (year == 2017) {
    address <- "https://data.cms.gov/api/views/gk7c-vejx/rows.csv?accessType=DOWNLOAD"
  } else {
    address <- "https://data.cms.gov/api/views/475s-fzi7/rows.csv?accessType=DOWNLOAD"
  }

  df <- LoadPUF(address);

  if (year == 2013) {
    # Note: 2013 data has a different format than 2014 and 2015
  }

  if (standardize_data == TRUE)
  {
  }

  if (enhance_data == TRUE) {

    df$Performance_Year <- year

    if (year != 2013) {
      df$CMS_HCC_RiskScore_PY <- (df$CMS_HCC_RiskScore_DIS_PY * df$N_AB_Year_DIS_PY +
                                    df$CMS_HCC_RiskScore_ESRD_PY * df$N_AB_Year_ESRD_PY +
                                    df$CMS_HCC_RiskScore_AGDU_PY * df$N_AB_Year_AGED_Dual_PY +
                                    df$CMS_HCC_RiskScore_AGND_PY * df$N_AB_Year_AGED_NonDual_PY) / df$N_AB
    }
  }

  return (df)
}

# Load the PUF files from the web.
LoadPUF <- function(address) {
  x <- RCurl::getURL(address)
  df <- read.csv(text = x)
  return (df)
}
