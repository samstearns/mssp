savings_variables <- tolower(c("ACO_ID", "ACO_Name", "N_AB", "CMS_HCC_RiskScore_PY",
                     "Per_Capita_Exp_TOTAL_PY", "UpdatedBnchmk", "Sav_Rate", "MinSavPerc", "BnchmkMinExp", "GenSaveLoss", "EarnSaveLoss", "QualScore", "Met_QPS"));


utilization_variables <- tolower(c("ACO_ID", "ACO_Name", "ADM", "ADM_S_Trm", "ADM_L_Trm", "ADM_Rehab",  "ADM_Psych", "P_EDV_Vis",
                           "P_EDV_Vis_HOSP","P_CT_VIS", "P_MRI_VIS", "P_EM_Total", "P_EM_PCP_Vis", "P_EM_SP_Vis",
                           "P_Nurse_Vis",
                           "P_FQHC_RHC_Vis",
                           "P_SNF_ADM",
                           "SNF_LOS",
                           "SNF_PayperStay"));
#                           #"readm_Rate_1000",
#                           "prov_Rate_1000",
#                           "P_SNF_ADM",   );

expenditures_variables <- tolower(c("ACO_ID", "ACO_Name", "CapAnn_INP_All", "CapAnn_INP_S_trm", "CapAnn_INP_L_trm", "CapAnn_INP_Rehab",  "CapAnn_INP_Psych",
                            "CapAnn_HSP", "CapAnn_SNF", "CapAnn_OPD", "CapAnn_PB", "CapAnn_AmbPay", "CapAnn_HHA", "CapAnn_DME"));


# Note: ACO40 removed due to non-numeric scores
#quality_variables <- c("QualScore", "QualPerfShare", "FinalShareRate", "ACO1", "ACO2", "ACO3", "ACO4", "ACO5", "ACO6", "ACO7",
#                       "ACO8", "ACO9", "ACO10", "ACO11", "ACO13", "ACO14", "ACO15", "ACO16", "ACO17", "ACO18", "ACO19", "ACO20","ACO21", "ACO27", "ACO28", "ACO30",
#                       "ACO31", "ACO33", "ACO34", "ACO35", "ACO36", "ACO37", "ACO38", "ACO39", "ACO41", "ACO42", "DM_Comp");


#' Helper function to access utilization metrics
#' @param df SSP data
#' @param year MSSP performance year.
#' @return Data frame with mssp utilization metrics.
#' @examples
#' utilization_metrics(df, 2016)
#' @export
utilization_metrics <- function(df, year = NULL) {
  aco_results <- df[, utilization_variables]
}

#' Helper function to access cost metrics
#' @param df SSP data
#' @param year MSSP performance year.
#' @return Data frame with mssp cost metrics.
#' @examples
#' cost_metrics(df, 2016)
#' @export
cost_metrics <- function(df, year = NULL) {
  aco_results <- df[, expenditures_variables]
}

#' Helper function to access savings metrics
#' @param df SSP data
#' @param year MSSP performance year.
#' @return Data frame with mssp savings metrics.
#' @examples
#' savings_metrics(df, 2016)
#' @export
savings_metrics <- function(df, year = NULL) {
  aco_results <- df[, savings_variables]
}

#' Helper function to access quality metrics
#' @param df SSP data
#' @param year MSSP performance year.
#' @return Data frame with mssp quality metrics.
#' @examples
#' quality_metrics(df, 2016)
#' @export
quality_metrics <- function(df, year = NULL) {
  aco_results <- df[, quality_variables]
}

#' Profiles utilization metrics vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_utilization(df, "A95164", 2016)
#' @export
profile_utilization <- function(df, aco_id, year = NULL) {
  profile_aco(df, aco_id, utilization_variables)
}

#' Profiles expenditures vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_expenditure(df, "A95164", 2016)
#' @export
profile_expenditures <- function(df, aco_id, year = NULL) {
  profile_aco(df, aco_id, expenditures_variables)
}

#' Profiles quality measures vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_quality(df, "A95164", 2016)
#' @export
profile_quality <- function(df, aco_id, year = NULL) {
  profile_aco(df, aco_id, quality_variables)
}

#' Profiles contract measures vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_savings(df, "A95164", 2016)
#' @export
profile_savings <- function(df, aco_id, year = NULL) {
  profile_aco(df, aco_id, savings_variables)
}

#' Trends contract savings over time
#' @param aco_num ACO Number.
#' @param df Optional dataframe with MSSP data. If not supplied, uses mssp_all_years package in dataset
#' @return Data frame with mssp data.
#' @examples
#' trend_savings("A95164")
#' @export
trend_savings <- function(aco_id, df = NULL) {
  vars <- c("performance_year", savings_variables)
  trend_aco(df, aco_id, vars)
}

#' Trends expenditures over time
#' @param aco_num ACO Number.
#' @param df Optional dataframe with MSSP data. If not supplied, uses mssp_all_years package in dataset
#' @return Data frame with expenditure metrics over time.
#' @examples
#' trend_expenditures("A95164")
#' @export
trend_expenditures <- function(aco_id, df = NULL) {
  vars <- c("performance_year", expenditures_variables)
  trend_aco(df, aco_id, vars)
}

#' Trends utilization over time
#' @param aco_num ACO Number.
#' @param df Optional dataframe with MSSP data. If not supplied, uses mssp_all_years package in dataset
#' @return Data frame with utilization metrics over time.
#' @examples
#' trend_savings("A95164")
#' @export
trend_utilization <- function(aco_id, df = NULL) {
  vars <- c("performance_year", utilization_variables)
  trend_aco(df, aco_id, vars)
}

trend_aco <- function(df, aco_id, profile_variables) {
  if ( is.null(df) ) {
    df <- mssp_all_years
  }
  aco_results <- df[which(df$aco_id==aco_id), profile_variables]
}

profile_aco <- function(df, aco_id, profile_variables, year = NULL) {

  # get results for the selected ACO
  # TODO: filter for year
  aco_results <- df[which(df$aco_id==aco_id), profile_variables]

  # Calculate median values
  median_results <- data.frame(matrix(ncol = length(profile_variables), nrow = 3))
  colnames(median_results) <- profile_variables
  median_results[1,1] <- "N/A"
  median_results[1,2] <- "Median"
  median_results[2,1] <- "N/A"
  median_results[2,2] <- "% Diff vs. Median"
  median_results[3,1] <- "N/A"
  median_results[3,2] <- "Percentile"

  i <- 3
  for (var in profile_variables[3:length(profile_variables)]) {
    print(var)
    median_results[1, i] <- median(df[,var], na.rm = TRUE)
    median_results[2, i] <- aco_results[1, i] * 1.0 / median_results[1, i] - 1.0

    results_dist <- ecdf(df[, var])
    median_results[3, i] <- results_dist(df[which(df$aco_id==aco_id), var])
    i <- i + 1
  }

  # join the ACO and median values
  aco_results <- rbind(aco_results, median_results);
}
