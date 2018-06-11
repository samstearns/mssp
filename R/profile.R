utilization_variables <- c("ACO_Num", "ACO_Name", "ADM", "ADM_S_Trm", "ADM_L_Trm", "ADM_Rehab",  "ADM_Psych", "readm_Rate_1000", "prov_Rate_1000",
                           "P_SNF_ADM", "P_EDV_Vis", "P_EDV_Vis_HOSP", "P_CT_VIS", "P_MRI_VIS", "P_EM_Total", "P_EM_PCP_Vis", "P_EM_SP_Vis")

expenditures_variables <- c("ACO_Num", "ACO_Name", "CapAnn_INP_All", "CapAnn_INP_S_trm", "CapAnn_INP_L_trm", "CapAnn_INP_Rehab",  "CapAnn_INP_Psych", "readm_Rate_1000", "prov_Rate_1000",
                            "CapAnn_HSP", "CapAnn_SNF", "CapAnn_INP_Other", "CapAnn_OPD", "CapAnn_PB", "CapAnn_AmbPay", "CapAnn_HHA", "CapAnn_DME")

#' Looks up the ACO Num with an ACO name
#' @param df SSP data
#' @param str Search string with the name of the ACO
#' @return Potential matches
#' @examples
#' lookup_aco_num("Mercy")
#' @export
lookup_aco_num <- function(df, str) {
  matches <- grep(str, df$ACO_Name)

  df[matches, 1:2]
}

#' Looks up the ACO Num with an ACO name
#' @param df SSP data
#' @param str Name of the state
#' @return ACOs where assigned beneficiaries reside in the state
#' @examples
#' lookup_aco_num("Massachusetts")
#' @export
lookup_aco_by_state <- function(df, str) {
  matches <- grep(str, df$ACO_State)

  df[matches, 1:2]
}

#' Profiles utilization metrics vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_utilization(df, "A95164", 2016)
#' @export
profile_utilization <- function(df, aco_num, year = NULL) {
  profile_aco(df, aco_num, utilization_variables)
}

#' Profiles expenditures vs. national sample
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_expenditure(df, "A95164", 2016)
#' @export
profile_expenditures <- function(df, aco_num, year = NULL) {
  profile_aco(df, aco_num, expenditures_variables)
}


profile_aco <- function(df, aco_num, profile_variables, year = NULL) {

  # get utilization for the selected ACO
  # TODO: filter for year
  aco_results <- df[which(df$ACO_Num==aco_num), profile_variables]

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
    median_results[1, i] <- median(df[,var])
    median_results[2, i] <- aco_results[1, i] * 1.0 / median_results[1, i] - 1.0

    results_dist <- ecdf(df[, var])
    median_results[3, i] <- results_dist(df[which(df$ACO_Num==aco_num), var])
    i <- i + 1
  }

  # join the ACO and median values
  aco_results <- rbind(aco_results, median_results)
}
