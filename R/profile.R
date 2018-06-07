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

#' Displays utilization metrics
#' @param df SSP data
#' @param aco_num ACO Number.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' profile_utilization(df, 2016)
#' @export
profile_utilization <- function(df, aco_num, year = NULL) {
  utilization_variables <- c("ACO_Num", "ACO_Name", "ADM", "ADM_S_Trm", "ADM_L_Trm", "ADM_Rehab",  "ADM_Psych", "readm_Rate_1000", "prov_Rate_1000",
                             "P_SNF_ADM", "P_EDV_Vis", "P_EDV_Vis_HOSP", "P_CT_VIS", "P_MRI_VIS", "P_EM_Total", "P_EM_PCP_Vis", "P_EM_SP_Vis")

  # get utilization for the selected ACO
  # TODO: filter for year
  aco_results <- df[which(df$ACO_Num==aco_num), utilization_variables]
  print(class(aco_results))

    # Calculate the median values
  median_results <- data.frame(matrix(ncol = 17, nrow = 2))
  colnames(median_results) <- utilization_variables
  median_results[1,1] <- "N/A"
  median_results[1,2] <- "Median"
  median_results[1,1] <- "N/A"
  median_results[1,2] <- "% Diff vs. Median"
  i <- 3
  for (var in uv[3:17]) {
    median_results[1, i] <- median(df[,var])
    median_results[2, i] <- aco_results[1, i] * 1.0 / median_results[1, i] - 1.0
    i <- i + 1
  }

  # join the ACO and median values
  aco_results <- rbind(aco_results, median_results)
}

#CapAnn_INP_All
#CapAnn_INP_S_trm
#CapAnn_INP_L_trm
#CapAnn_INP_Rehab
#CapAnn_INP_Psych
#CapAnn_HSP
#CapAnn_SNF
#CapAnn_INP_Other
#CapAnn_OPD
#CapAnn_PB
#CapAnn_AmbPay
#CapAnn_HHA
#CapAnn_DME
