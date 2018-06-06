#' Looks up the ACO Num with an ACO name
#' @param str Search string with the name of the ACO
#' @return Potential matches
#' @examples
#' lookup_aco_num("Mercy")
#' @export
lookup_aco_num <- function(str) {
  matches <- grep(str, c$ACO_Name)

  c[matches, 1:2]
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
  utilization_variables <- c("ACO_Num", "ACO_Name", "ADM", "P_SNF_ADM")

  # get utilization for the selected ACO
  # TODO: filter for year
  df[which(df$ACO_Num==aco_num), utilization_variables]

  median(df[utilization_variables])
}
