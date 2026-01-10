#' Looks up the ACO Num with an ACO name
#' @param df SSP data
#' @param str Search string with the name of the ACO
#' @return Potential matches
#' @examples
#' a <- load_puf_file(2022)
#' lookup_aco_num(a, "Mercy")
#' @export
lookup_aco_num <- function(df, str) {
  matches <- grep(str, df$aco_name)

  df[matches, 1:2]
}

#' Returns list of ACOs in a state
#' @param df SSP data
#' @param str Name of the state
#' @return ACOs where assigned beneficiaries reside in the state
#' @examples
#' a <- load_puf_file(2022)
#' lookup_aco_by_state(a, "Maine")
#' @export
lookup_aco_by_state <- function(df, str) {
  matches <- grep(str, df$aco_state)

  df[matches, 1:2]
}
