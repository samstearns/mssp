years <- c(2013, 2014, 2015, 2016, 2017)
url_info <- c("faep-t7cf", "888h-akbg", "7rrf-3gxr", "3jk5-q6dr", "gk7c-vejx")
url_lookup <- data.frame(years, url_info)

standard_fields_2013 <- toupper(c("ACO_Num", "ACO_NAME", "N_AB", "QualScore", "Performance_Year"))

#' Downloads PUF files from CMS website (https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html)
#'
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_puf_file <- function(year="1000") {

  if (!requireNamespace("RCurl", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  if (year == 2013) {
    address <- "faep-t7cf"
  } else if (year == 2014) {
    address <- "888h-akbg"
  } else if (year == 2015) {
    address <- "7rrf-3gxr"
  } else if (year == 2016) {
    address <- "3jk5-q6dr"
  } else if (year == 2017) {
    address <- "gk7c-vejx"
  } else {
    print("Invalid performance year. Please select a value between 2013 and 2017.")
    return()
  }

  url = paste("https://data.cms.gov/api/views/", address, "/rows.csv?accessType=DOWNLOAD", sep = "")

  df <- LoadPUF(url);

  return (df)
}

load_multi_year_db <- function() {

  # create db
  i <- 1

  # for each year in URL_Lookup
  for (year in url_lookup[ ,1]) {

    p <- load_puf_file(year)

    print(paste("Dowloading PUF file for", year, "with", nrow(p), "rows and", length(p), "columns\n"))

    # Add the year
    p$Performance_Year <- year

    # Identify the columns to filter for, adjusting for capitalization
    filtervars <- toupper(names(p)) %in% toupper(standard_fields_2013)

    newdata <- p[filtervars]
    # Standardize the column names to merge data frames
    colnames(newdata) <- toupper(standard_fields_2013)

    # ADD TO DF
    if ( i == 1 ) {
      db <- newdata
    } else {
      db <- rbind(db, newdata)
    }

    i <- i+1
  }

  # return DB
  return(db)
}

#' Downloads PUF files from CMS website (https://www.cms.gov/Research-Statistics-Data-and-Systems/Downloadable-Public-Use-Files/SSPACO/index.html)
#' and applies enhancements
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_enhanced_puf_file <- function(year="1000") {
  df <- load_puf_file(year)
  return (enhance_puf_file(df, year))
}


#' Applies enhancements to a datafrane containin PUF file data
#' @param df Dataframe containing downloaded PUF file.
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(df, 2016)
#' @export
enhance_puf_file <- function(df, year) {

  df$Performance_Year <- year

  if (year != 2013) {
    df$CMS_HCC_RiskScore_PY <- (df$CMS_HCC_RiskScore_DIS_PY * df$N_AB_Year_DIS_PY +
                                  df$CMS_HCC_RiskScore_ESRD_PY * df$N_AB_Year_ESRD_PY +
                                  df$CMS_HCC_RiskScore_AGDU_PY * df$N_AB_Year_AGED_Dual_PY +
                                  df$CMS_HCC_RiskScore_AGND_PY * df$N_AB_Year_AGED_NonDual_PY) / df$N_AB
  }

  return(df)
}

# Load the PUF files from the web.
LoadPUF <- function(address) {
  x <- RCurl::getURL(address)
  df <- read.csv(text = x)
  return (df)
}
