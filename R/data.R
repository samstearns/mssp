# List of years and URLs, in descending order
years <- c(2017, 2016, 2015, 2014, 2013)
url_info <- c("gk7c-vejx", "3jk5-q6dr", "7rrf-3gxr", "888h-akbg", "gk7c-vejx")

url_lookup <- data.frame(years, url_info)

standard_fields_2013 <- toupper(c("ACO_Num", "ACO_NAME", "N_AB", "QualScore", "Per_Capita_Exp_TOTAL_PY", "HistBnchmk", "UpdatedBnchmk", "Performance_Year"))

#' Downloads PUF files from CMS website
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

#' Downloads PUF files from CMS website from multiple years and integrates into a single dataset.
#' @return Data frame with mssp data from all years.
#' @examples
#' load_multi_year_db()
#' @export
load_multi_year_db <- function() {

  most_recent_year <- years[1]
  print(paste("Creating multi-year DB for ", length(years), " years. Most recent year =", most_recent_year))

  # for each year in URL_Lookup
  for (year in url_lookup[ ,1]) {

    print(paste("Dowloading PUF file for", year))

    if ( most_recent_year == year ) {
      # Download the most recent year.
      # Use the structure of this year for the multi-year database
      most_recent_year_data <- load_puf_file(year)

      # Add a column to record the year
      most_recent_year_data$Performance_Year <- year

      ncols <- length(most_recent_year_data)

      # Preserve original column names for the most recent year
      original_col_names <- colnames(most_recent_year_data)

      colnames(most_recent_year_data) <- toupper(colnames(most_recent_year_data))

      multi_year_data <- most_recent_year_data

    } else {
      # prior years
      b <- load_puf_file(year)
      b$Performance_Year <- year
      nrows <- nrow(b)
      # Standardize the column names to merge data frames
      colnames(b) <- toupper(colnames(b))

      # Create a new DF with N rows from B and N cols from A
      df <- data.frame(matrix(NA, nrow = nrows, ncol = ncols))
      colnames(df) <- colnames(most_recent_year_data)

      # Loop through each column in A
      print(paste("Merging columns for", year))

      for (i in 1:ncols) {
        col <- colnames(most_recent_year_data)[i]
        # Look up the position of the column by name
        colIndex <- which(names(b)==col)

        if (identical(colIndex, integer(0))) {
          # if not in B, copy blank cell
          print(paste(col, " is not found in PY ", year))
        } else {
          # if found in B, copy to the dataframe
          df[,i] <- b[,colIndex]
        }

        colnames(df)[i] <- colnames(most_recent_year_data)[i]
      }

      # Paste the two dataframes togeter
      multi_year_data <- rbind(multi_year_data, df)
    } # end prior year
  }

  colnames(multi_year_data) <- original_col_names

  # Add the risk score
  #junk$nm[junk$nm == "B"] <- "b"
  if (multi_year_data$Performance_Year != 2013) {
    multi_year_data$CMS_HCC_RiskScore_PY <- (multi_year_data$CMS_HCC_RiskScore_DIS_PY * multi_year_data$N_AB_Year_DIS_PY +
                                               multi_year_data$CMS_HCC_RiskScore_ESRD_PY * multi_year_data$N_AB_Year_ESRD_PY +
                                               multi_year_data$CMS_HCC_RiskScore_AGDU_PY * multi_year_data$N_AB_Year_AGED_Dual_PY +
                                               multi_year_data$CMS_HCC_RiskScore_AGND_PY * multi_year_data$N_AB_Year_AGED_NonDual_PY) / multi_year_data$N_AB
  } else {
    multi_year_data$CMS_HCC_RiskScore_PY <- NULL;
  }

  # return DB
  return(multi_year_data)
}

#' Downloads PUF files from CMS website and applies enhancements
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_enhanced_puf_file <- function(year="1000") {
  df <- load_puf_file(year)
  return (enhance_puf_file(df, year))
}


#' Applies enhancements to a datafrane containing PUF file data
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
