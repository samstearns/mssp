# List of years and URLs, in descending order
years <- c(2024, 2023, 2022, 2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013);

per_capita_exps <- tolower(c("CapAnn_INP_All", "CapAnn_INP_S_trm", "CapAnn_INP_L_trm", "CapAnn_INP_Rehab", "CapAnn_INP_Psych",
                             "CapAnn_HSP", "CapAnn_SNF", "CapAnn_OPD", "CapAnn_PB", "CapAnn_AmbPay", "CapAnn_HHA", "CapAnn_DME",
                     "Per_Capita_Exp_ALL_ESRD_BY1",
                     "Per_Capita_Exp_ALL_DIS_BY1",
                     "Per_Capita_Exp_ALL_AGDU_BY1",
                     "Per_Capita_Exp_ALL_AGND_BY1",
                     "Per_Capita_Exp_ALL_ESRD_BY2",
                     "Per_Capita_Exp_ALL_DIS_BY2",
                     "Per_Capita_Exp_ALL_AGDU_BY2",
                     "Per_Capita_Exp_ALL_AGND_BY2",
                     "Per_Capita_Exp_ALL_ESRD_BY3",
                     "Per_Capita_Exp_ALL_DIS_BY3",
                     "Per_Capita_Exp_ALL_AGDU_BY3",
                     "Per_Capita_Exp_ALL_AGND_BY3",
                     "Per_Capita_Exp_ALL_ESRD_PY",
                     "Per_Capita_Exp_ALL_DIS_PY",
                     "Per_Capita_Exp_ALL_AGDU_PY",
                     "Per_Capita_Exp_ALL_AGND_PY",
                     "Per_Capita_Exp_TOTAL_PY"
                     ));

num_benes <- tolower(c(
  "N_AB",
  "N_AB_Year_ESRD_BY3",
  "N_AB_Year_DIS_BY3",
  "N_AB_Year_AGED_Dual_BY3",
  "N_AB_Year_AGED_NonDual_BY3",
  "N_AB_Year_PY",
  "N_AB_Year_ESRD_PY",
  "N_AB_Year_DIS_PY",
  "N_AB_Year_AGED_Dual_PY",
  "N_AB_Year_AGED_NonDual_PY",
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
  "N_Ben_Race_Other"
));

util_rates <- tolower(c(
  "ADM",
  "ADM_S_Trm",
  "ADM_L_Trm",
  "ADM_Rehab",
  "ADM_Psych",
  "P_EDV_Vis",
  "P_EDV_Vis_HOSP",
  "P_CT_VIS",
  "P_MRI_VIS",
  "P_EM_Total",
  "P_EM_PCP_Vis",
  "P_EM_SP_Vis",
  "P_Nurse_Vis",
  "P_FQHC_RHC_Vis",
  "P_SNF_ADM",
  "SNF_LOS",
  "SNF_PayperStay"
));

bmrk_values <- tolower(c("BnchmkMinExp", "GenSaveLoss", "DisAdj", "EarnSaveLoss", "UpdatedBnchmk", "HistBnchmk", "ABtotBnchmk", "ABtotExp", "Adv_Pay_Amt"));

percentage_savings <- tolower(c("Sav_Rate", "MinSavPerc", "MaxShareRate", "FinalShareRate", "QualScore"));

#' Downloads PUF files from CMS website
#'
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
#' @importFrom utils write.csv
#' @importFrom utils read.csv
load_puf_file <- function(year="1000") {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.", call. = FALSE)
  }

  endpoints <- get_cms_endpoints();

  address <- endpoints[endpoints$year == year, 2]

  if (address == "") {
    print("Invalid performance year. Please select a value between 2013 and 2024")
    return()
  }

  df <- jsonlite::fromJSON(address);

  # Convert the column types from chr by writing to a temporary CSV
  filename = paste0(tempdir(), "/mssp", ".csv");
  write.csv(df, file = filename );
  dfa <- read.csv(filename)

  # Remove the temporary CSV file
  unlink(filename)

  # Standardize the column names to lower case. 2019 is all lower case, other years camel case
  names(dfa) <- tolower(names(dfa))

  # starting in 2022, risk scores scores includes label * for ACOS small sample size
  # https://www.hhs.gov/guidance/document/cms-cell-suppression-policy
  # Remove this with NAs to avoid warning NAs introduced by coercion

  for (value in per_capita_exps) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
      dfa[, value] <- gsub("*", "", dfa[,value])
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  for (value in num_benes) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  for (value in util_rates) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
      dfa[, value] <- gsub("*", "", dfa[,value])
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  for (value in bmrk_values) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  dfa[, "cms_hcc_riskscore_esrd_py"] <- gsub("*", "", dfa[,"cms_hcc_riskscore_esrd_py"])
  dfa[, "cms_hcc_riskscore_esrd_py"] <- as.numeric(dfa[,"cms_hcc_riskscore_esrd_py"])

  dfa[, "cms_hcc_riskscore_agdu_py"] <- gsub("*", "", dfa[,"cms_hcc_riskscore_agdu_py"])
  dfa[, "cms_hcc_riskscore_agdu_py"] <- as.numeric(dfa[,"cms_hcc_riskscore_agdu_py"])


  for (value in percentage_savings) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub("%", "", dfa[,value])

      # 2014 qual scores includes label P4R for ACOS who selected this option.
      # Remove this with NAs to avoid warning NAs introduced by coercion
      if(year == 2014) {
        dfa[, value] <- gsub("P4R", "", dfa[,value])
      }

      dfa[, value] <- as.numeric(dfa[,value])

      if (year == 2021 && (value == "sav_rate" || value == "minsavperc" || value == "qualscore" || value == "maxsharerate" || value == "finalsharerate" )) {
        dfa[, value] <- dfa[,value] / 100.0
      }
      if (year == 2020 && ( value == "qualscore" )) {
        dfa[, value] <- dfa[,value] / 100.0
      }

      if (year == 2019 && ( value == "qualscore" )) {
        dfa[, value] <- dfa[,value] / 100.0
      }

      if (year == 2015 && (value == "sav_rate" || value == "minsavperc" || value == "qualscore" || value == "finalsharerate" )) {
        dfa[, value] <- dfa[,value] / 100.0
      }
      if (year == 2014 && (value == "sav_rate" || value == "minsavperc" )) {
        dfa[, value] <- dfa[,value] / 100.0
      }

    }
    }

  # Remove non-ASCII characters in ACO Name
  dfa[, "aco_name"] <- iconv(dfa[, "aco_name"], from = "UTF-8", to = "ASCII", sub = "")

  return (dfa)
}

#' Downloads PUF files from CMS website from multiple years and integrates into a single dataset.
#' @param verbose Provides detail on download process if set to TRUE
#' @return Data frame with mssp data from all years.
#' @examples
#' df <- load_multi_year_db()
#' @export
load_multi_year_db <- function(verbose = FALSE) {

  most_recent_year <- years[1]
  print(paste("Creating multi-year DB for ", length(years), " years. Most recent year =", most_recent_year))

  # for each year in URL_Lookup
  for (year in years) {

    print(paste("Downloading PUF file for", year))

    if ( most_recent_year == year ) {
      # Download the most recent year.
      # Use the structure of this year for the multi-year database
      most_recent_year_data <- load_enhanced_puf_file(year)

      # Add a column to record the year
      ncols <- length(most_recent_year_data)

      # Preserve original column names for the most recent year
      original_col_names <- colnames(most_recent_year_data)

      colnames(most_recent_year_data) <- tolower(colnames(most_recent_year_data))

      multi_year_data <- most_recent_year_data

    } else {
      # prior years
      b <- load_enhanced_puf_file(year)

      nrows <- nrow(b)
      # Standardize the column names to merge data frames
      colnames(b) <- tolower(colnames(b))

      # Create a new DF with N rows from B and N cols from A
      df <- data.frame(matrix(NA, nrow = nrows, ncol = ncols))
      colnames(df) <- colnames(most_recent_year_data)

      # Loop through each column in A
      if (verbose) {
        print(paste("Merging columns for", year))
      }
      for (i in 1:ncols) {
        col <- colnames(most_recent_year_data)[i]
        # Look up the position of the column by name
        colIndex <- which(names(b)==col)

        if (identical(colIndex, integer(0))) {
          # if not in B, copy blank cell
          if (verbose) {
            print(paste(col, " is not found in PY ", year))
          }
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

  return(multi_year_data)
}

#' Downloads PUF files from CMS website and applies enhancements
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_enhanced_puf_file(2016)
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
#' df <- load_puf_file(2016)
#' enhance_puf_file(df, 2016)
#' @export
enhance_puf_file <- function(df, year) {

  df$performance_year <- year

  if (year < 2017) {
    colnames(df)[2] <- "ACO_NUM"

    # add a column for the ACO_ID
    aic <- aco_id_crosswalk[c("ACO_NUM", "ACO_ID")]
    df <- merge(df, aic, by="ACO_NUM", sort=FALSE)
  }
  else if (year > 2017) {
    #colnames(df)[2] <- "ACO_NUM"

    # add a column for the ACO_ID
    #aic <- aco_id_crosswalk[c("ACO_NUM", "ACO_ID")]
    #df <- merge(df, aic, by="ACO_NUM", sort=FALSE)
  }

  # Ensure column names have consistent capitalization, due to 2018 being lowercase
  names(df) <- tolower(names(df))

  # Standardize column names
  if (year < 2015) {
    colnames(df)[colnames(df) == 'n_ab_year_dis'] <- 'n_ab_year_dis_py'
    colnames(df)[colnames(df) == 'n_ab_year_esrd'] <- 'n_ab_year_esrd_py'
    colnames(df)[colnames(df) == 'n_ab_year_aged_dual'] <- 'n_ab_year_aged_dual_py'
    colnames(df)[colnames(df) == 'n_ab_year_aged_nondual'] <- 'n_ab_year_aged_nondual_py'
  }

  # Create risk score
  df$cms_hcc_riskscore_py <- (df$cms_hcc_riskscore_dis_py * df$n_ab_year_dis_py +
                                      df$cms_hcc_riskscore_esrd_py * df$n_ab_year_esrd_py +
                                      df$cms_hcc_riskscore_agdu_py * df$n_ab_year_aged_dual_py +
                                     df$cms_hcc_riskscore_agnd_py * df$n_ab_year_aged_nondual_py) / df$n_ab

  return(df)
}
