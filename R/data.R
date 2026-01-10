# List of years and URLs, in descending order
years <- c(2021, 2020, 2019, 2018, 2017, 2016, 2015, 2014, 2013);

per_capita_exps <- tolower(c("CapAnn_INP_All", "CapAnn_INP_S_trm", "CapAnn_INP_Rehab", "CapAnn_INP_Psych",
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

#  "ACO_Num", "ACO_NAME", "N_AB", "QualScore", "Per_Capita_Exp_TOTAL_PY", "HistBnchmk", "UpdatedBnchmk", "Performance_Year"))


#' Downloads PUF files from CMS website
#'
#' @param year MSSP performance year.
#' @return Data frame with mssp data.
#' @examples
#' load_puf_file(2016)
#' @export
load_puf_file <- function(year="1000") {

  if (!requireNamespace("jsonlite", quietly = TRUE)) {
    stop("Package \"pkg\" needed for this function to work. Please install it.", call. = FALSE)
  }

  if (year == 2013) {
    address <- "https://data.cms.gov/data-api/v1/dataset/bc90f498-76f4-4e75-8225-8aae30336059/data"
  } else if (year == 2014) {
    address <- "https://data.cms.gov/data-api/v1/dataset/0ef9b1e2-e23b-4a01-921c-1ac7290c814b/data"
  } else if (year == 2015) {
    address <- "https://data.cms.gov/data-api/v1/dataset/156c00e2-ab42-4923-b54f-09c031f5f28d/data"
  } else if (year == 2016) {
    address <- "https://data.cms.gov/data-api/v1/dataset/a290fdd3-976a-4fc9-9139-a98193b3af82/data"
  } else if (year == 2017) {
    address <- "https://data.cms.gov/data-api/v1/dataset/3b306450-1836-417b-b779-7d70fd2fc734/data"
  } else if (year == 2018) {
    address <- "https://data.cms.gov/data-api/v1/dataset/80c86127-8839-4f35-b87b-aa37664afd19/data"
  } else if (year == 2019) {
    address <- "https://data.cms.gov/data-api/v1/dataset/9c3a4c69-7d00-4307-9b6f-a080dc90417e/data"
  } else if (year == 2020) {
    address <- "https://data.cms.gov/data-api/v1/dataset/8f073013-9db0-4b12-9a34-5802bdabbdfe/data"
  } else if (year == 2021) {
    address <- "https://data.cms.gov/data-api/v1/dataset/a5d74ce2-ba38-47be-8523-146e4ad41832/data"
  } else if (year == 2022) {
    address <- "https://data.cms.gov/data-api/v1/dataset/a5d74ce2-ba38-47be-8523-146e4ad41832/data"
  } else if (year == 2023) {
    address <- "https://data.cms.gov/data-api/v1/dataset/7082a8f1-6d51-4723-853d-086bf254f5fb/data"
  } else if (year == 2024) {
    address <- "https://data.cms.gov/data-api/v1/dataset/73b2ce14-351d-40ac-90ba-ec9e1f5ba80c/data"
  } else {
    print("Invalid performance year. Please select a value between 2013 and 2024")
    return()
  }

  df <- jsonlite::fromJSON(address);

  # Convert the column types from chr by writing to a temporary CSV
  filename = paste0(tempdir(), "/mssp", ".csv");
  write.csv(df, file = filename );
  dfa <- read.csv(filename)

  # Standardize the column names to lower case. 2019 is all lower case, other years camel case
  names(dfa) <- tolower(names(dfa))

  # Remove the temporary CSV file
  unlink(filename)

  for (value in per_capita_exps) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
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
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  for (value in bmrk_values) {
    if(value %in% colnames(dfa)) {
      dfa[, value] <- gsub(",", "", dfa[,value])
      dfa[, value] <- as.numeric(dfa[,value])
    }
  }

  # starting in 2022, risk scores scores includes label * for ACOS small sample size
  # https://www.hhs.gov/guidance/document/cms-cell-suppression-policy
  # Remove this with NAs to avoid warning NAs introduced by coercion
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

  return (dfa)
}

#' Downloads PUF files from CMS website from multiple years and integrates into a single dataset.
#' @return Data frame with mssp data from all years.
#' @examples
#' load_multi_year_db()
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
      most_recent_year_data <- load_puf_file(year)

      # Add a column to record the year
      most_recent_year_data$performance_year <- year

      ncols <- length(most_recent_year_data)

      # Preserve original column names for the most recent year
      original_col_names <- colnames(most_recent_year_data)

      colnames(most_recent_year_data) <- tolower(colnames(most_recent_year_data))

      multi_year_data <- most_recent_year_data

    } else {
      # prior years
      b <- load_puf_file(year)
      b$performance_year <- year

      # Standardize savings rate calculation
      if (year == 2014 | year == 2015) {
        b$sav_rate <- b$sav_rate / 100.0;
        b$minsavperc <- b$minsavperc / 100.0;
      }

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

  # Add the risk score
  if (multi_year_data$performance_year != 2013) {
    multi_year_data$cms_hcc_riskscore_py <- (multi_year_data$cms_hcc_riskscore_dis_py * multi_year_data$n_ab_year_dis_py +
                                               multi_year_data$cms_hcc_riskscore_esrd_py * multi_year_data$n_ab_year_esrd_py +
                                               multi_year_data$cms_hcc_riskscore_agdu_py * multi_year_data$n_ab_year_aged_dual_py +
                                               multi_year_data$cms_hcc_riskscore_agnd_py * multi_year_data$n_ab_year_aged_nondual_py) / multi_year_data$n_ab
  } else {
    multi_year_data$cms_hcc_riskscore_py <- NULL;
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

  df$performance_year <- year


  if (year < 2018) {
    colnames(df)[2] <- "ACO_NUM"
    #df$aco_id <- df$aco_num

    # add a column for the ACO_ID
    aic <- aco_id_crosswalk[c("ACO_NUM", "ACO_ID")]

    df <- merge(df, aic, by="ACO_NUM", sort=FALSE)
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
