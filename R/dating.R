#' To format collection dates for samples
#'
#' This function formats the dates in the form of YYYY-MM-DD to a decimal year to
#' be used in tip dating analysis.
#'
#' @param metadata Object of class dataframe. It will contain information
#'    about collection dates for the sequences
#' @param country Object of class character. It should be in the form of
#'    "SN" or "SN.CGR"
#'
#' @return A list in which the first element contain information in which the
#'     complete collection date is known; the second element of the list contain
#'     elements that have missing dating information
#' @details The returned elements in the list will contain 2 dataframes. The
#'     first dataframe will have NA for collection dates that is only known
#'     from some parts (I mean, it is only know the YYYY.MM, or only YYYY). The
#'     second dataframe will have the upper and lower bound for the samples
#'     that we only partally know the collection dates.
#' @export
#'
#' @examples # write example here
data_format <- function(metadata, country){
  if(country != "CGR"){
    metadata$tip <- paste(metadata$Accession_number, metadata$Subtype, country, metadata$Year, sep='.')
  }else{metadata$tip <- paste(metadata$Accession_number, country, sep='_')}

  metadata$date <- paste(metadata$Year, metadata$Month, metadata$Day, sep='-')
  metadata$date_ymd <- lubridate::ymd(metadata$date)
  metadata$decimal <- lubridate::decimal_date(metadata$date_ymd)
  metadata$decimal <- as.factor(metadata$decimal)

  missing_sample <- metadata[is.na(metadata$decimal),]
  missing_sample$lower_date <- paste(missing_sample$Lower_year, missing_sample$Lower_month, missing_sample$Lower_day, sep='-')
  missing_sample$lower_ymd <- lubridate::ymd(missing_sample$lower_date)
  missing_sample$lower <- lubridate::decimal_date(missing_sample$lower_ymd)

  missing_sample$upper_date <- paste(missing_sample$Upper_year, missing_sample$Upper_month, missing_sample$Upper_day, sep='-')
  missing_sample$upper_ymd <- lubridate::ymd(missing_sample$upper_date)
  missing_sample$upper <- lubridate::decimal_date(missing_sample$upper_ymd)

  return(list(metadata, missing_sample))
}


#' Replace values in sequence tip name
#'
#' It replaces values in sample sequence name to reflect the input as expected
#' by BEAST.
#'
#' @param metadata Object of class dataframe.
#'
#' @return Return the sample names by replacing the last bit separeted by a
#'    dash (_) which represents the year of sample collection to the decimal
#'    representing the collection date.
#' @export
#'
#' @examples
#' To do
replace_values <- function(metadata){
  if(grepl("\\.[0-9]+$", metadata["tip"])){
    metadata["tip"] <- gsub('\\.[0-9]+$', paste("_", metadata["decimal"], sep=""), metadata["tip"])
  }else{
    metadata["tip"] <- paste(metadata["tip"], metadata["decimal"], sep="_")
  }
}

#' Convert tips of a phylogentic tree suitable to use with BEAST and phydyn
#'
#' Function that makes some manipulations on dataframes to return a tip name
#' that is suitable to use with phydyn in BEAST. This function was written
#' taking into consideration this specific project and might not work as it is
#' for your own data.
#'
#' @param m.df1 Dataframe object containig information for the Senegal HIV
#'     samples that metadata associated to it is missing.
#' @param m.cgr.df2 Dataframe object containing information for the CGR sequences
#'     that metadata associated to it is missing.
#' @param df3 Dataframe object for all the Senegal sequences
#' @param df4 Dataframe object for all CGR sequences
#' @param all_data Dataframe containing all trait associated to each tip in the
#'     phylogenetic tree
#'
#' @return a dataframe with additional columns representing the tips as
#'     expected by phydyn in BEAST
#' @export
#'
#' @examples TO DO
beast_phydyn <- function(m.df1, m.cgr.df2, df3, df4, all_data){
  # Dataframe for the missing sample date
  missing_sample.df <- rbind(m.df1[c("tip","lower","upper")],
                             m.cgr.df2[c("tip","lower","upper")])

  # adding a middle point to the samples that have missing data in a column named "decimal"
  missing_sample.df["decimal"] <- (missing_sample.df$lower
                                   + missing_sample.df$upper)/2

  # Dataframe containg info for Senegal and CGR sequences. Some values in the
  # decimal colum will be NA
  hiv.df <- rbind(df3[c("Accession_number","tip","decimal")],
                  df4[c("Accession_number","tip","decimal")])
  hiv.df$decimal <- as.character(hiv.df$decimal)
  hiv.df$decimal <- as.numeric(hiv.df$decimal)

  dec.na <- is.na(hiv.df$decimal)

  # Replaces the values in which decimal = NA, to a middle point between lower and
  # upper bounds
  hiv.df$decimal[dec.na] <- missing_sample.df$decimal[match(missing_sample.df$tip,
                                                            hiv.df$tip[dec.na])]

  # create the tips for BEAST analysis, in which the dating is the collection date
  # as decimal values, i.e. 1998.457
  hiv.df["beast_tip"] <- apply(hiv.df, 1, replace_values)

  hiv.df["phydyn_tip"] <- paste(hiv.df$beast_tip,
                                all_data$States[match(hiv.df$tip, all_data$tip.name)],
                                sep= "_")

  return(hiv.df)
}

#' Organize metadata by HIV subtype.
#'
#' This function will organize metadata for dating analysis purpose.
#' It will subset data for subtypes B, C and 02_AG, and have the collection
#' date formated as decimal for tip dating analysis
#'
#' @param all_data_SN Dataframe object of metadata for the Senegal only sequences
#' @param all_data_CGR Dataframe object of metadata for the CGR sequences
#' @param code If code = 1, it will return as a list dataframes for each subtype
#'    but with dating formatted to decimal. If code = 2, it will return a list
#'    of dataframes in which collection date is missing, and a dataframe with all
#'    SN only available sequences
#'
#' @return a list of dataframes
#' @export
#'
#' @examples TO DO
organize_metadata_bySubtype <- function(all_data_SN, all_data_CGR, code){

  if(is.numeric(code) == FALSE){
    message("Code should be numeric and equal 1 or 2. Read manual for more details")
  }

  # Metadata for Senegal sequences
  B.SN <- subset(all_data_SN, Subtype == "B")
  C.SN <- subset(all_data_SN, Subtype == "C")
  AG.SN <- subset(all_data_SN, Subtype == "02_AG")

  # Metadata for CGR sequences
  B.CGR <- subset(all_data_CGR, Subtype == "B")
  C.CGR <- subset(all_data_CGR, Subtype == "C")
  AG.CGR <- subset(all_data_CGR, Subtype == "02_AG")


  ##### subtype B ########
  B.SN.data <- data_format(B.SN, "SN")
  B.SN.df <- B.SN.data[[1]]
  B.missing_sample <- B.SN.data[[2]]

  B.CGR.data <- data_format(B.CGR, "CGR")
  B.CGR.df <- B.CGR.data[[1]]
  B.CGR.df["Sex"] <- NA
  B.CGR.missing_sample <- B.CGR.data[[2]]
  B.CGR.missing_sample["Sex"] <- NA


  ##### subtype C ########
  C.SN.data <- data_format(C.SN, "SN")
  C.SN.df <- C.SN.data[[1]]
  C.missing_sample <- C.SN.data[[2]]

  C.CGR.data <- data_format(C.CGR, "CGR")
  C.CGR.df <- C.CGR.data[[1]]
  C.CGR.df["Sex"] <- NA
  C.CGR.missing_sample <- C.CGR.data[[2]]
  C.CGR.missing_sample["Sex"] <- NA

  ##### subtype 02_AG ########
  AG.SN.data <- data_format(AG.SN, "SN")
  AG.SN.df <- AG.SN.data[[1]]
  AG.missing_sample <- AG.SN.data[[2]]

  AG.CGR.data <- data_format(AG.CGR, "CGR")
  AG.CGR.df <- AG.CGR.data[[1]]
  AG.CGR.df["Sex"] <- NA
  AG.CGR.missing_sample <- AG.CGR.data[[2]]
  AG.CGR.missing_sample["Sex"] <- NA

  if(code == 1){
    return(list(B_all_data = B.SN.data,
                C_all_data = C.SN.data,
                AG_all_data = AG.SN.data,
                B_all_CGR = B.CGR.data,
                C_all_CGR = C.CGR.data,
                AG_all_CGR = AG.CGR.data))
  }else if(code == 2){
    data_SN <- rbind(B.SN.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")],
                     C.SN.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")],
                     AG.SN.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")],
                     B.CGR.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")],
                     C.CGR.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")],
                     AG.CGR.df[c("tip", "Sex", "Risk_group", "date_ymd", "decimal")])

    data_missing_sample <- rbind(B.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")],
                                 C.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")],
                                 AG.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")],
                                 B.CGR.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")],
                                 C.CGR.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")],
                                 AG.CGR.missing_sample[c("tip", "Sex", "Risk_group", "date_ymd", "lower", "upper")])
    return(list(SN_df = data_SN, missing_sample = data_missing_sample))

    }
}
