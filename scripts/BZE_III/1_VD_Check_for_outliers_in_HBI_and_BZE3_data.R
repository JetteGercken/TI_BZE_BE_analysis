###########################################################################################
# Check for outliers in HBI and BZE 3 stock and change data
###########################################################################################

# V. Dietrich: viktoria.dietrich@thuenen.de
# First created: April 2025
# Last modified: 6.6.2025

##########################################################################################

setwd("C:\\Users\\dietrich_v\\Documents\\Thuenen\\BZE\\2_Test_Bestandes_R_skripte\\_Markierte_Tabellen")
getwd()

# !!! data sets all with VD in their file name included as these are copies from the original output
# there might be a newer data set available

# data aggregation levels
HBI_stocks<-read.csv("HBI_LT_RG_DW_stocks_ha_all_groups - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_stocks)

BZE3_stocks<-read.csv("BZE3_LT_RG_DW_stocks_ha_all_groups - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_stocks)

change<-read.csv("HBI_BZE3_LT_RG_DW_changes_all_groups - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(change)


# data single trees
HBI_RG<-read.csv("HBI_RG_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_RG)
HBI_LT<-read.csv("HBI_LT_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_LT)
HBI_DW<-read.csv("HBI_DW_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_DW)

BZE3_RG<-read.csv("BZE3_RG_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_RG)
BZE3_LT<-read.csv("BZE3_LT_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_LT)
BZE3_DW<-read.csv("BZE3_DW_update_4 - VD.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_DW)


# data for plot status to compare if 0 values are okay
HBI_LT_stat_2<-read.csv("HBI_LT_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_LT_stat_2)
BZE3_LT_stat_2<-read.csv("BZE3_LT_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_LT_stat_2)

HBI_RG_stat_2<-read.csv("HBI_RG_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_RG_stat_2)
BZE3_RG_stat_2<-read.csv("BZE3_RG_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_RG_stat_2)

HBI_DW_stat_2<-read.csv("HBI_DW_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(HBI_DW_stat_2)
BZE3_DW_stat_2<-read.csv("BZE3_DW_stat_2.csv",sep=",", dec=".", header = T,stringsAsFactors = FALSE)
str(BZE3_DW_stat_2)


# functions --------------------------------------------------------------------------------

flag_outliers <- function(data, group_columns, value_column, 
                          summary_compartment = NULL, 
                          compartment_column = NULL) {
  
  # Create dynamic column names
  outlier_column <- paste0(value_column, "_outlier")
  mean_column <- paste0(value_column, "_group_mean")
  sd_column <- paste0(value_column, "_group_sd")
  
  # Initialize new columns
  data[[outlier_column]] <- NA
  data[[mean_column]] <- NA
  data[[sd_column]] <- NA
  
  # Get all unique group combinations
  unique_groups <- unique(data[, group_columns, drop = FALSE])
  
  for (i in 1:nrow(unique_groups)) {
    group_index <- rep(TRUE, nrow(data))
    for (col in group_columns) {
      group_index <- group_index & (data[[col]] == unique_groups[i, col])
    }
    
    group_rows <- which(group_index)
    group_data <- data[group_rows, ]
    
    group_mean <- mean(group_data[[value_column]], na.rm = TRUE)
    group_sd <- sd(group_data[[value_column]], na.rm = TRUE)
    
    outlier_condition <- abs(group_data[[value_column]] - group_mean) > 2 * group_sd
    
    data[[outlier_column]][group_rows] <- ifelse(
      is.na(data[[outlier_column]][group_rows]),
      ifelse(outlier_condition,
             ifelse(group_data[[value_column]] > group_mean,
                    "Above Mean 2sd", "Below Mean 2sd"),
             "No Outlier"),
      data[[outlier_column]][group_rows]
    )
    
    data[[mean_column]][group_rows] <- group_mean
    data[[sd_column]][group_rows] <- group_sd
    
    # ---- Optional: Handle summary compartment ----
    if (!is.null(summary_compartment) && !is.null(compartment_column) && compartment_column %in% names(data)) {
      summary_rows <- group_rows[data[group_rows, compartment_column] == summary_compartment]
      
      for (summary_row in summary_rows) {
        plot_id <- data[summary_row, "plot_ID"]
        
        compartment_rows <- which(
          data$plot_ID == plot_id &
            data[[compartment_column]] != summary_compartment
        )
        
        other_outliers <- data[[outlier_column]][compartment_rows] %in% c("Above Mean 2sd", "Below Mean 2sd")
        
        if (!is.na(data[[outlier_column]][summary_row]) && 
            data[[outlier_column]][summary_row] == "No Outlier" && 
            any(other_outliers, na.rm = TRUE)) {
          data[[outlier_column]][summary_row] <- "outlier_in_compartment"
        }
      }
    }
  }
  
  return(data)
}


# in the function there is the information missing on the direction of the not matching status: 
# is the outlier over- or under-shooting
advise_explanation_status <- function(data, ref_col, compare_cols) {
  # Ensure the reference column exists in the data
  if (!(ref_col %in% names(data))) {
    stop("Reference column is not present in the data.")
  }
  
  # Ensure all specified comparison columns exist in the data
  if (!all(compare_cols %in% names(data))) {
    stop("One or more comparison columns are not present in the data.")
  }
  
  # Apply logic to determine explanation status and add as a new column
  data$explanation_status <- apply(data, 1, function(row) {
    ref_value <- row[[ref_col]]
    
    # If the reference value is NA, skip the row and return NA for explanation status
    if (is.na(ref_value)) {
      return(NA)
    }
    
    comparisons <- row[compare_cols]
    
    # SPECIAL CASE:
    # If ref is "No Outlier" and all non-NA comparison columns are also "No Outlier",
    # then return "No Outlier"
    if (ref_value == "No Outlier") {
      non_na_comparisons <- comparisons[!is.na(comparisons)]
      if (length(non_na_comparisons) > 0 && all(non_na_comparisons == "No Outlier")) {
        return("No Outlier")
      }
    }
    
    # GENERAL CASE:
    # Treat NAs in comparison columns as "no match"
    matches <- comparisons == ref_value
    matches[is.na(comparisons)] <- FALSE  # Any NA in comparisons counts as no match
    
    if (all(!matches)) {
      return("unexplained")
    } else if (any(matches)) {
      return("partially explained")
    } else if (all(matches)) { # as in the flag_outliers-function above and below is already separated, this is enough as conditions
      return("explained")
    }
  })
  
  return(data)
}


handle_zero_biomass <- function(data, dataCompare, 
                                value_column, compartment, 
                                column_plot, column_zero, 
                                column_check, required_entries = NULL) {
  
  # Use all unique entries in column_check if not provided
  if (is.null(required_entries)) {
    required_entries <- unique(dataCompare[[column_check]])
  }
  
  # Create new status column name dynamically
  status_column <- paste0(value_column, "_Zero_stat_", compartment)
  data[[status_column]] <- NA
  
  # Loop through rows
  for (i in 1:nrow(data)) {
    value <- data[[value_column]][i]
    plot_id <- data$plot_ID[i]
    
    if (!is.na(value)) {
      if (value == 0) {
        # Filter dataCompare for this plot ID
        subset_compare <- dataCompare[dataCompare[[column_plot]] == plot_id, ]
        
        # Extract the column to check for required entries
        available_entries <- unique(subset_compare[[column_check]])
        
        # Check conditions
        all_zeros <- nrow(subset_compare) > 0 && all(subset_compare[[column_zero]] == 0, na.rm = TRUE)
        has_all_required_entries <- all(required_entries %in% available_entries)
        
        if (all_zeros && has_all_required_entries) {
          data[[status_column]][i] <- "OK"
        } else {
          data[[status_column]][i] <- paste0(value_column, "_Zero_NotIn_stats2")
        }
      } else {
        data[[status_column]][i] <- "Not Zero"
      }
    }
  }
  
  return(data)
}



###########################################################################
# per each aggregation levels on each plot
##########################################################################

# Flag outliers for HBI stocks ---------------------------------------------------------------

# without grouping, makes no sense due to very different ranges
#boxplot(HBI_stocks$B_t_ha)
#boxplot(HBI_stocks$C_t_ha)
#boxplot(HBI_stocks$N_t_ha)

HBI_stocks1 <- flag_outliers(HBI_stocks, group_columns = c("stand", "SP_code", "compartiment", "stand_component") , 
                             value_column = "B_t_ha", summary_compartment="total", compartment_column="compartiment")
HBI_stocks2 <- flag_outliers(HBI_stocks1, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                             value_column = "C_t_ha", summary_compartment="total", compartment_column="compartiment")
HBI_stocks3 <- flag_outliers(HBI_stocks2, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                             value_column = "N_t_ha", summary_compartment="total", compartment_column="compartiment")
HBI_stocks4 <- flag_outliers(HBI_stocks3, group_columns = c("stand", "SP_code", "compartiment", "stand_component"),
                             value_column = "n_ha", summary_compartment="total", compartment_column="compartiment")
HBI_stocks5 <- flag_outliers(HBI_stocks4, group_columns = c("stand", "SP_code", "compartiment", "stand_component"),
                             value_column = "mean_H_m", summary_compartment="total", compartment_column="compartiment")
HBI_stocks6 <- flag_outliers(HBI_stocks5, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                             value_column = "mean_DBH_cm", summary_compartment="total", compartment_column="compartiment")
HBI_stocks_outliers <- flag_outliers(HBI_stocks6, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                                     value_column = "BA_m2_ha", summary_compartment="total", compartment_column="compartiment")
rm(HBI_stocks1,HBI_stocks2,HBI_stocks3,HBI_stocks4,HBI_stocks5, HBI_stocks6)

HBI_stocks_outliers_ex<-advise_explanation_status(HBI_stocks_outliers, ref_col = "B_t_ha_outlier", 
                                                  compare_cols= c("n_ha_outlier","mean_H_m_outlier","mean_DBH_cm_outlier", "BA_m2_ha_outlier"))
HBI_stocks_outliers_ex01<-handle_zero_biomass(HBI_stocks_outliers_ex,  dataCompare=HBI_LT_stat_2,value_column = "B_t_ha",compartment="LT",
                                               column_plot="plot_ID",column_zero="B_CCS_t_ha", # here you check if all entries for the biomass at this plot are 0
                                              column_check="CCS_r_m", required_entries=c(17.84,5.64,12.62)) # here you check if all sub-plots are present in the "biomass is 0"-list
HBI_stocks_outliers_ex02<-handle_zero_biomass(HBI_stocks_outliers_ex01,  dataCompare=HBI_RG_stat_2,value_column = "B_t_ha",compartment="RG",
                                               column_plot="plot_ID",column_zero="B_t_ha",
                                              column_check="CCS_nr", required_entries=c(1,2,3,4))
HBI_stocks_outliers_ex0<-handle_zero_biomass(HBI_stocks_outliers_ex02,  dataCompare=HBI_DW_stat_2,value_column = "B_t_ha",compartment="DW",
                                              column_plot="plot_ID",column_zero="B_t_ha",
                                             column_check="plot_ID") # for dead wood we do not need required entries, as there is only one subplot per plot)
rm(HBI_stocks_outliers,HBI_stocks_outliers_ex)

# View the result
print(HBI_stocks_outliers_ex0)
write.csv(HBI_stocks_outliers_ex0,"outliers_HBI_stocks.csv")
#write.csv2(HBI_stocks_outliers,"outliers_HBI_stocks_2.csv")
#write.table(HBI_stocks_outliers,"outliers_HBI_stocks_t.csv")

# Flag outliers for BZE3 test stocks ---------------------------------------------------------------

# without grouping, makes no sense due to very different ranges
#boxplot(BZE3_stocks$B_t_ha)
#boxplot(BZE3_stocks$C_t_ha)
#boxplot(BZE3_stocks$N_t_ha)

BZE3_stocks1 <- flag_outliers(BZE3_stocks, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                              value_column = "B_t_ha", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks2 <- flag_outliers(BZE3_stocks1, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                              value_column = "C_t_ha", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks3 <- flag_outliers(BZE3_stocks2, group_columns = c("stand", "SP_code", "compartiment", "stand_component"),
                              value_column = "N_t_ha", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks4 <- flag_outliers(BZE3_stocks3, group_columns = c("stand", "SP_code", "compartiment", "stand_component"),
                              value_column = "n_ha", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks5 <- flag_outliers(BZE3_stocks4, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                              value_column = "mean_H_m", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks6 <- flag_outliers(BZE3_stocks5, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                              value_column = "mean_DBH_cm", summary_compartment="total", compartment_column="compartiment")
BZE3_stocks_outliers <- flag_outliers(BZE3_stocks6, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                                      value_column = "BA_m2_ha", summary_compartment="total", compartment_column="compartiment")
rm(BZE3_stocks1,BZE3_stocks2,BZE3_stocks3,BZE3_stocks4,BZE3_stocks5,BZE3_stocks6)

BZE3_stocks_outliers_ex<-advise_explanation_status(BZE3_stocks_outliers, ref_col = "B_t_ha_outlier", 
                                                  compare_cols= c("n_ha_outlier","mean_H_m_outlier","mean_DBH_cm_outlier", "BA_m2_ha_outlier"))
BZE3_stocks_outliers_ex01<-handle_zero_biomass(BZE3_stocks_outliers_ex,  dataCompare=BZE3_LT_stat_2,value_column = "B_t_ha",compartment="LT",
                                               column_plot="plot_ID",column_zero="B_CCS_t_ha", # here you check if all entries for the biomass at this plot are 0
                                               column_check="CCS_r_m", required_entries=c(17.84,5.64,12.62)) # here you check if all sub-plots are present in the "biomass is 0"-list)
BZE3_stocks_outliers_ex02<-handle_zero_biomass(BZE3_stocks_outliers_ex01,  dataCompare=BZE3_RG_stat_2,value_column = "B_t_ha",compartment="RG",
                                               column_plot="plot_ID",column_zero="B_t_ha",
                                               column_check="CCS_nr", required_entries=c(1,2,3,4))
BZE3_stocks_outliers_ex0<-handle_zero_biomass(BZE3_stocks_outliers_ex02,  dataCompare=BZE3_DW_stat_2,value_column = "B_t_ha",compartment="DW",
                                              column_plot="plot_ID",column_zero="B_t_ha",
                                              column_check="plot_ID") # for dead wood we do not need required entries, as there is only one subplot per plot
rm(BZE3_stocks_outliers,BZE3_stocks_outliers_ex,BZE3_stocks_outliers_ex02,BZE3_stocks_outliers_ex01)

# View the result
print(BZE3_stocks_outliers_ex0)
write.csv(BZE3_stocks_outliers_ex0,"outliers_BZE3_stocks.csv")
#write.csv2(BZE3_stocks_outliers,"outliers_BZE3_stocks_2.csv")
#write.table(BZE3_stocks_outliers,"outliers_BZE3_stocks_t.csv")

# Flag outliers for change between HBI and BZE3 stocks---------------------------------------------------------------

# without grouping, makes no sense due to very different ranges
#boxplot(change$B_t_ha_diff)
#boxplot(change$C_t_ha_diff)
#boxplot(change$N_t_ha_diff)

change1 <- flag_outliers(change, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                         value_column = "B_t_ha_diff", summary_compartment="total", compartment_column="compartiment")
change2 <- flag_outliers(change1, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                         value_column = "C_t_ha_diff", summary_compartment="total", compartment_column="compartiment")
change3 <- flag_outliers(change2, group_columns = c("stand", "SP_code", "compartiment", "stand_component"),
                         value_column = "N_t_ha_diff", summary_compartment="total", compartment_column="compartiment")
change4 <- flag_outliers(change3, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                         value_column = "n_ha_diff", summary_compartment="total", compartment_column="compartiment")
change5 <- flag_outliers(change4, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                         value_column = "mean_H_m_diff", summary_compartment="total", compartment_column="compartiment")
change6 <- flag_outliers(change5, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                         value_column = "mean_DBH_cm_diff", summary_compartment="total", compartment_column="compartiment")
change_outliers <- flag_outliers(change6, group_columns = c("stand", "SP_code", "compartiment", "stand_component"), 
                                 value_column = "mean_BA_m2_diff", summary_compartment="total", compartment_column="compartiment")
rm(change1,change2,change3,change4,change5,change6)

change_outliers_ex<-advise_explanation_status(change_outliers, ref_col = "B_t_ha_diff_outlier", 
                       compare_cols= c("n_ha_diff_outlier","mean_H_m_diff_outlier", "mean_DBH_cm_diff_outlier", "mean_BA_m2_diff_outlier"))
# without zero status, makes no scence, as this is differences

# View the result
print(change_outliers_ex)
write.csv(change_outliers_ex,"outliers_change_HBI_to_BZE3.csv")
#write.csv2(change_outliers,"outliers_change_HBI_to_BZE3_2.csv")
#write.table(change_outliers,"outliers_change_HBI_to_BZE3_t.csv")


###########################################################################
# per each single tree (do on HPC?)
##########################################################################

# Flag outliers for HBI single trees ---------------------------------------------------------------

### RG ###
HBI_RG1 <- flag_outliers(HBI_RG, group_columns = c( "SP_code", "compartiment","stand"), value_column = "B_kg_tree", 
                         summary_compartment="total", compartment_column="compartiment")
HBI_RG2 <- flag_outliers(HBI_RG1, group_columns = c( "SP_code", "compartiment","stand"), value_column = "N_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_RG_outliers <- flag_outliers(HBI_RG2, group_columns = c( "SP_code", "compartiment","stand"), value_column = "C_kg_tree",
                                 summary_compartment="total", compartment_column="compartiment")
rm(HBI_RG1,HBI_RG2)

# View the result
print(HBI_RG_outliers)
write.csv(HBI_RG_outliers,"outliers_HBI_RG.csv")
#write.csv2(HBI_RG_outliers,"outliers_HBI_RG_2.csv")
#write.table(HBI_RG_outliers,"outliers_HBI_RG_t.csv")

### LT ###
HBI_LT1 <- flag_outliers(HBI_LT, group_columns = c( "SP_code", "compartiment","stand"), value_column = "B_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT2 <- flag_outliers(HBI_LT1, group_columns = c( "SP_code", "compartiment","stand"), value_column = "N_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT3 <- flag_outliers(HBI_LT2, group_columns = c( "SP_code", "compartiment","stand"), value_column = "C_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT4 <- flag_outliers(HBI_LT3, group_columns = c( "SP_code", "compartiment","stand"), value_column = "H_m",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT5 <- flag_outliers(HBI_LT4, group_columns = c( "SP_code", "compartiment","stand"), value_column = "DBH_cm",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT6 <- flag_outliers(HBI_LT5, group_columns = c( "SP_code", "compartiment","stand"), value_column = "BA_m2",
                         summary_compartment="total", compartment_column="compartiment")
HBI_LT_outliers <- flag_outliers(HBI_LT6, group_columns = c( "SP_code", "compartiment","stand"), value_column = "age",
                                 summary_compartment="total", compartment_column="compartiment")
rm(HBI_LT1,HBI_LT2,HBI_LT3,HBI_LT4,HBI_LT5)

HBI_LT_outliers_ex<-advise_explanation_status(HBI_LT_outliers, ref_col = "B_kg_tree_outlier", 
                                                   compare_cols= c("H_m_outlier","BA_m2_outlier", "age_outlier", "DBH_cm_outlier"))
rm(HBI_LT_outliers)

# View the result
print(HBI_LT_outliers_ex)
write.csv(HBI_LT_outliers_ex,"outliers_HBI_LT.csv")
#write.csv2(HBI_LT_outliers,"outliers_HBI_LT_2.csv")
#write.table(HBI_LT_outliers,"outliers_HBI_LT_t.csv")

### DW ###
HBI_DW1 <- flag_outliers(HBI_DW, group_columns = c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "B_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_DW2 <- flag_outliers(HBI_DW1, group_columns = c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "N_kg_tree",
                         summary_compartment="total", compartment_column="compartiment")
HBI_DW_outliers <- flag_outliers(HBI_DW2, group_columns = c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "C_kg_tree",
                                 summary_compartment="total", compartment_column="compartiment")
rm(HBI_DW1,HBI_DW2)

# View the result
print(HBI_DW_outliers)
write.csv(HBI_DW_outliers,"outliers_HBI_DW.csv")
#write.csv2(HBI_DW_outliers,"outliers_HBI_DW_2.csv")
#write.table(HBI_DW_outliers,"outliers_HBI_DW_t.csv")


# Flag outliers for BZE3 single trees  ---------------------------------------------------------------

### RG ###
BZE3_RG1 <- flag_outliers(BZE3_RG, group_columns = c( "SP_code", "compartiment","stand"), value_column = "B_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_RG2 <- flag_outliers(BZE3_RG1, group_columns = c( "SP_code", "compartiment","stand"), value_column = "N_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_RG_outliers <- flag_outliers(BZE3_RG2, group_columns = c( "SP_code", "compartiment","stand"), value_column = "C_kg_tree",
                                  summary_compartment="total", compartment_column="compartiment")
rm(BZE3_RG1,BZE3_RG2)

# View the result
print(BZE3_RG_outliers)
write.csv(BZE3_RG_outliers,"outliers_BZE3_RG.csv")
#write.csv2(BZE3_RG_outliers,"outliers_BZE3_RG_2.csv")
#write.table(BZE3_RG_outliers,"outliers_BZE3_RG_t.csv")

### LT ###
BZE3_LT1 <- flag_outliers(BZE3_LT, group_columns = c( "SP_code", "compartiment","stand"), value_column = "B_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT2 <- flag_outliers(BZE3_LT1, group_columns = c( "SP_code", "compartiment","stand"), value_column = "N_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT3 <- flag_outliers(BZE3_LT2, group_columns = c( "SP_code", "compartiment","stand"), value_column = "C_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT4 <- flag_outliers(BZE3_LT3, group_columns = c( "SP_code", "compartiment","stand"), value_column = "H_m",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT5 <- flag_outliers(BZE3_LT4, group_columns = c( "SP_code", "compartiment","stand"), value_column = "DBH_cm",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT6 <- flag_outliers(BZE3_LT5, group_columns = c( "SP_code", "compartiment","stand"), value_column = "BA_m2",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_LT_outliers <- flag_outliers(BZE3_LT6, group_columns = c( "SP_code", "compartiment","stand"), value_column = "age",
                                  summary_compartment="total", compartment_column="compartiment")
rm(BZE3_LT1,BZE3_LT2,BZE3_LT3,BZE3_LT4,BZE3_LT5)

BZE3_LT_outliers_ex<-advise_explanation_status(BZE3_LT_outliers, ref_col = "B_kg_tree_outlier", 
                                              compare_cols= c("H_m_outlier","BA_m2_outlier", "age_outlier", "DBH_cm_outlier"))
rm(BZE3_LT_outliers)

# View the result
print(BZE3_LT_outliers_ex)
write.csv(BZE3_LT_outliers_ex,"outliers_BZE3_LT.csv")
#write.csv2(BZE3_LT_outliers,"outliers_BZE3_LT_2.csv")
#write.table(BZE3_LT_outliers,"outliers_BZE3_LT_t.csv")

### DW ###
BZE3_DW1 <- flag_outliers(BZE3_DW, group_columns = c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "B_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_DW2 <- flag_outliers(BZE3_DW1, group_columns =c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "N_kg_tree",
                          summary_compartment="total", compartment_column="compartiment")
BZE3_DW_outliers <- flag_outliers(BZE3_DW2, group_columns =c( "dw_type", "dw_sp","compartiment","SP_code"), value_column = "C_kg_tree",
                                  summary_compartment="total", compartment_column="compartiment")
rm(BZE3_DW1,BZE3_DW2)

# View the result
print(BZE3_DW_outliers)
write.csv(BZE3_DW_outliers,"outliers_BZE3_DW.csv")
#write.csv2(BZE3_DW_outliers,"outliers_BZE3_DW_2.csv")
#write.table(BZE3_DW_outliers,"outliers_BZE3_DW_t.csv")

