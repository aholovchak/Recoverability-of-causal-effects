#' Generates data set with missing values in c-DAG variables due to missingness indicators' values
#' @param data_frame data for which missing data has to be imputed
#' @return data set with partially observed variables
#' If missingness only in VL_t and MEMS_t, this corresponds to Simulation 1.
#' If missingness in VL_t, MEMS_t, C12_t, weight_t, this corresponds to Simulation 2 (uncomment the corresponding lines to do so).

data_missing_wide <- function(data_frame){
  for(i in 1:length(data_frame[,1])){
    #if(data_frame[i, "M.weight_0"] == 1) data_frame[i, "Weight_0"] <- NA
    if(data_frame[i, "M.VL_0"] == 1) data_frame[i, "VL_0"] <- NA

    #if(data_frame[i, "M.C12_1"] == 1) data_frame[i, "C12_1"] <- NA
    #if(data_frame[i, "M.weight_1"] == 1) data_frame[i, "Weight_1"] <- NA
    if(data_frame[i, "M.VL_1"] == 1) data_frame[i, "VL_1"] <- NA
    if(data_frame[i, "M.MEMS_1"] == 1) data_frame[i, "MEMS_1"] <- NA

    #if(data_frame[i, "M.C12_2"] == 1) data_frame[i, "C12_2"] <- NA
    #if(data_frame[i, "M.weight_2"] == 1) data_frame[i, "Weight_2"] <- NA
    if(data_frame[i, "M.VL_2"] == 1) data_frame[i, "VL_2"] <- NA
    if(data_frame[i, "M.MEMS_2"] == 1) data_frame[i, "MEMS_2"] <- NA

    #if(data_frame[i, "M.C12_3"] == 1) data_frame[i, "C12_3"] <- NA
    #if(data_frame[i, "M.weight_3"] == 1) data_frame[i, "Weight_3"] <- NA
    if(data_frame[i, "M.VL_3"] == 1) data_frame[i, "VL_3"] <- NA
    if(data_frame[i, "M.MEMS_3"] == 1) data_frame[i, "MEMS_3"] <- NA

    #if(data_frame[i, "M.C12_4"] == 1) data_frame[i, "C12_4"] <- NA
    #if(data_frame[i, "M.weight_4"] == 1) data_frame[i, "Weight_4"] <- NA
    if(data_frame[i, "M.VL_4"] == 1) data_frame[i, "VL_4"] <- NA
    if(data_frame[i, "M.MEMS_4"] == 1) data_frame[i, "MEMS_4"] <- NA

    #if(data_frame[i, "M.C12_5"] == 1) data_frame[i, "C12_5"] <- NA
    #if(data_frame[i, "M.weight_5"] == 1) data_frame[i, "Weight_5"] <- NA
    if(data_frame[i, "M.VL_5"] == 1) data_frame[i, "VL_5"] <- NA
    if(data_frame[i, "M.MEMS_5"] == 1) data_frame[i, "MEMS_5"] <- NA
  }
  return(data_frame)
}


data_missing_long <- function(data_frame){
  for(i in 1:length(data_frame[,1])){
    #if(data_frame[i, "M.C12"] == 1) data_frame[i, "C12"] <- NA
    #if(data_frame[i, "M.weight"] == 1) data_frame[i, "Weight"] <- NA
    if(data_frame[i, "M.VL"] == 1) data_frame[i, "VL"] <- NA
    if(data_frame[i, "M.MEMS"] == 1) data_frame[i, "MEMS"] <- NA
  }
  return(data_frame)
}
