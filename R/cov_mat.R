#' Create a reference matrix for filtered COV values
#'
#'
#' Create a modified reference table including offset IDs and columns of filtered COV values
#'
#' @importFrom dplyr filter left_join bind_cols
#' @importFrom tidyselect contains
#' @param backslip_ref A reference datatable containing coefficient of variation (COV) values.
#' @param offset_ID The column number in the reference datatable 'backslip_ref' corresponding
#' to the names of offset .mat files
#' @param offset_type Input can be "lateral", "vertical" or "total". Any other input
#' will result in an error.
#' @param COV_values The vector containing the cutoff COV values you would like
#' to create filtered datasets for.
#' @param backslip_data The list corresponding to combined lateral, vertical and total PDFs.
#' The default is 'backslip_data'.
#' @return A datatable with offset_IDs and associated COV values
#' @export
#'
#'
#'
#'
#'

filter.covREF <- function(backslip_ref,offset_ID,offset_type,COV_values,backslip_data = backslip_data){
     #initalize COV_matrix reference with offset ID column
     COV_mat <- backslip_ref[offset_ID]
     backslip_ref = as.data.frame(backslip_ref)

     if(offset_type == "lateral"){
          #complete loop to create filtered vectors and bind to offset_ID
          for(i in 1:length(COV_values)){
               COV_filtMat <- as.data.frame(backslip_ref[offset_ID])
               colnames(COV_filtMat) <- colnames(backslip_ref)[offset_ID]
               COV_valFilt <- dplyr::filter(backslip_ref, COV_lat <= COV_values[i])
               COV_filtMat <- dplyr::left_join(COV_filtMat,COV_valFilt, by = colnames(backslip_ref)[offset_ID])
               COV_filtVal <- as.matrix(COV_filtMat$COV_lat)
               COV_mat <- dplyr::bind_cols(COV_mat,COV_filtVal)
          }
          colnames(COV_mat)<- c("offset_ID", paste("lessthan",COV_values, sep="_"))

     }
     if(offset_type == "vertical"){
          #complete loop to create filtered vectors and bind to offset_ID
          for(i in 1:length(COV_values)){
               COV_filtMat <- as.data.frame(backslip_ref[offset_ID])
               colnames(COV_filtMat) <- colnames(backslip_ref)[offset_ID]
               COV_valFilt <- dplyr::filter(backslip_ref, COV_vert <= COV_values[i])
               COV_filtMat <- dplyr::left_join(COV_filtMat,COV_valFilt, by = colnames(backslip_ref)[offset_ID])
               COV_filtVal <- as.matrix(COV_filtMat$COV_vert)
               COV_mat <- dplyr::bind_cols(COV_mat,COV_filtVal)
          }
          colnames(COV_mat)<- c("offset_ID", paste("lessthan",COV_values, sep="_"))
     }
     if(offset_type == "total"){
          #complete loop to create filtered vectors and bind to offset_ID
          for(i in 1:length(COV_values)){
               COV_filtMat <- as.data.frame(backslip_ref[offset_ID])
               colnames(COV_filtMat) <- colnames(backslip_ref)[offset_ID]
               COV_valFilt <- dplyr::filter(backslip_ref, COV_tot <= COV_values[i])
               COV_filtMat <- dplyr::left_join(COV_filtMat,COV_valFilt, by = colnames(backslip_ref)[offset_ID])
               COV_filtVal <- as.matrix(COV_filtMat$COV_lat)
               COV_mat <- dplyr::bind_cols(COV_mat,COV_filtVal)
          }
          colnames(COV_mat)<- c("offset_ID", paste("lessthan",COV_values, sep="_"))
     }
     return(COV_mat)
}
