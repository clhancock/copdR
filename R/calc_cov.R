#' Calculate the coefficient of variation
#'
#'
#' Calculate the coefficient of variation (COV) values for lateral, vertical,
#' or total offset values.
#'
#' @importFrom dplyr mutate
#' @importFrom tidyselect all_of
#' @importFrom magrittr %>%
#' @param backslip_ref A reference datatable containing confidence values.
#' @param offset_type Input can be "lateral", "vertical" or "total". Any other input
#' will result in an error.
#' @param stdev A number referencing the column number of standard deviations
#' in the reference datatable 'backslip_ref'
#' @param mean A number referencing the column number of means in the reference
#' datatable 'backslip_ref'
#' @return The input datatable with an additional column of COV values
#' @export
#'
#'
#'
#'
#

calc.cov <- function(backslip_ref, offset_type, mean, stdev){
     if(offset_type == "lateral"){
          backslip_ref2 <- dplyr::mutate(backslip_ref, COV_lat = (backslip_ref[stdev]/backslip_ref[mean]))
     }
     if(offset_type == "vertical"){
          backslip_ref2 <- dplyr::mutate(backslip_ref, COV_vert = (backslip_ref[stdev]/backslip_ref[mean]))
     }
     if(offset_type == "total"){
          backslip_ref2 <- dplyr::mutate(backslip_ref, COV_tot = (backslip_ref[stdev]/backslip_ref[mean]))
     }
     return(backslip_ref2)
}
