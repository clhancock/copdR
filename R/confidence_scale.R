#' Scale offset values by confidence
#'
#'
#' Multiply each probability distribution function contained in a data frame by
#' a confidence value. Make sure confidence values are >= 1. Values equal to 1
#' are highest confidence measurements. Larger values (>1) are lower confidence.
#'
#'
#' @param offset_data A datatable with x values as the first column and probability
#' distribution functions (y values) as subsequent columns.
#' @param backslip_ref A datatable containing confidence values.
#' @param confidence_column The column number in a 'backslip_ref' table with confidence
#' values.
#' @param N The integer number of offset sites
#' (i.e. # of files contained in input mat folder)
#' @return A single datatable with backslip offsets scaled by measurement confidence
#' @export
#'
#'
#'

conf.scale <- function(offset_data, backslip_ref, confidence_column, N){
     x_val <- offset_data[1]
     y_scale <- offset_data[1:N+1]* t(1/(backslip_ref[1:N,confidence_column]))
     data_scaled <- as.data.frame(cbind(x_val, y_scale))
     colnames(data_scaled) = colnames(offset_data)
     return(data_scaled)

}

