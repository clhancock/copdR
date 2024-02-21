#' Melt backslip data table
#'
#'
#' Melt backslip data table into a three column data frame that can be
#' plotted in ggplot.
#'
#' @importFrom reshape melt.data.frame
#' @param data_table A single table with x values as the first column and every
#' subsequent column is a PDF of y values for a single offset site.
#' @param offset_type Input can be "lateral", "vertical" or "total". Any other input
#' will result in an error.
#' @return A data frame with 3 columns including x values, all backslip names,
#' and lateral values.
#' @export
#'

simplify4plot <- function(data_table, offset_type){
     if(offset_type == "lateral"){
          data <- as.data.frame(data_table)
          data_melt <- reshape::melt.data.frame(data, id.vars = "h_x", variable.name = "backslip_ID", value.name = "lateral")

     }
     if(offset_type == "vertical"){
          data <- as.data.frame(data_table)
          data_melt <- reshape::melt.data.frame(data, id.vars = "z_x", variable.name = "backslip_ID", value.name = "vertical")

     }
     if(offset_type == "total"){
          data <- as.data.frame(data_table)
          data_melt <- reshape::melt.data.frame(data, id.vars = "t_x", variable.name = "backslip_ID", value.name = "total")

     }else{
          stop("ERROR: Offset type not recognized. Function recognizes only 'lateral', 'vertical' or 'total' offset.")
     }

}
