#' Import and organize backslip file names into a character vector.
#'
#'
#' @return A character vector listing all the names of .mat files.
#' @export
#'


list.mat <- function(){
     #generate a vector of all backslip data names
     path <- as.character(readline(prompt = "Enter backslip folder name:   "))
     bs.names <- list.files(path = paste0(getwd(),"/",path), pattern = ".mat")

     if(length(list.files(path = paste0(getwd(),"/",path), pattern = ".mat")) == 0){
          stop("ERROR: Folder not found. Check that the folder is in your working drive, and that the folder name is spelled correctly.")
     }else{
          return(bs.names)
     }
}



#' Combine .mat file data
#'
#'
#' Combine all offset data from the mat files.
#' Make sure each file name starts with a maximum 4-letter/number identifier.
#' @importFrom utils setTxtProgressBar txtProgressBar
#' @param folder_name A folder in your working directory that contains all
#'   the .mat files. Name should be in quotations, i.e. "folder_name".
#' @param bs_names A vector with the names of the backslip files.
#' @return A list of three matrices including lateral, vertical and total offset.
#' @export
#'
#'
#'


matrix.mat <- function(folder_name, bs_names){
     bs.init <- R.matlab::readMat(file.path(folder_name, bs_names[1]))
     #initialize a matrix from the first .mat data
     h_x = t(stats::na.omit(bs.init$XCORDATAH))
     h_y = t(stats::na.omit(bs.init$XCOR.SUMH))
     lateral = dplyr::bind_cols(h_x, h_y)
     colnames(lateral) = suppressMessages(c("h_x","h_y"))

     z_x = t(stats::na.omit(bs.init$XCORDATAV))[,1]
     z_y = t(stats::na.omit(bs.init$XCOR.SUMV))
     vertical = dplyr::bind_cols(x = z_x, y = z_y)
     colnames(vertical) = suppressMessages(c("z_x","z_y"))

     t_x = t(stats::na.omit(bs.init$XCORDATAT))
     t_y = t(stats::na.omit(bs.init$XCOR.SUMT))
     total = dplyr::bind_cols(x = t_x, y = t_y)
     colnames(total) = suppressMessages(c("t_x","t_y"))

     #progress bar

     n_iter <- length(bs_names)-1 # Number of iterations of the loop

     # Initializes the progress bar
     pb <- utils::txtProgressBar(min = 0,      # Minimum value of the progress bar
                          max = n_iter, # Maximum value of the progress bar
                          style = 3,    # Progress bar style (also available style = 1 and style = 2)
                          width = 50,   # Progress bar width. Defaults to getOption("width")
                          char = "=")   # Character used to create the bar

     suppressMessages(for(i in 2:length(bs_names)){

          pathname <- file.path(folder_name, bs_names[i])
          bs.data <- R.matlab::readMat(pathname)

          h_x = t(stats::na.omit(bs.data$XCORDATAH))
          h_y = t(stats::na.omit(bs.data$XCOR.SUMH))
          h_all = dplyr::bind_cols(h_x,h_y)
          colnames(h_all) = c("h_x","h_y")

          z_x = t(stats::na.omit(bs.data$XCORDATAV))[,1]
          z_y = t(stats::na.omit(bs.data$XCOR.SUMV))
          z_all = dplyr::bind_cols(z_x,z_y)
          colnames(z_all) = c("z_x","z_y")

          t_x = t(stats::na.omit(bs.data$XCORDATAT))
          t_y = t(stats::na.omit(bs.data$XCOR.SUMT))
          t_all = dplyr::bind_cols(t_x, t_y)
          colnames(t_all) = c("t_x","t_y")

          lateral <- dplyr::left_join(lateral,h_all, by = "h_x")
          vertical <- dplyr::left_join(vertical,z_all, by = "z_x")
          total <- dplyr::left_join(total,t_all, by = "t_x")

          utils::setTxtProgressBar(pb, i)
     })
     close(pb)
     #renaming
     colnames(lateral)[2:(length(bs_names)+1)] <- substr(bs_names,1,4)
     colnames(vertical)[2:(length(bs_names)+1)] <- substr(bs_names,1,4)
     colnames(total)[2:(length(bs_names)+1)] <- substr(bs_names,1,4)
     return(list(backslip_lat = lateral,
                 backslip_vert = vertical,
                 backslip_tot = total))
}
