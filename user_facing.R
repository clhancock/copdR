library(readr)



## 1. Load in data ###

#create a reference vector of offset names
backslip_names <- list.mat()

#create a list containing three items, including 1. lateral, 2. vertical, 3. total offset
backslip_data <- matrix.mat(folder_name = "example_mat",backslip_names)

#load in your reference CSV. See example table "example.csv" for how your table should be formatted
backslip_ref <- read_csv("example.csv", show_col_types = FALSE)


## 2.






scaled <- as.data.frame(cbind(data[[1]][1], data[[1]][1:N+1]*(1/(backslip_ref[1:N,12]))
                              (data[[1]][2:N+1]*(1/(backslip_ref[1:N+1,12])))))

scale.test<- melt.data.frame(data_scaled, id.vars = "h_x", variable.name = "backslip_ID", value.name = "lateral")
ggplot(scale.test, aes(x=h_x, y = value, level = factor(variable)))+
     stat_align()+
     labs(title = "Left Lateral Displacement")

ggplot(test2, aes(x=h_x, y = value, level = factor(variable)))+
     stat_align()+
     labs(title = "Left Lateral Displacement")

confidence <- matrix(backslip_ref$confidence)


#initialize lateral scaled data
lat_scaled <-lateral_clip
#scaled lat data
lat_scaled[2:206] <- lat_scaled[2:206]*(1/confidence)

#initialize vertical scaled data
vert_scaled <-vertical
vert_scaled[2:206] <- vert_scaled[2:206]*(1/confidence)

#initialize total scaled data
total_scaled <-total
#scaled lat data
total_scaled[2:206] <- total_scaled[2:206]*(1/confidence)


visualize <- function(data_table, offset_type){
     if(offset_type == "lateral"){
          ggplot(data_table, aes(x=h_x, y = value, level = factor(variable)))+
               stat_align()+
               labs(title = "Left Lateral Displacement")
     }if(offset_type == "vertical"){

     }if(offset_type == "total"){

     }

}


#plot raw unfiltered data (this will probably be user-facing)
ggplot(test2, aes(x=h_x, y = value, level = factor(variable)))+
     stat_align()+
     labs(title = "Left Lateral Displacement")

ggplot(vert_melt, aes(x=x, y = vertical, level = factor(backslip_ID)))+
     stat_align()+
     xlim(-1,0)+
     labs(title = "vertical displacement magnitude")

ggplot(total_melt, aes(x=x, y = total, level = factor(backslip_ID)))+
     stat_align()+
     xlim(0,4)+
     labs(title = "total displacement magnitude")


library(reshape)

#visually inspect data
#which(lateral$x ==-5)
#lateral_clip <- lateral[501:1001,]
