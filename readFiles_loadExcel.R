#install.packages("readr")
#install.packages("xlsx")
library(readr)
library(signal)
library(gsignal)
library(pracma)
library(dplyr)
library("xlsx")

#Set the directory in which the files are located 

setwd("~/Desktop/TFG_testsR/read_y_med/")

do_seg_process=function(a_csv){
  
  mdur_test <- read_delim(a_csv, 
                      ";", escape_double = FALSE, trim_ws = TRUE
                      )
  
  colnames(mdur_test)=c("col1", "col2", "col3", "col4", "col5", "col6", "frames", "biceps", "triceps")
  
  mdur_test2=data.frame(mdur_test$frames, mdur_test$biceps, mdur_test$triceps)
  colnames(mdur_test2)=c("frames", "biceps", "triceps")
  mdur_norm <- mdur_test2
  
  mdur_norm$biceps <- mdur_norm$biceps / max(mdur_norm$biceps)
  mdur_norm$triceps <- mdur_norm$triceps/ max(mdur_norm$triceps)
  #print(mdur_norm)
  
  emg_signal <- mdur_norm$triceps
  #print(emg_signal)
  
  first_10 = mdur_norm[1:10,'triceps']
  avg_tricep10 = mean(first_10)
  #avg_tricep10
  
  # Define the threshold value to be 25% more than the avg of the first 2.5 seconds
  cut_threshold <- avg_tricep10 + (avg_tricep10*0.25)
  #cut_threshold
  
  #Identify the peaks that surpass a threshold to later know where to cut the segments 
  peaks <- findpeaks(emg_signal, minpeakheight = cut_threshold*1.4)
  peak_positions <- peaks[,2]-1
  peak_positions
  
  start_idx <- peaks[1,3]
  
  
  # Initialize variables to define the start and end of each interval
  group_list <- list()
  start_count <- 0
  end_count <- 0
  min_distance = 16
  list_segments <- list()
  
  # Loop through each peak found 
  for (i in 1:(length(peak_positions)-1)) {
    
    # Check if the current peak is close to the previous peak
    if ((peak_positions[i+1] - peak_positions[i]) < min_distance) {
      
      # If it does, start a new segment or add to the current one
      if (start_count == 0) {
        start_idx <- peaks[i,3]
        start_count <- i
      }
      end_idx <- peaks[i+1, 4]
      
      
    } else {
      
      # If it does not, store the current segment and reset the variables
      if (start_count > 0) {
        #segment <- emg_signal[start_idx:end_idx]
        #list_segments[[length(segments) + 1]] <- segment
        
        current_df <- mdur_norm[start_idx:end_idx, c(1, 2, 3)]
        group_list[[length(group_list)+1]] <- current_df
      }
      start_count <- 0
      #start_idx <- peaks[i,3]
      #end_idx <- peaks[i, 4]
      # Store the previous segment and start a new one
    }
  }
  
  if (start_count > 0) {
    current_df <- mdur_norm[start_idx:end_idx, c(1, 2, 3)]
    group_list[[length(group_list)+1]] <- current_df
  }
  
  #Extract each element of the list y rename the columns
  #print(group_list) 
  #Extract each element of the list y rename the columns
  #group_list
  
  #Do downsampling to get all the segments of the same length 
  all_lengths <- c()
  list_triceps <- list()
  #len_segment <- length(group_list[[3]]$frames)
  #len_segment
  length(group_list)
  
  # Find the shortest segment
  for (i in 1:length(group_list)) {
    len_segment <- length(group_list[[i]]$frames)
    all_lengths <- c(all_lengths, len_segment)
    list_triceps[[length(list_triceps) + 1]] <- group_list[[i]]$triceps
  }
  #min_length <- min(all_lengths)
  #min_length
  
  # Define the desired output signal length
  desired_length <- 100
  desired_length
  
  
  # Resample each segment to match the desired length
  resample_biceps <- lapply(group_list, function(segment) {
    resampled_segment_bicep <- approx(segment, n = desired_length, method = "linear")$y
    return(resampled_segment_bicep)
  } )
  #print(resample_biceps)
  
  # Calculate the mean of the list of resampled segments
  mean_biceps <- apply(simplify2array(resample_biceps), 1, mean)
  #print(mean_biceps)
  # Plot the mean segment
  #plot(mean_biceps, type = "l", xlab = "Index", ylab = "Mean Value", main = "Mean Array")
  
  
  resample_triceps<- lapply(list_triceps, function(segment) {
    resampled_segment_tricep <- approx(segment, n = desired_length, method = "linear")$y
    
    return(resampled_segment_tricep)
  } )
  mean_triceps <- apply(simplify2array(resample_triceps), 1, mean)
  #print(resample_triceps)
  
  # Combine the resampled segments into a single dataframe for each muscle
  resampled_df_bicep <- as.data.frame(mean_biceps)
  #print(resampled_df_bicep)
  
  resampled_df_tricep <-as.data.frame(mean_triceps)
  #print(resampled_df_tricep)
  
  resampled_list_both <- list()
  resampled_list_both <- list(resampled_df_bicep,resampled_df_tricep)
  #print(resampled_list_both)
  return(resampled_list_both)
  
  # Optional: Plot the resampled segments
  #par(mfrow=c(2,1))
  #plot(group_list[[1]]$biceps, type="l", main="Original Signal")
  #plot(resampled_segments_df$V1, type="l", main="Resampled Signal")
  
}

the_dir_ex= "~/Desktop/TFG_testsR/read_y_med/"

#Make a list with all the csv files existing in the directory to later apply the function 
all_files=list.files("~/Desktop/TFG_testsR/read_y_med/", 
                     pattern = "*.csv", full.names = TRUE)
all_files


data_list_b <- list()
data_list_t <- list()

for (i in seq_along(all_files)) {
  
  # Extract data from the CSV file
  extracted_data <- do_seg_process(all_files[i])
  
  # Add the extracted data to the data lists
  data_list_b[[i]] <- extracted_data[[1]]
  data_list_t[[i]] <- extracted_data[[2]]
  
}

# Combine the data into one huge data frame
combined_data_biceps <- do.call(cbind, data_list_b)
#names(combined_data_biceps) <- c("P1", "P2","P3","P4", "P5","P6","P7", "P8","P9","P10", "P11","P12","P13","P14","P15", "P16")
combined_data_triceps <- do.call(cbind, data_list_t)
#names(combined_data_triceps) <- c("P1", "P2","P3","P4", "P5","P6","P7", "P8","P9","P10", "P11","P12","P13","P14","P15", "P16")

#Change columns into rows to be stored in excel
rows_data_biceps <-t(combined_data_biceps)
rows_data_triceps <-t(combined_data_triceps)


my_path <- "/Users/mariaceleste/Desktop/TFG_testsR/test_saveAll/"
write.xlsx2(rows_data_biceps, paste0(my_path, "activation_segments_young.xlsx"), append = TRUE,row.names = FALSE, sheetName = "biceps_med_y")
write.xlsx2(rows_data_triceps, paste0(my_path, "activation_segments_young.xlsx"), append = TRUE,row.names = FALSE, sheetName = "tricep_med_y")

#--------STOP---------------------------------------------------------------------------------

list_csv <- list()
for(file in all_files){
  
  mdur_test <-read_delim(file, 
                         delim=";", escape_double = FALSE, trim_ws = TRUE)
  colnames(mdur_test)=c("col1", "col2", "col3", "col4", "col5", "col6", "frames", "biceps", "triceps")
  
  mdur_test2=data.frame(mdur_test$frames, mdur_test$biceps, mdur_test$triceps)
  colnames(mdur_test2)=c("frames", "biceps", "triceps")
  mdur_norm <- mdur_test2
  
  #list_csv[[length(list_csv)+1]] <- df_seg2
  
  mdur_norm$biceps <- mdur_norm$biceps / max(mdur_norm$biceps)
  mdur_norm$triceps <- mdur_norm$triceps/ max(mdur_norm$triceps)
  
  emg_signal <- mdur_norm$triceps
  #print(emg_signal)

  #get the avg for the first 2.5 seconds para sacar el threshold
  #Here we set the threshold and minimum duration of each segment to detected the muscle activation segments
  
  first_10 = mdur_norm[1:10,'triceps']
  avg_tricep10 = mean(first_10)
  #avg_tricep10
  
  # Define the threshold value to be 25% more than the avg of the first 2.5 seconds
  cut_threshold <- avg_tricep10 + (avg_tricep10*0.25)
  #cut_threshold
  
  #Identify the peaks that surpass a threshold to later know where to cut the segments 
  peaks <- findpeaks(emg_signal, minpeakheight = cut_threshold*1.4)
  
  
  #if after 5 samples there is not peak than keep the last peak found and start a new segment
  #Slow - si la diferencia de posicion entre picos es mas de 15 entonces cortar en el último pico y me quedo con el ultimo minimo
  
  start_idx <- peaks[1,3]
  
  
  # Initialize variables to define the start and end of each interval
  group_list <- list()
  start_count <- 0
  end_count <- 0
  min_distance = 17
  list_segments <- list()
  
  
  #Loop through each peak found 
  for (i in 1:(length(peak_positions)-1)) {
    
    # Check if the current peak is close to the previous peak
    if ((peak_positions[i+1] - peak_positions[i]) < min_distance) {
      
      # If it does, start a new segment or add to the current one
      if (start_count == 0) {
        start_idx <- peaks[i,3]
        start_count <- i
      }
      end_idx <- peaks[i+1, 4]
      
      
    } else {
      
      # If it does not, store the current segment and reset the variables
      if (start_count > 0) {
        #segment <- emg_signal[start_idx:end_idx]
        #list_segments[[length(segments) + 1]] <- segment
        
        current_df <- mdur_test[start_idx:end_idx, c(1, 2, 3)]
        group_list[[length(group_list)+1]] <- current_df
      }
      start_count <- 0
      #start_idx <- peaks[i,3]
      #end_idx <- peaks[i, 4]
      # Store the previous segment and start a new one
    }
  }
  
  if (start_count > 0) {
    current_df <- mdur_test[start_idx:end_idx, c(1, 2, 3)]
    group_list[[length(group_list)+1]] <- current_df
  }
  
  #Extract each element of the list y rename the columns
  #group_list
  df_seg1 <- as.data.frame(group_list[1])
  colnames(df_seg1)[1] = "frame1"
  colnames(df_seg1)[2] = "bicep1"
  colnames(df_seg1)[3] = "tricep1"
  df_seg1
  
  df_seg2 <- as.data.frame(group_list[2])
  colnames(df_seg2)[1] = "frame2"
  colnames(df_seg2)[2] = "bicep2"
  colnames(df_seg2)[3] = "tricep2"
  df_seg2
  
  df_seg3 <- as.data.frame(group_list[3])
  colnames(df_seg3)[1] = "frame3"
  colnames(df_seg3)[2] = "bicep3"
  colnames(df_seg3)[3] = "tricep3"
  df_seg3
  
  
  
  list_csv[[length(list_csv)+1]] <- df_seg2
}


#get all the bicep columns written in an excel sheet 
#once I have made a dataframe with the 3 segments, extract all same variables (bicep/tricep) and put them in a different row in the corresponding excel sheet
#Make a dataframe with all the segments of the same variable and load that df into an excel sheet

  ################
  
