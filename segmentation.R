#install.packages('tidyverse')
#install.packages("qpcR")
#install.packages("minpack.lm")

library(readxl)
library(dplyr)
library(readr)
library(ggplot2)


#Make same code for brachioradialis
#Made a loop to make the same for all the patients 

#Import the emg data gathered by mdurance from each patient and store it in a dataframe

mdur_prueba1 <- seg_prueba1
View(mdur_prueba1)
#-----------------------------------------------------------------------------
# Create a new dataframe with normalized values for each column
mdur_norm <- mdur_prueba1
mdur_norm$`rms_biceps_brachii_(right)_(µV)` <- mdur_norm$`rms_biceps_brachii_(right)_(µV)` / max(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
mdur_norm$`rms_triceps_brachii_long_(right)_(µV)` <- mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`/ max(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)
# view the normalized dataframe
View(mdur_norm)


#seg_prueba1 <- read_excel("Desktop/TFG_testsR/seg_prueba1.xls", 
#                          col_types = c("skip", "skip", "skip", 
#                                        "skip", "skip", "skip", "numeric", 
#                                        "numeric", "numeric"))
#View(seg_prueba1)

#?read_excel

mean(mdur_norm$`frames_(0.25s)`)
mean(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
mean(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)
#-----------------------------------------------------------------------------

#Get the avg of the tricep in the first 10 seconds
first_10sec = mdur_prueba1[1:10,'rms_triceps_brachii_long_(right)_(µV)']
avg_tricep10 = mean(first_10sec$`rms_triceps_brachii_long_(right)_(µV)`)
avg_tricep10

# Define the threshold value to be the double of the calculated avg within the first 10 seconds
cut_threshold <- avg_tricep10*2
cut_threshold
#upper_threshold <- avg_tricep10*2


#-----------------------------------------------------------------------------------------
# Define a list to store the groups of rows/segmensts found 
group_list <- list()
# Extract the rows that meet the threshold condition and store them in different columns

# Loop through each row in the dataframe based on the tricep muscle activity

# Initialize variables ro define the start and end of each interval
start_row <- 0
end_row <- 0
start_time <- 0
end_time <- 0

# Loop over each row in the dataframe mdur_norm
for (i in 1:nrow(mdur_prueba1)) {
  
  # Verify if the row meets the threshold condition 
  if (mdur_prueba1[i, "rms_triceps_brachii_long_(right)_(µV)"] >= cut_threshold) {
    
    # If it does, start a new dataframe or add to the current one
    if (start_row == 0) {
      start_row <- i
    }
    end_row <- i
    
  } else {
    
    # If it does not, store the current data frame and reset the variables
    if (start_row > 0) {
      current_df <- mdur_prueba1[start_row:end_row, c(1, 2, 3)]
      group_list[[length(group_list)+1]] <- current_df
    }
    start_row <- 0
    end_row <- 0
    
  }
  
}

# At the end of the dataframe mdur, store the new final data frame
if (start_row > 0) {
  current_df <- mdur_prueba1[start_row:end_row, c(1, 2, 3)]
  group_list[[length(group_list)+1]] <- current_df
}


#Extract each element of the list y rename the columns
group_list

#Make a condition that if the frames are less than 30 sec then discard the group


#Intentar hacer un for (i in 1:ncol(group_list)) {
  
#}
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


# Find out maximum row length to later column-bind the dfs
max_length <- max(c(nrow(df_seg1),nrow(df_seg2), nrow(df_seg3)))    
max_length   

if(nrow(df_seg1) < max_length){
  rows <- nrow(df_seg1)
  extra_rows = max_length - rows
  df_seg1[rows+ extra_rows,] <- NA
  
} else if(nrow(df_seg2) < max_length) {
  rows <- nrow(df_seg2)
  extra_rows = max_length - rows
  df_seg2[rows+ extra_rows,] <- NA
} else if(nrow(df_seg3) < max_length){
  rows <- nrow(df_seg3)
  extra_rows = max_length - rows
  df_seg3[rows+ extra_rows,] <- NA
  
  }
  

combined_df <- cbind(df_seg1, df_seg2, df_seg3)
combined_df # Print the combined data frame

#Each column is a segment from the cycle of a muscle 


#-----------------------------------------------------------------------------------------

# Create a dataframe to with the amplitudes normalized 

mdur_norm <- mdur_prueba1
max_bicep <- max(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
max_tricep <- max(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)

# apply the function to each bicep and tricep column
bicep_replace <- c("bicep1","bicep2","bicep3")
tricep_replace <- c("tricep1","tricep2","tricep3")

norm_ampli <- combined_df
norm_ampli[bicep_replace] <- norm_ampli[bicep_replace] / max_bicep
norm_ampli[tricep_replace] <- norm_ampli[tricep_replace] / max_tricep
norm_ampli
 
#Replace the missing values with 0 instead of NA
norm_ampli <- norm_ampli %>% 
  mutate_if(is.numeric, ~replace_na(., 0))
norm_ampli


mean(mdur_norm$`frames_(0.25s)`)
mean(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
mean(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)

# Print the new dataframe
#-----------------------------------------------------------------------------------------
# Make a normalization to the time axis with the min-max scaling method

time_df <- combined_df[,c(1,4,7)]
time_df

# define a function to perform min-max scaling
normalize <- function(x) {
  x <- x[!is.na(x)]
  return((x - min(x)) / (max(x) - min(x)))
}

# apply the function to each column
time_norm <- lapply(time_df[,1:2], normalize)

# convert the resulting matrix back to a dataframe
time_norm <- as.data.frame(time_norm)
time_norm <- time_norm[1]
#--------------------------------------------------------------------------------
#Graph the each segment with the x and y axis normalized

norm_ampli <- norm_ampli %>%
  select(2, 3, 5, 6, 8, 9)
all_norm_df <- cbind(time_norm,norm_ampli)

all_norm_df

all_norm_df %>%
  ggplot(aes(frame1,bicep1)) + geom_line()
all_norm_df %>%
  ggplot(aes(frame1,tricep1)) + geom_line()
#--------------------------------------------------------------------------------




