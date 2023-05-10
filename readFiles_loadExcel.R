#install.packages("readr")
library(readr)

#Indícale a R en qué ruta está la carpeta donde están contenidos todos los archivos que quieres usar

all_files=list.files("/Users/mariaceleste/Desktop/TFG_testsR/test_readAll", 
                     pattern = "*.csv", full.names = TRUE)

for(file in all_files){
  
  X20_slow_y_bt <- read_delim("~/Desktop/TFG_testsR/Trials/20_slow_y_bt.csv", 
                              delim = ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                                   `rms_mean_(µV)` = col_skip(), `rms_mean_sec_(µV/s)` = col_skip(), 
                                                                                   `mvc_(µV)` = col_skip(), `mvc_h_(µV)` = col_skip(), 
                                                                                   ...6 = col_skip()), trim_ws = TRUE)
  #Lee el arhivo i-ésimo (puedes hacerlo así, o como tú lo tengas en tu script)
  mdur_test <- read_delim(file, 
                        ";", escape_double = FALSE, col_types = cols(...1 = col_skip(), 
                                                                     `rms_mean_(µV)` = col_skip(), `rms_mean_sec_(µV/s)` = col_skip(), 
                                                                     `mvc_(µV)` = col_skip(), `mvc_h_(µV)` = col_skip(), 
                                                                     ...6 = col_skip()), trim_ws = TRUE)
  mdur_norm <- mdur_test
  mdur_norm$`rms_biceps_brachii_(right)_(µV)` <- mdur_norm$`rms_biceps_brachii_(right)_(µV)` / max(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
  mdur_norm$`rms_triceps_brachii_long_(right)_(µV)` <- mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`/ max(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)
  # view the normalized dataframe
  #View(mdur_norm)
  
  emg_signal <- mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`
  #emg_signal
  #emg_env <- abs(hilbert(emg_signal))
  #plot(x = mdur_norm$`frames_(0.25s)`,y = emg_env,type='l')
  
  #Active window for:
  # 0.35hz (21bpm) - 18 a 20 segundos
  # 0.50hz (30bpm) - 11 a 13 segundos
  # 0.65hz (39bpm) - 10 a 11.50 segundos (40 frames of difference)
  
  #get the avg for the first 2.5 seconds para sacar el threshold
  # Here we set the threshold and minimum duration of each segment to detected the muscle activation segments
  first_10 = mdur_norm[1:10,'rms_triceps_brachii_long_(right)_(µV)']
  avg_tricep10 = mean(first_10$`rms_triceps_brachii_long_(right)_(µV)`)
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
  
  max_length <- max(c(nrow(df_seg1),nrow(df_seg2), nrow(df_seg3)))    
  #max_length   
  
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
  #combined_df 
  
  setwd ("C:/RUTA/.../CarpetaDESTINO")
  write.table(x=combined_df, file=file, sep=";", row.names=FALSE)
  
  #Si quieres hacer un único data.frame con todos los datos de todos los arvhivos, debes generar el código para crearlo
  #primero y, después, generar un único archivo excel (tras cada iteración del bucle, debe ir rellenándose ese
  #"gran" data frame que contenga todo)
  
  ################
  #Código para ir creando en cada iteración un único data.frame con toda la información
  ################
  
}