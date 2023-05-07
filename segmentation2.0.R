
#New option of segmentation to based on the average moving window
library(dplyr)
library(signal)
library(gsignal)
library(pracma)
library(spectral)
library(zoo)

#Load all the files at once                                                                         
mdur_prueba1 <- X20_slow_y_bt
#X20_slow_y_bt
#X10_slow_y_bt
#X25_fast_y_bt
#seg_prueba1
View(mdur_prueba1)
#-----------------------------------------------------------------------------
# Create a new dataframe with normalized values for each column
mdur_norm <- mdur_prueba1
mdur_norm$`rms_biceps_brachii_(right)_(µV)` <- mdur_norm$`rms_biceps_brachii_(right)_(µV)` / max(mdur_norm$`rms_biceps_brachii_(right)_(µV)`)
mdur_norm$`rms_triceps_brachii_long_(right)_(µV)` <- mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`/ max(mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`)
# view the normalized dataframe
#View(mdur_norm)

emg_signal <- mdur_norm$`rms_triceps_brachii_long_(right)_(µV)`
emg_signal
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
avg_tricep10

# Define the threshold value to be 20% more than the avg of the first 2.5 seconds
cut_threshold <- avg_tricep10 + (avg_tricep10*0.25)
cut_threshold

#Identify the peaks that surpass a threshold to later know where to cut the segments 
peaks <- findpeaks(emg_signal, minpeakheight = cut_threshold*1.6)

peak_positions <- peaks[,2]-1
peak_positions
# Print the list of peak positions
length(peaks)
#get the position of the peaks 
peak_position <- c()

#if after 5 samples there is not peak than keep the last peak found and start a new segment
#Slow - si la diferencia de posicion entre picos es mas de 15 entonces cortar en el último pico y me quedo con el ultimo minimo
min_distance = 15
segments <- list()
start_idx <- peaks[1]
peak_positions[2] - peak_positions[2 - 1]

for (i in 2:length(peaks)) {
 
  # Check if the current peak is close to the previous peak
  if (peaks[i] - peaks[i - 1] > min_distance) {
    end_idx <- peaks[i]
  } else {
    # Store the previous segment and start a new one
    segment <- x[start_idx:end_idx]
    segments[[length(segments) + 1]] <- segment
    start_idx <- peaks[i]
    end_idx <- peaks[i]
  }
}
# Store the last segment
segment <- x[start_idx:end_idx]
segments[[length(segments) + 1]] <- segment


#we go through the peaks excluding the first and the last peaks found
for (i in 2:(length(peaks) - 1)) {
  start_idx <- peaks[i] 
  end_idx <- peaks[i + 1] - 1
  segment <- x[start_idx:end_idx]
  segments[[i - 1]] <- segment
}

# Interpolate values at peak locations
env_peaks <- approx(1:length(emg_signal), emg_signal, peaks)
print(env_peaks)
#si despues de ciertos segundos no hay picos arriba del threashold que corte

# Plot the original signal and its envelope over the peaks
plot(emg_signal,type = "l", main = "EMG Signal (Peaks Only)")

points(peaks, env_peaks$y, col = "red", pch = 20)


for (i in seq_along(peaks)[-1]) {
  # check for onset
  if ((offset[i]-onset[i]) >= min_duration) {
    segment <- emg_signal[onset[i]:offset[i]]
    segment[!above_threshold[onset[i]:offset[i]]] <- 0
    segments[[i]] <- segment
  }
  
}

#si es el principio de la señal descartar el primero y el ultimo pico
#capturar desde unos segundos antes del primer pico y unos segundos despues del ultimo del segmento


#--------------------------------------------------------------------------------

detect_muscle_activation2 <- function(emg_signal, threshold, min_duration) {
  
  
  # First get the envelope of the EMG signal for each segment
  emg_env <- abs(hilbert(emg_signal))
  emg_env
  # Detect the onset and offset of muscle activation based on the envelope
  onset <- which(diff(emg_signal > threshold) == 1)
  offset <- which(diff(emg_signal > threshold) == -1)
  
  if (length(onset) > length(offset)) {
    onset <- onset[-length(onset)]
  }
  if (length(offset) > length(onset)) {
    offset <- offset[-1]
  }
  
  
  if (length(onset) > length(offset)) {
    offset <- c(offset, length(emg_signal))
  }
  dur <- offset - onset
  # Keep segments with duration above the minimum duration
  keep <- dur >= min_duration
  onset <- onset[keep]
  offset <- offset[keep]
  dur <- dur[keep]
  
  # Return the onset, offset, and duration of muscle activation segments
   list(onset = onset, offset = offset, duration = dur)
}


detect_activation_segments <- function(emg_signal, threshold, min_duration) {

  # initialize onset, offset, and segment variables
  onset <- NULL
  offset <- NULL
  segments <- list()
  
  # First get the higher peaks (envelope of the EMG signal) 
  emg_peaks <- abs(hilbert(emg_signal))
  # Detect the onset and offset of muscle activation based on the threshold
  # Based on the envelope check where the signal crosses below a threshold 
  onset <- which(diff(emg_env > threshold) == 1)
  offset <- which(diff(emg_env > threshold) == -1)

  duration <- offset - onset
  # Keep segments with duration equal or above the minimum duration
  keep <- duration >= min_duration  
  
  # loop over threshold crossings to extract muscle activation segments
  above_threshold <- emg_signal > threshold
  for (i in seq_along(above_threshold)[-1]) {
    # check for onset
    if ((offset[i]-onset[i]) >= min_duration) {
      segment <- emg_signal[onset[i]:offset[i]]
      segment[!above_threshold[onset[i]:offset[i]]] <- 0
      segments[[i]] <- segment
    }
    
  }
  
  # handle edge case where last value is above threshold
  if (above_threshold[length(emg_signal)]) {
    offset <- c(offset, length(emg_signal))
  }
  
  # ensure onset and offset vectors have the same length
  if (length(onset) > length(offset)) {
    offset <- c(offset, length(emg_signal))
  }
  
  # extract segments and set non-activated values to zero
  for (i in seq_along(onset)) {
    segment <- emg_signal[onset[i]:offset[i]]
   # segment[!above_threshold[onset[i]:offset[i]]] <- 0
    segments[[i]] <- segment
  }
  
  # return list of segments
  return(segments)
}


detect_muscle_activation3 <- function(emg_signal, window_size, sd_threshold = 3) {
  # apply sliding window to compute mean and standard deviation of EMG signal
  means <- rollapply(emg_signal, window_size, mean, align = "right", partial = TRUE)
  sds <- rollapply(emg_signal, window_size, sd, align = "right", partial = TRUE)
  
  # compute threshold as a function of mean and standard deviation
  threshold <- means + sd_threshold * sds
  
  # initialize onset, offset, and segment variables
  onset <- NULL
  offset <- NULL
  segments <- list()
  
  # loop over threshold crossings to extract muscle activation segments
  above_threshold <- emg_signal > threshold
  for (i in seq_along(above_threshold)[-1]) {
    # check for onset
    if (!above_threshold[i - 1] && above_threshold[i]) {
      onset <- c(onset, i)
    }
    
    # check for offset
    if (above_threshold[i - 1] && !above_threshold[i]) {
      offset <- c(offset, i - 1)
    }
  }
  
  # handle edge case where last value is above threshold
  if (above_threshold[length(emg_signal)]) {
    offset <- c(offset, length(emg_signal))
  }
  
  # ensure onset and offset vectors have the same length
  if (length(onset) > length(offset)) {
    offset <- c(offset, length(emg_signal))
  }
  
  # extract segments and set non-activated values to zero
  for (i in seq_along(onset)) {
    segment <- emg_signal[onset[i]:offset[i]]
    segment[!above_threshold[onset[i]:offset[i]]] <- 0
    segments[[i]] <- segment
  }
  
  # return list of segments
  return(segments)
}



min_duration <- 40 #10 segundos es decir los frames x 0.25
# Detect the muscle activation segments in the EMG signal
#segments <- detect_muscle_activation3(emg_signal, 100, 3)
segments <- detect_muscle_activation(emg_signal, cut_threshold, min_duration)
View(segments)

# Plot the original EMG signal and the segments with muscle activation
df <- data.frame(t = mdur_norm$`frames_(0.25s)`, emg_signal = emg_signal, segment = 0)
for (i in seq_along(segments$onset)) {
  emg_signal$segment[(segments$onset[i]+1):(segments$offset[i]+1)] <- i
  plot(segments[[i]], type = "l", main = paste0("Segment ", i))
}

t = mdur_norm$`frames_(0.25s)`
length(t)
View(dur)

for (i in seq_along(segments)) {
  plot(segments[[i]], type = "l", main = paste0("Segment ", i))
}


ggplot(df, aes(x = mdur_norm$`frames_(0.25s)` , y = emg_signal, color = factor(segment))) +
  geom_line() +
  scale_color_discrete(name = "Segment") +
  theme_classic()


# Compute the envelope of the EMG signal

# Detect the onset and offset of muscle activation based on the envelope of the bicep 
onset <- which(diff(emg_env > threshold) == 1)
offset <- which(diff(emg_env > threshold) == -1)

## Define the function for detecting muscle activation
detect_muscle_activation <- function(mdur_norm, threshold) {
  
  # Normalize the EMG signal
  emg_norm <- (mdur_norm - mean(mdur_norm)) / sd(mdur_norm)
  
  # Detect the onset and offset of muscle activation based on the threshold
  onset <- which(diff(emg_filt > threshold) == 1)
  offset <- which(diff(emg_filt > threshold) == -1)
  if (length(onset) > length(offset)) {
    onset <- onset[-length(onset)]
  }
  if (length(offset) > length(onset)) {
    offset <- offset[-1]
  }
  
# Merge segments that are too close to each other
merge_idx <- which(onset[2:length(onset)] - offset[1:(length(offset) - 1)] < min_duration)
onset <- onset[-(merge_idx + 1)]
offset <- offset[-merge_idx]

# Return the segments of the EMG signal with muscle activation
segments <- lapply(1:length(onset), function(i) emg_signal[onset[i]:offset[i]])
return(segments)
}


# Generate 


# Apply the Hilbert transform
h <- hilbert(emg_signal)
env <- abs(h)

# Plot the original signal and its envelope
plot(emg_signal, type = "l", main = "EMG Signal and Envelope")
lines(env, col = "red")
legend("topright", legend = c("EMG Signal", "Envelope"), col = c("black", "red"), lty = 1)