# Function: get_error_rate

# Input:
# time: time for each earthquake in days
# window_size: length of alarm for each corresponding earthquake in days
# output: a list with element 'v' and 'tau'
# v: the error rate for the input
# tau: time covered by alarm / (time[last] - time[first]) 
get_error_rate = function(time, window_size){

  # check for input
  if (!is.numeric(time) | !is.numeric(window_size)) stop('time and window_size must be all numeric')
  if (length(time) != length(window_size)) stop('time and window_size should have same length')
  if (any(is.na(time) | is.null(time) | is.nan(time) | ! is.numeric(time))
      | any(is.na(window_size) | is.null(window_size | is.nan(window_size | ! is.numeric(window_size))))) stop('time and window_size shoud be number wtih no NA, NULL or NAN')
  
  # Get information about the input
  size = length(time)  
  right_alarm = time + window_size
  right_alarm[right_alarm>max(time)] = max(time) # time won't go beyond the last earthquake
  
  # Initialize variables
  actual_left = rep(0, size)
  actual_right = rep(0, size)  
  right_max_so_far = 0
  predicted = rep(FALSE, size)
  
  # Get actual alarms for each earthquake
  for (i in 1:size){
    # Case two earthquakes occurs at the same time (not likely)
    if ((i != 1) && (time[i] == time[i-1])){
      if (right_alarm[i] > right_max_so_far){
        actual_left[i] = right_max_so_far
        actual_right[i] = right_max_so_far = right_alarm[i]
      }
      predicted[i] = predicted[i-1]
    # Case the earthquake is not predicted  
    } else if (time[i] > right_max_so_far){
      actual_left[i] = time[i]
      actual_right[i] = right_max_so_far = right_alarm[i]
    # Case the earthquake is predicted and its alarm is longer than the existing one
    } else if (right_alarm[i] >right_max_so_far){
      actual_left[i] = right_max_so_far
      actual_right[i] = right_max_so_far = right_alarm[i]
      predicted[i] = TRUE
    # Case the earthquake is predicted and its alarm is shorter than the existing one
    } else {
      predicted[i] = TRUE
    }
  }
  
  # Get v and tau
  v = 1 - sum(predicted) / size
  tau = sum(actual_right - actual_left)/(max(time) - min(time))
  
  # Return the list
  return (list('v' = v, 'tau' = tau))
}



# Function: get_error_diagram

# Input:
# time: time for each earthquake in days
# model: the winodw size without the scaling variable k
# k: list of 1000 scaling variables to change window size, and thus the tau
#    default ensures that tau and almost evenly distributed in range(0,1)
# Output: a list of taus and error rates for a specific model
get_error_diagram = function(time, model, k = 0.98 ^ (1:1000)){
  
  # Check input
  if (any(is.na(k) | is.null(k) | is.nan(k) | !is.numeric(k))) stop("k should be number with no NA, NULL or NAN")
  
  # Initilize variables
  size = length(k) 
  vs = rep(0, size)
  taus = rep(0, size)
  
  # Get tau and error for each given k
  for (i in 1:size){
    window_size = k[i] * model
    result = get_error_rate(time, window_size)
    vs[i] = result$v
    taus[i] = result$tau
    # These are here to ensure you the program is still running 
    if (i%%5 == 0){
      print(paste("get the first ", i, " results"))
    }
  } 
  
  # Return the result
  return (list("vs" = vs, "taus" = taus))
}
