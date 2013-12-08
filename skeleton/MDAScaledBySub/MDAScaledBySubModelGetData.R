# This file test on a improved model from MDA: W(M) = k * (nu ^M - tM)
# Get all tau and v information for different nus

source("./SupportFunctions/ErrorRateFunction.R")


# Function: get_error_diagrams

# Input:
# time: time for each earthquake in days
# magnitude: the magnitude of corresponding earthquakes
# nus: a vector of different nus to try]
# ts: a vector of different ts to try
# Output:  a list of lists of taus and error rates for different models
get_error_diagrams_MDASub = function(time, magnitude, nus, ts){
  result = list()
  for (i in nus){
    for (j in ts){
      print(paste('nu: ', i, 't: ', j))
      model = i ^ magnitude - j * magnitude
      result[[paste('nu', i, 't', j, sep = '')]] = get_error_diagram(time, model)        
    }
  }
  return (result)
}


# Get data, and we filter out earthquakes with magnitude <= 3
data = read.csv("./DataFrame.csv")
data = data[data$magnitude>3,]

# run get_error_diagrams
time = data$time
magnitude = data$magnitude
test_nu_MDASub = seq(4, 8, 0.2)
test_t_MDASub = seq(0, 2, 0.2)
result_MDASub = get_error_diagrams_MDASub(time, magnitude, test_nu_MDASub, test_t_MDASub)

save(result_MDASub, test_nu_MDASub, test_t_MDASub, time, magnitude, file = './MDAScaledBySub/MDAScaledBySubModel.RData')
