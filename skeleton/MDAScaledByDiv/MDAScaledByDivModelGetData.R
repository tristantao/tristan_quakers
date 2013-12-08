# This file test on a improved model from MDA: W(M) = k * (nu ^M / M)
# Get all tau and v information for different nus

source("./SupportFunctions/ErrorRateFunction.R")


# Function: get_error_diagrams

# Input:
# time: time for each earthquake in days
# magnitude: the magnitude of corresponding earthquakes
# nus: a vector of different nus to try]
# ts: a vector of different ts to try
# Output:  a list of lists of taus and error rates for different models
get_error_diagrams_MDADiv = function(time, magnitude, nus){
  result = list()
  for (i in nus){
    print(paste('nu: ', i))
    model = i ^ magnitude / magnitude
    result[[paste('nu', i, sep = '')]] = get_error_diagram(time, model)        
  }
  return (result)
}


# Get data, and we filter out earthquakes with magnitude <= 3
data = read.csv("./DataFrame.csv")
data = data[data$magnitude>3,]

# run get_error_diagrams
time = data$time
magnitude = data$magnitude
test_nu_MDADiv = seq(4, 8, 0.2)
result_MDADiv = get_error_diagrams_MDADiv(time, magnitude, test_nu_MDADiv)

save(result_MDADiv, test_nu_MDADiv, time, magnitude, file = './MDAScaledByDiv/MDAScaledByDivModel.RData')
