# This file test on Leun's MDA model: W(M) = k * nu ^M
# Get all tau and v information for different nus

source("./SupportFunctions/ErrorRateFunction.R")


# Function: get_error_diagrams

# Input:
# time: time for each earthquake in days
# magnitude: the magnitude of corresponding earthquakes
# nus: a vector of different nus to try
# Output:  a list of lists of taus and error rates for different models
get_error_diagrams_MDA = function(time, magnitude, nus){
  result = list()
  for (i in nus){
    print(paste('nu: ', i))
    model = i ^ magnitude
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
test_nu_MDA = seq(4, 8, 0.2)
result_MDA = get_error_diagrams_MDA(time, magnitude, test_nu_MDA)

save(result_MDA, test_nu_MDA, time, magnitude, file = './MDA/MDAModel.RData')
