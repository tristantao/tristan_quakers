# This file is a demo on how to use get_error_diagram() in SupportFunction

library(scales)

# Suppose I want to test the MDA model: W(M) = k * nu ^M on data set demo.csv
setwd('~/stat157/TheQuakers/skeleton/')

source('./SupportFunctions/ErrorRateFunction.R')

# Read in the data
data = read.csv(file = './Demo/demo.csv')
time = data$time
magnitude = data$magnitude

##################################
# suppose I want to test on nu = 4
##################################

# Model is the window function without the scaling parameter k, which controls in window size
model = 4^magnitude 

# get_error_diagram() default to use k = 0.98^(1:1000), which is good for models in x^M form
result_nu_4 = get_error_diagram(time, model)

# You can set the k your self for the best set of window size
result_nu_4_k = get_error_diagram(time, model, 0.9^(1:100))

# The return value is the taus and vs for each k
names(result_nu_4)
print(paste('For result_nu_4, tau is a ',class(result_nu_4$taus), ' of length ', length(result_nu_4$taus), sep = ''))
print(paste('For result_nu_4, vs is a ',class(result_nu_4$vs), ' of length ', length(result_nu_4$vs), sep = ''))

# The function will check the validlity of input
result_wrong_input = get_error_diagram(1, model)
result_wrong_input = get_error_diagram(1, model, 'a')
result_wrong_input = get_error_diagram("a, b", model)
result_wrong_input = get_error_diagram(c(NA, time[-1]), model)

# What the error diagram look like for result_nu_4
plot(result_nu_4$taus, result_nu_4$vs, pch = 19, cex = 0.1, main = "Error Digram", xlab = 'nu=4', ylab = 'error rate')

# What the error diagram look like for result_nu_4_k 
plot(result_nu_4_k$taus, result_nu_4_k$vs, pch = 19, cex = 0.5, main = "Error Digram", xlab = 'nu=4', ylab = 'error rate')
# As you can see, using k = 0.9^(1:100) doesn't give us that many points for tau in range(0.2, 0.8)
# So picking the right k set is very important

########################################
# suppose I want to test on a set of nus
########################################

# Please put all the return values of get_erro_diagram() 
# in a list to use get_error_rate_average for analysis

result_different_nus = list()

# We want to test on nu = 4, 5, 6
nus = 4:6

# Get taus and vs for each model
for (i in nus){
  print(paste('nu:', i))
  model = i^magnitude 
  result_different_nus[[paste('nu', i, sep='')]] = get_error_diagram(time, model)
}

# What the error diagrams looks like
plot(result_different_nus$nu4$taus, result_different_nus$nu4$vs, pch = 19, cex = 0.1, col = alpha('green', 0.5), main = "Error Digram", xlab = 'nu=4', ylab = 'error rate')
points(result_different_nus$nu5$taus, result_different_nus$nu5$vs, pch = 19, cex = 0.1, col = alpha('red', 0.5))
points(result_different_nus$nu6$taus, result_different_nus$nu6$vs, pch = 19, cex = 0.1, col = alpha('blue', 0.5))

#############################################################
# Please save your result as RData somewhere, 
# because you might want to do other analysis on the data too
#############################################################
save(result_different_nus, nus, file = './Demo/demo.RData')
