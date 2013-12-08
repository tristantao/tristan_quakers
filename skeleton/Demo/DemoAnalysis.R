# This file is a demo on how to use get_error_rate_average() in SupportFunction

source('./SupportFunctions/ErrorAreaFunction.R')

load('./Demo/demo.RData')

plot(result_different_nus$nu4$taus, result_different_nus$nu4$vs, pch = 19, cex = 0.1, col = alpha('green', 0.5), main = "Error Digram", xlab = 'nu=4', ylab = 'error rate')
points(result_different_nus$nu5$taus, result_different_nus$nu5$vs, pch = 19, cex = 0.1, col = alpha('red', 0.5))
points(result_different_nus$nu6$taus, result_different_nus$nu6$vs, pch = 19, cex = 0.1, col = alpha('blue', 0.5))

# You might be wondering how to check which model is better. 
# get_error_rate_average() returns the mean of error rate for each model
errs = get_error_rate_average(result_different_nus)
errs

# Plot it
plot(nus, errs, cex = 0.5, pch = 19, type = 'b', col = 'green', main = "error rate mean", ylim = c(0.4, 0.55), xlab = 'nu', ylab = 'error rate')

# Actually, if tau is bigger than 0.5, the model is not that useful, so you should provide a bound to measure
errs1 = get_error_rate_average(result_different_nus, rightBound = 0.75)
points(nus, errs1, cex = 0.5, pch = 19, type = 'b', col = 'red')

# Also , if tau is smaller than 0.01, whcih means that the alarm will be up for less than 4 days each year,
# it is not likely we can catch any earthquakes.
errs3 = get_error_rate_average(result_different_nus, 0.05, 0.75)
points(nus, errs3, cex = 0.5, pch = 19, type = 'b', col = 'blue')
legend(5.7, 0.47, legend = c('all', '<0.75', '0.05-0.75'), fill = c('green', 'red', 'blue'), cex = 0.5)
