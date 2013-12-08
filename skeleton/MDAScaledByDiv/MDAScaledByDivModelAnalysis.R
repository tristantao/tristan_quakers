source('./SupportFunctions/ErrorAreaFunction.R')
load("./MDAScaledByDiv/MDAScaledByDivModel.RData")

# Analysis on W(M) = k * (nu ^M / M)
# The goodness of fit depend on what range of tau we use


# Test on error area for tau of different range
png(file = "./MDAScaledByDiv/errs0.png", 480, 320)
errs = get_error_rate_average(result_MDADiv)
plot(test_nu_MDADiv, errs, cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for all taus", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs1.png", 480, 320)
errs = get_error_rate_average(result_MDADiv, 0.01)
plot(test_nu_MDADiv, errs, cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau > 0.01", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs2.png", 480, 320)
errs2 = get_error_rate_average(result_MDADiv, 0.01, 0.749999)
plot(test_nu_MDADiv, errs2, col = 'red', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.75)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs3.png", 480, 320)
errs3 = get_error_rate_average(result_MDADiv, 0.01, 0.5)
plot(test_nu_MDADiv, errs3, col = 'blue', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.5)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs4.png", 480, 320)
errs4 = get_error_rate_average(result_MDADiv, 0.01, 0.375)
plot(test_nu_MDADiv, errs4, col = 'yellow', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.375)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs5.png", 480, 320)
errs5 = get_error_rate_average(result_MDADiv, 0.01, 0.25)
plot(test_nu_MDADiv, errs5, col = 'green', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.25)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs6.png", 480, 320)
errs6 = get_error_rate_average(result_MDADiv, 0.01, 0.125)
plot(test_nu_MDADiv, errs6, col = 'purple', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.125)", , xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDAScaledByDiv/errs7.png", 480, 320)
errs7 = get_error_rate_average(result_MDADiv, 0.3, 0.5)
plot(test_nu_MDADiv, errs7, col = 'purple', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.3, 0.5)", xlab = "nu", ylab = "error rate")
dev.off()

meanOfErrorRate = data.frame('r0.01To1' = errs, 
                             'r0.01To0.75' = errs2, 
                             'r0.01To0.5' = errs3, 
                             'r0.01To0.375' = errs4,
                             'r0.01To0.25' = errs5,
                             'r0.01To0.125' = errs6,
                             'r0.3To0.5' = errs7)
write.csv(meanOfErrorRate, file = './MDAScaledByDiv/MDADivMeanOfErrorRate.csv')

# Find for which nu is the area the minimum
min1 = which(errs == min(errs))
min2 = which(errs2 == min(errs2))
min3 = which(errs3 == min(errs3))
min4 = which(errs4 == min(errs4))
min5 = which(errs5 == min(errs5))
min6 = which(errs6 == min(errs6))


# Plot the Error Diagram
png(file = "./MDAScaledByDiv/errorDiagram1.png", 480, 320)
plot(result_MDADiv$nu6.6$taus, result_MDADiv$nu6.6$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 6.6', xlab = 'tau', ylab = 'v')
dev.off()

png(file = "./MDAScaledByDiv/errorDiagram2.png", 480, 320)
plot(result_MDADiv$nu7$taus, result_MDADiv$nu7$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 7', xlab = 'tau', ylab = 'v', col = 'red')
dev.off()

png(file = "./MDAScaledByDiv/errorDiagram3.png", 480, 320)
plot(result_MDADiv$nu7.2$taus, result_MDADiv$nu7.2$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 7.2', xlab = 'tau', ylab = 'v', col = 'blue')
dev.off()

png(file = "./MDAScaledByDiv/errorDiagram4.png", 480, 320)
plot(result_MDADiv$nu8$taus, result_MDADiv$nu8$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 8', xlab = 'tau', ylab = 'v', col = 'yellow')
dev.off()
