# Analysis on the MDA models
# The goodness of fit depend on what range of tau we use

source('./SupportFunctions/ErrorAreaFunction.R')
load("./MDA/MDAModel.RData")


# Test on error area for tau of different range
png(file = "./MDA/errs0.png", 480, 320)
errs = get_error_rate_average(result_MDA)
plot(test_nu_MDA, errs, cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for all taus", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs1.png", 480, 320)
errs = get_error_rate_average(result_MDA, 0.01)
plot(test_nu_MDA, errs, cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau > 0.01", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs2.png", 480, 320)
errs2 = get_error_rate_average(result_MDA, 0.01, 0.75)
plot(test_nu_MDA, errs2, col = 'red', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.75)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs3.png", 480, 320)
errs3 = get_error_rate_average(result_MDA, 0.01, 0.5)
plot(test_nu_MDA, errs3, col = 'blue', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.5)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs4.png", 480, 320)
errs4 = get_error_rate_average(result_MDA, 0.01, 0.375)
plot(test_nu_MDA, errs4, col = 'yellow', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.375)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs5.png", 480, 320)
errs5 = get_error_rate_average(result_MDA, 0.01, 0.25)
plot(test_nu_MDA, errs5, col = 'green', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.25)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs6.png", 480, 320)
errs6 = get_error_rate_average(result_MDA, 0.01, 0.125)
plot(test_nu_MDA, errs6, col = 'purple', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.01, 0.125)", xlab = "nu", ylab = "error rate")
dev.off()

png(file = "./MDA/errs7.png", 480, 320)
errs7 = get_error_rate_average(result_MDA, 0.3, 0.5)
plot(test_nu_MDA, errs7, col = 'purple', cex = 0.5, pch = 19, type = 'b', main = "Error rate mean for tau (0.3, 0.5)", xlab = "nu", ylab = "error rate")
dev.off()

meanOfErrorRate = data.frame('r0.01To1' = errs, 
                            'r0.01To0.75' = errs2, 
                            'r0.01To0.5' = errs3, 
                            'r0.01To0.375' = errs4,
                            'r0.01To0.25' = errs5,
                            'r0.01To0.125' = errs6,
                            'r0.3To0.5' = errs7)
write.csv(meanOfErrorRate, file = './MDA/MDAMeanOfErrorRate.csv')

# Find for which nu is the area the minimum
min1 = which(errs == min(errs))
min2 = which(errs2 == min(errs2))
min3 = which(errs3 == min(errs3))
min4 = which(errs4 == min(errs4))
min5 = which(errs5 == min(errs5))
min6 = which(errs6 == min(errs6))


# Plot the Error Diagram
png(file = "./MDA/errorDiagram1.png", 480, 320)
plot(result_MDA$nu5.4$taus, result_MDA$nu5.4$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 5.4', xlab = 'tau', ylab = 'v')
dev.off()

png(file = "./MDA/errorDiagram2.png", 480, 320)
plot(result_MDA$nu5.8$taus, result_MDA$nu5.8$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 5.8', xlab = 'tau', ylab = 'v', col = 'red')
dev.off()

png(file = "./MDA/errorDiagram3.png", 480, 320)
plot(result_MDA$nu6.4$taus, result_MDA$nu6.4$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 6.4', xlab = 'tau', ylab = 'v', col = 'blue')
dev.off()

png(file = "./MDA/errorDiagram4.png", 480, 320)
plot(result_MDA$nu6.8$taus, result_MDA$nu6.8$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 6.8', xlab = 'tau', ylab = 'v', col = 'yellow')
dev.off()

png(file = "./MDA/errorDiagram5.png", 480, 320)
plot(result_MDA$nu7$taus, result_MDA$nu7$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 7', xlab = 'tau', ylab = 'v', col = 'green')
dev.off()

png(file = "./MDA/errorDiagram6.png", 480, 320)
plot(result_MDA$nu7.8$taus, result_MDA$nu7.8$vs, cex = 0.1, pch = 19, main = 'Error diagrams for nu = 7.8', xlab = 'tau', ylab = 'v', col = 'purple')
dev.off()
