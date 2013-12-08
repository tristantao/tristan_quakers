library(scales)
source('./SupportFunctions/ErrorAreaFunction.R')
load("./MDAScaledBySub/MDAScaledBySubModel.RData")

png(file = "./MDAScaledBySub/errs1.png", 480, 320)
errs = get_error_rate_average(result_MDASub, 0.01)
plot(test_nu_MDASub, errs[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.5), main = "Error rate mean for tau > 0.01", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.5))
lines(test_nu_MDASub, errs[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.5))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs2.png", 480, 320)
errs2 = get_error_rate_average(result_MDASub, 0.01, 0.75)
plot(test_nu_MDASub, errs2[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.01, 0.75)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs2[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs2[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs3.png", 480, 320)
errs3 = get_error_rate_average(result_MDASub, 0.01, 0.5)
plot(test_nu_MDASub, errs3[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.01, 0.5)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs3[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs3[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs4.png", 480, 320)
errs4 = get_error_rate_average(result_MDASub, 0.01, 0.375)
plot(test_nu_MDASub, errs4[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.01, 0.375)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs4[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs4[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs5.png", 480, 320)
errs5 = get_error_rate_average(result_MDASub, 0.01, 0.25)
plot(test_nu_MDASub, errs5[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.01, 0.25)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs5[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs5[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs6.png", 480, 320)
errs6 = get_error_rate_average(result_MDASub, 0.01, 0.125)
plot(test_nu_MDASub, errs6[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.01, 0.125)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs6[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs6[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

png(file = "./MDAScaledBySub/errs7.png", 480, 320)
errs7 = get_error_rate_average(result_MDASub, 0.3, 0.5)
plot(test_nu_MDASub, errs7[seq(1,231, 11)], cex = 0.5, pch = 19, type = 'b', col = alpha('green',0.7), main = "Error rate mean for tau in range (0.3, 0.5)", xlab = 'nu', ylab = 'error rate')
lines(test_nu_MDASub, errs7[seq(6,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('blue', 0.7))
lines(test_nu_MDASub, errs7[seq(11,231,11)], cex = 0.5, pch = 19, type = 'b', col = alpha('red', 0.7))
legend('right', legend = c('t = 0', 't = 1', 't = 2'), fill = c('green', 'blue', 'red'), cex = 0.6)
dev.off()

meanOfErrorRate = data.frame('r0.01To1' = errs, 
                             'r0.01To0.75' = errs2, 
                             'r0.01To0.5' = errs3, 
                             'r0.01To0.375' = errs4,
                             'r0.01To0.25' = errs5,
                             'r0.01To0.125' = errs6,
                             'r0.3To0.5' = errs7)
write.csv(meanOfErrorRate, file = './MDAScaledBySub/MDASubMeanOfErrorRate.csv')

png(file = "./MDAScaledBySub/errst1.png", 480, 320)
plot(test_t_MDASub, errs[1:11], cex = 0.5, pch = 19, col = alpha('green', 0.7), main = 'Area under ErrorDiagram for tau > 0.01, nu = 4', xlab = 't', ylab = 'error rate')
dev.off()

png(file = "./MDAScaledBySub/errst2.png", 480, 320)
plot(test_t_MDASub, errs[56:66], cex = 0.5, pch = 19, col = alpha('red', 0.7), main = 'Area under ErrorDiagram for tau > 0.01, nu = 5', xlab = 't', ylab = 'error rate')
dev.off()

png(file = "./MDAScaledBySub/errst3.png", 480, 320)
plot(test_t_MDASub, errs[111:121], cex = 0.5, pch = 19, col = alpha('blue', 0.7), main = 'Area under ErrorDiagram for tau > 0.01, nu = 6', xlab = 't', ylab = 'error rate')
dev.off()

png(file = "./MDAScaledBySub/errst4.png", 480, 320)
plot(test_t_MDASub, errs[166:176], cex = 0.5, pch = 19, col = alpha('yellow', 0.7), main = 'Area under ErrorDiagram for tau > 0.01, nu = 7', xlab = 't', ylab = 'error rate')
dev.off()

png(file = "./MDAScaledBySub/errst5.png", 480, 320)
plot(test_t_MDASub, errs[221:231], cex = 0.5, pch = 19, col = alpha('purple', 0.7), main = 'Area under ErrorDiagram for tau > 0.01, nu = 8', xlab = 't', ylab = 'error rate')
dev.off()


