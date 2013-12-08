Something about the ETAS model:  
* The ETAS model is based on point process modelling of the seismicity and based on 3 assumptions:  
--the background seismicity follows he Poisson process;  
--the number of aftershocks are exponentially proportional to the magnitude M;  
--decrease of aftershocks follows modified Omori law (the power law).  

* Prof. Stark's slides give the function of conditional intensity at time t given the history up to t.  
--（a complex function could be found on slide 21)  
--Turn on alarms when the conditional intensity is above some threshold.  

* By fitting to the data, the ETAS model determines the parameters by the maximum likelihood method. Thus we can predict the incidence of the earthquake, conforming to regional diversity.  
--you may find this on <http://www.ism.ac.jp/~ogata/12ISM_NL_WEBver/earthquakeForecast_E.html>  
--so that means, ETAS returns the MLE of the parameters of the model (mu-per day, K, c-days, alpha, p); in addition to these, the R ETAS function also returns D, q, and gamma.  

* R ETAS returns:  
--param: the MLE of the model parameters  
--bk: estimates of u(x,y)(definition can be found in the lib of etas package),the bckground intensity  
--pb: probabilities of being background event, i.e., the mainshock  
--opt: results pf optimization-the value of the log-likelihood function at the optimum point, its gradient at optimum point, and AIC(a measure of the relative quality of a statistical model, for a given set of data; provides a means for model selection) of the model  
--rates: pixel images of the estimated total intensity, background intensity, clustering intensity, and conditional intensity  

* There are also programs called ETAS, ETASIM(for simulation with data) and RETAS, etc, other than R.  
-- <http://www.ism.ac.jp/editsec/csm/index/csm_033.pdf>  
