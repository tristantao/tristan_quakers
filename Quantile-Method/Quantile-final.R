#install.packages("sfsmisc")
library(sfsmisc)
############################################################

print ("Begin RawData to RealTimeData conversoin")

args = commandArgs(trailingOnly = TRUE)
input_data_path = args[1]
print (input_data_path)

cleandata <-read.csv(input_data_path)

numbers <- c(1:nrow(cleandata))
long <-cleandata[["LON"]]
lat <-cleandata[["LAT"]]
magnitude <-cleandata[["MAG"]]
dates <-cleandata[["YYYY.MM.DD"]]
times <-cleandata[["HH.mm.SS.ss"]]
datatime <-as.POSIXct(strptime(paste(dates, times), "%Y/%m/%d %H:%M:%OS"))
newtime <-as.numeric(difftime(datatime,datatime[1],units="days"))

depth <- cleandata[["DEPTH"]]
year <- as.numeric(strftime(datatime, format="%Y"))
month <- as.numeric(strftime(datatime, format="%m"))
day <- as.numeric(strftime(datatime, format="%d"))

DataFrame = data.frame(no.= numbers,longitude=long,latitude=lat,magnitude=magnitude, time=newtime,depth=depth,year=year,month=month,day=day)


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


##############################################################
#Function of assigning windoe lengths to magnitudes based on the percentiles.
#Inputs:
#mag=magnitudes you want to get window lengths from
#quan=waiting times of earthquake with magnitude in each bin at a specific percentile
Qfun=function(mag,quan){
  index=ceiling((mag-3)/0.1)+1
  win=quan[index]
  return(win)
}
##############################################################################

mags=DataFrame[DataFrame$magnitude > 3,]$magnitude
times=DataFrame[DataFrame$magnitude > 3,]$time
maglist=seq(3.1,max(mags)+0.1,0.1)
LUEN=get_error_diagram(times,(5.8^ mags), k = 0.98 ^ (1:1000))

#########################################################
quan=matrix(0,nrow=length(maglist),ncol=1)
quan[1]=quantile(times[which(mags==maglist[1])+1]-times[which(mags==maglist[1] )],probs=0.9)
for (i in 2:length(quan)){
  quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+1]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=0.9,na.rm=TRUE)
}

jpeg(file="90Percentile.jpeg",1200,800)
plot(maglist,quan,cex=2,cex.main=1.5,cex.lab=1.5,cex.axis=1.2,xlab="Magnitude",ylab="Window Steps (in days)",main="90th Percentiles of Waiting Times",pch=19)
dev.off()

######################################################################

ns=5
percentile=c(seq(0.7,0.99, length.out=length(maglist)))

quan=matrix(0,nrow=length(maglist),ncol=1)
quan[1]=quantile(times[which(mags==maglist[1])+ns]-times[which(mags==maglist[1] )],probs=percentile[1], na.rm=TRUE)
for (i in 2:length(quan)){
  quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+ns]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=percentile[i],na.rm=TRUE)
}
quan[is.na(quan)]=0

Qres=get_error_diagram(times,Qfun(mags,quan), k = 0.98 ^ (1:1000))

area1=round(integrate.xy(Qres[[2]],Qres[[1]]),digits=4)
area2=round(integrate.xy(LUEN[[2]],LUEN[[1]]),digits=4)
jpeg(file="ErrorECDF.jpeg",1200,800)
plot(Qres[[2]],Qres[[1]],type="l",lwd=2,cex.axis=1.2,cex.main=1.5,cex.lab=2,main="Error Diagram of ECDF MDA",xlab=expression(tau),ylab=expression(nu))
lines(LUEN[[2]],LUEN[[1]],type="l",lwd=1,col="blue")
legend(.5,0.9,c(paste("ECDF MDA(area=",area1,")",sep=""),paste("Luen's MDA(area=",area2,")",sep="")),lty=c(1,1),col=c("black","blue"),cex=2,lwd=c(2,2))
dev.off()

