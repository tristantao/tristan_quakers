
library(sfsmisc)

################################################
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
  if (any(is.na(time)) | any(is.na(window_size))) stop('time and window_size shoud not contain NAs')
  
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


#######################################################################


mags=DataFrame[DataFrame$magnitude > 3,]$magnitude
times=DataFrame[DataFrame$magnitude > 3,]$time
maglist=seq(3.1,max(mags)+0.1,0.1)


############################################################

#Plot error diagram of Luen's model with our algorithm!

Ks=c(0.98 ^ (1:1000),seq(1,5,0.1))
VTau=matrix(0, nrow=length(Ks),ncol=2)
for (i in 1:length(Ks)) {
  print(i)
  window_size=Ks[i]*(5.8^mags)
  window_size[is.na(window_size)]=0
  VTau[i,1]=get_error_rate(times,window_size)$v
  VTau[i,2]=get_error_rate(times,window_size)$tau
}

area=integrate.xy(sort(VTau[,2]),VTau[order(VTau[,2]),1])
jpeg(file="ErrorLuen.jpeg",1200,800)
plot(sort(VTau[,2]),VTau[order(VTau[,2]),1],type="l",main="Error Diagram(Luen's Model)",xlab=expression(tau),ylab=expression(nu))
legend(.5,0.9,c(paste("Area under the curve=",area)),cex=2)
dev.off()


##############################################################################

#Plot error diagram for the Quantile-Method (only consider 1 successor to the each eartquake)

ns=1
percentile=0.9

quan=matrix(0,nrow=length(maglist),ncol=1)
quan[1]=quantile(times[which(mags==maglist[1])+ns]-times[which(mags==maglist[1] )],probs=percentile[1], na.rm=TRUE)
for (i in 2:length(quan)){
  quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+ns]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=percentile[i],na.rm=TRUE)
}
quan[is.na(quan)]=0

Ks=c(0.98 ^ (1:1000),seq(1,5,0.1))
VTau=matrix(0, nrow=length(Ks),ncol=2)
for (i in 1:length(Ks)) {
  print(i)
  window_size=Ks[i]*Qfun(mags,quan)
  window_size[is.na(window_size)]=0
  VTau[i,1]=get_error_rate(time,window_size)$v
  VTau[i,2]=get_error_rate(time,window_size)$tau
}

area=integrate.xy(sort(VTau[,2]),VTau[order(VTau[,2]),1])
jpeg(file="ErrorDiffQuant.jpeg",1200,800)
plot(sort(VTau[,2]),VTau[order(VTau[,2]),1],type="l",main=paste("Error Diagram (ns=",ns,",varying percentiles",sep=""),xlab=expression(tau),ylab=expression(nu))
legend(.5,0.9,c(paste("Area under the curve=",area)),cex=2)
dev.off()

##############################################################################

#Tuning!

result=matrix(0,nrow=0,ncol=3)
colnames(result)=c("N.S.","Percentile","Area")

ns=c(1,2,3,4,5)
percentile=c(seq(0.7,1,0.05))

for (k in 1:length(ns)){
  for(j in 1:length(percentile)){
    quan=matrix(0,nrow=length(maglist),ncol=1)
    quan[1]=quantile(times[which(mags==maglist[1])+ns[k]]-times[which(mags==maglist[1] )],probs=percentile[j], na.rm=TRUE)
    for (i in 2:length(quan)){
      quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+ns[k]]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=percentile[j],na.rm=TRUE)
    }
    quan[is.na(quan)]=0
    Ks=c(0.98 ^ (1:1000),seq(1,5,0.1))
    VTau=matrix(0, nrow=length(Ks),ncol=2)
    for (i in 1:length(Ks)) {
      print(c(k,j,i))
      window_size=Ks[i]*Qfun(mags,quan)
      window_size[is.na(window_size)]=0
      VTau[i,1]=get_error_rate(time,window_size)$v
      VTau[i,2]=get_error_rate(time,window_size)$tau
    }
    area=integrate.xy(sort(VTau[,2]),VTau[order(VTau[,2]),1])
    result=rbind(result,matrix(c(ns[k],percentile[j],area),ncol=3))
  }
}

write.csv(result,"result.csv")



#####################################################################

#Pick smaller percentiles for smaller magnitudes, larger percentiles for greater magnitude: 

result1=matrix(0,nrow=0,ncol=2)
colnames(result1)=c("N.S.","Area")

ns=c(1,2,3,4,5)
percentile=c(seq(0.7,0.99, length.out=length(maglist)))

for (k in 1:length(ns)){
  quan=matrix(0,nrow=length(maglist),ncol=1)
  quan[1]=quantile(times[which(mags==maglist[1])+ns[k]]-times[which(mags==maglist[1] )],probs=percentile[1], na.rm=TRUE)
  for (i in 2:length(quan)){
    quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+ns[k]]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=percentile[i],na.rm=TRUE)
  }
  quan[is.na(quan)]=0
  
  Ks=c(0.98 ^ (1:1000),seq(1,5,0.1))
  VTau=matrix(0, nrow=length(Ks),ncol=2)
  for (i in 1:length(Ks)) {
    print(c(k,i))
    window_size=Ks[i]*Qfun(mag,quan)
    window_size[is.na(window_size)]=0
    VTau[i,1]=get_error_rate(time,window_size)$v
    VTau[i,2]=get_error_rate(time,window_size)$tau
  }
  area=integrate.xy(sort(VTau[,2]),VTau[order(VTau[,2]),1])
  result1=rbind(result1,matrix(c(ns[k],area),ncol=2))
  
}
# N.S  Area
#  1	0.2804136
#  2	0.2811069
#  3	0.2808707
#  4	0.2807918
#  5	0.2807587
write.csv(result1,"result1.csv")

