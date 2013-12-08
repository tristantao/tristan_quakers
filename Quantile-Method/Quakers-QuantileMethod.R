#first, need to install "sfsmisc" package, can run this: 
#install.packages("sfsmisc")

library(sfsmisc)

## Quantile-Method #################################

Qfun=function(mag){
  index=ceiling((mag-3)/0.1)+1
  win=quan[index]
  return(win)
}

mags=DataFrame[DataFrame$magnitude > 3,]$magnitude
times=DataFrame[DataFrame$magnitude > 3,]$time
maglist=seq(3.1,max(mags)+0.1,0.1)

start = ISOdate(1938,1,1,0,0,0)
test.start = ISOdate(1989,1,1,0,0,0)
finish = ISOdate(2013,1,1,0,0,0)

training.period = as.numeric(test.start - start)
test.period = as.numeric(finish - test.start)
period = as.numeric(finish - start)
timelist=2:training.period

n.training = sum(times<training.period)
n.test = sum(times<period) - n.training
n.events = sapply(timelist,function(x){sum(times<x)})


#everything up to this point will be consistent for all pocess of testing models or parameters.

#Using this data set, Luen's model gives area under the curve=0.3106224
#######################################################

#First try with 90-percentile as the cutoff for window size:
quan=matrix(0,nrow=length(maglist),ncol=1)
quan[1]=quantile(times[which(mags==maglist[1])+1]-times[which(mags==maglist[1] )],probs=0.9)
for (i in 2:length(quan)){
  quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+1]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=0.9,na.rm=TRUE)
}

jpeg(file="90Quantile.jpeg",1200,800)
plot(maglist,quan,xlab="Magnitude",ylab="Window Steps")
dev.off()

wECDF.dist=rep(NA,length(timelist))
wECDF.list=rep(NA,n.training)

for(KK in 1:length(timelist)){
  wECDF.dist[KK]=min((timelist[KK]-times[1:n.events[KK]])/(Qfun(mags[1:n.events[KK]])),na.rm=TRUE)}
for(KK in 1:length(wECDF.list)){
  wECDF.list[KK]=min((times[1+KK]-times[1:(KK)])/(Qfun(mags[1:(KK)])),na.rm=TRUE)}
nu.wECDF=sapply(wECDF.dist,function(x){mean(wECDF.list>x)})
nu.wE=sort(nu.wECDF,decreasing=TRUE)

jpeg(file="Error1.jpeg",1200,800)
xx=1:length(nu.wECDF)
plot(seq(0,1,length.out=length(xx)),nu.wE[xx],type="l",xlab=expression(tau),ylab=expression(nu),main="Training set error diagrams",ylim=c(0,1),col="green",lty=2)
dev.off()

integrate.xy(x=seq(0,1,length.out=length(xx)), nu.wE[xx])

###########################################
#Tuning quantile cutoffs:
#Adopt training period, magnitudes, times, etc. from above.

quanlist=c(seq(0.8,1,by=0.01))  ##probabilities to input to the quantile function
ns=1               ##number of successors you want to consider; ex:1,2,3,4,5,..
#here are the only 2 places where things change.

TuningE.nu=matrix(0,nrow=length(timelist),ncol=length(quanlist))
for (j in 1:length(quanlist)){
  print(j)
  quan=matrix(0,nrow=length(maglist),ncol=1)
  quan[1]=quantile(times[which(mags==maglist[1])+ns]-times[which(mags==maglist[1] )],probs=quanlist[j])
  for (i in 2:length(quan)){
    quan[i]=quantile(times[which(mags>maglist[i] & mags<=maglist[i+1])+ns]-times[which(mags>maglist[i] & mags<=maglist[i+1])],probs=quanlist[j],na.rm=TRUE)
  }
  
  wECDF.dist=rep(NA,length(timelist))
  wECDF.list=rep(NA,n.training)
  
  for(KK in 1:length(timelist)){
    wECDF.dist[KK]=min((timelist[KK]-times[1:n.events[KK]])/(Qfun(mags[1:n.events[KK]])),na.rm=TRUE)}
  for(KK in 1:length(wECDF.list)){
    wECDF.list[KK]=min((times[1+KK]-times[1:(KK)])/(Qfun(mags[1:(KK)])),na.rm=TRUE)}
  nu.wECDF=sapply(wECDF.dist,function(x){mean(wECDF.list>x)})
  TuningE.nu[,j]=sort(nu.wECDF,decreasing=TRUE)
}

#You may want to change the name of the plot whenever you test with a new set of parameters
jpeg(file=paste("TuningError(ns=",ns,").jpeg",sep=""),1200,800)
colors=rainbow(length(quanlist))
xx=1:nrow(TuningE.nu)
plot(seq(0,1,length.out=length(xx)),TuningE.nu[,1],type="l",xlab=expression(tau),ylab=expression(nu),main=paste("Training set error diagrams(",ns,"successors)",sep=""),ylim=c(0,1),col=colors[1],lty=1)
for (i in 2:ncol(TuningE.nu)){
  lines(seq(0,1,length.out=length(xx)),TuningE.nu[,i],col=colors[i],lty=i)
}
legend(.75,1,c(paste(quanlist*100,"Percentile")),lty=c(1:length(quanlist)),cex=2,col=colors)
dev.off()

area=rep(0,length(quanlist))
xx=1:nrow(TuningE.nu)
for (j in 1:length(quanlist)){
  area[j]=integrate.xy(x=seq(0,1,length.out=length(xx)), TuningE.nu[xx,j])
}

jpeg(file=paste("AreaCom(ns=",ns,").jpeg",sep=""),1200,800)
plot(quanlist,area,pch=19,col=rainbow(length(quanlist)),main=paste("Area under Error Curve(",ns,"successors)"))
dev.off()

#It took us 6 min to test for 21 parameters

#which parameter does the best?
min(area)
quanlist[which(area==min(area))] 

#Records:
#0.96 gives the minimum(0.3157354) for 1 successor
#0.96 gives the minimum(0.3156379) for 2 successors
#0.97 gives the minimum(0.3155601) for 3 successors
#0.96 gives the minimum(0.3155305) for 4 successors
#0.92 gives the minimum(0.3155316) for 5 successors

