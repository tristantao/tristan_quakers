###############################################################################
#test on MDA

#import dataset '250' as 't250'
#The dataset used here is from the 250.csv that the curator's made. 
#Code here is adapted from Luen's thesis

cleandata=t250

dates <-cleandata[["YYYY.MM.DD"]]
times <-cleandata[["HH.mm.SS.ss"]]

#convert time format:
datatime <-as.POSIXct(strptime(paste(dates, times), "%Y/%m/%d %H:%M:%OS"))

    #find out the earliest date and set a start time: here I set 1999-01-01 as the first day
newtime <-as.numeric(difftime(datatime,"1999-01-01 00:00:00 PST",units="days"))

mag=t250$MAG
dat=cbind(newtime,mag)
newdat=dat[order(newtime),]

start = ISOdate(1999,1,1,0,0,0)
finish = ISOdate(2000,1,1,0,0,0)

training.period = as.numeric(finish - start)

times = newdat[,1]
mags = newdat[,2]


# Placeholders

#timelist = seq(training.period,period,0.1)
timelist=2:training.period

n.training = sum(times<training.period)-1

w2.dist=rep(NA,length(timelist))
w2.list=rep(NA,n.training)

w115.dist=rep(NA,length(timelist))
w115.list=rep(NA,n.training)

w58.dist=rep(NA,length(timelist))
w58.list=rep(NA,n.training)

n.events = sapply(timelist,function(x){sum(times<x)})

w.dist = timelist-times[n.events]

for(KK in 1:length(timelist)){
  w2.dist[KK]=min((timelist[KK]-times[1:n.events[KK]])/(4.7^mags[1:n.events[KK]]))}

for(KK in 1:length(timelist)){
  w58.dist[KK]=min((timelist[KK]-times[1:n.events[KK]])/(5.8^mags[1:n.events[KK]]))}

for(KK in 1:length(timelist)){
  w115.dist[KK]=(timelist[KK]-times[n.events[KK]])/(6^mags[n.events[KK]])}

# Find values at event times

w.list=diff(times[1:n.training])

for(KK in 1:length(w2.list)){
  w2.list[KK]=min((times[1+KK]-times[1:(KK)])/(4.7^mags[1:(KK)]))}

for(KK in 1:length(w58.list)){
  w58.list[KK]=min((times[1+KK]-times[1:(KK)])/(5.8^mags[1:(KK)]))}

for(KK in 1:length(w115.list)){
  w115.list[KK]=min((times[1+KK]-times[1:(KK)])/(6^mags[(KK)]))}

# Error diagrams

nu.w2=sapply(w2.dist,function(x){mean(w2.list>x)})
nu.w2=sort(nu.w2,decreasing=T)

nu.w58=sapply(w58.dist,function(x){mean(w58.list>x)})
nu.w58=sort(nu.w58,decreasing=T)

nu.w115=sapply(w115.dist,function(x){mean(w115.list>x)})
nu.w115=sort(nu.w115,decreasing=T)

nu.auto=sapply(w.dist,function(x){mean(w.list>x)})
nu.auto=sort(nu.auto,decreasing=T)

xx=seq(1,364,1)
yy=length(xx)

plot(seq(0,1,length.out=yy),nu.auto[xx],type="l",xlab=expression(tau),ylab=expression(nu),main="Error Diagrams of MDA's",ylim=c(0,1),col="green")
lines(seq(0,1,length.out=yy),nu.w2[xx],col="blue")
lines(seq(0,1,length.out=yy),nu.w58[xx],col="red")
lines(seq(0,1,length.out=yy),nu.w115[xx],col="black")
legend(.6,1,c("Auto-1^M","MDA 4.7^M","MDA 5.8^M","MDA 6^M"),lty=c(1,1,1,1),col=c("green","blue","red","black"))

#finding area under the curve in error diagram:
library(sfsmisc)
integrate.xy(x=seq(0,1,length.out=yy), nu.auto[xx])
integrate.xy(x=seq(0,1,length.out=yy), nu.w2[xx])
integrate.xy(x=seq(0,1,length.out=yy), nu.w58[xx])
integrate.xy(x=seq(0,1,length.out=yy), nu.w115[xx])


#codes for tuning parameters:
TimeRangelen=length(timelist)
n.training = sum(times<training.period)-1
paramlist=seq(2,15,0.2)
resD=matrix(NA,nrow=TimeRangelen,ncol=length(paramlist))
resL=matrix(NA,nrow=n.training,ncol=length(paramlist))
nu=matrix(0,nrow=TimeRangelen,ncol=length(paramlist))
errorInt=matrix(0,nrow=length(paramlist),ncol=1)
  for (i in 1:length(paramlist)){
      for(KK in 1:nrow(resL)){
          resL[KK,i]=min((times[1+KK]-times[1:(KK)])/(paramlist[i]^mags[1:(KK)]))}
      for(KK in 1:nrow(resD)){
          resD[KK,i]=min((timelist[KK]-times[1:n.events[KK]])/(paramlist[i]^mags[1:n.events[KK]]))}
      nu[,i]=sapply(resD[,i],function(x){mean(resL[,i]>x)})
      nu[,i]=sort(nu[,i],decreasing=T)
      xx=seq(0,TimeRangelen,length.out=min(TimeRangelen,1000))
      errorInt[i,]=integrate.xy(x=seq(0,1,length.out=length(xx)-1), nu[xx,i])
  }
plot(seq(2,15,0.2),errorInt)

plot(seq(0,1,length.out=length(xx)-1),nu.auto[xx],type="l",xlab=expression(tau),ylab=expression(nu),main="Training set error diagrams",ylim=c(0,1),col="black")
for(i in 1:length(paramlist)){
  lines(seq(0,1,length.out=length(xx)-1),nu[xx,i],col=450+i)
}


