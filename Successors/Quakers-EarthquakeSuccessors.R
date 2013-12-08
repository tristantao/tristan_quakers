install.packages("scales")
library(scales)


#Download DataFrame from https://www.dropbox.com/s/tzx4qqxhh9u9iz2/DataFrame.csv
#DataFrame = read.csv(file="DataFrame.csv", header =T)
mags=DataFrame$magnitude
times=DataFrame$time

jpeg(file="successorsALL.jpeg",1400,800)
cols=c("pink","deeppink",'red',"deeppink4","blue")
plot(mags,rep(0,length.out=length(mags)),pch=20,ylim=c(0,max(diff(times,lag=5))),xlim=c(0,8),ylab="Time",xlab="Magnitude")

for (i in 1:length(mags)){
  points(mags[i],times[i+1]-times[i],pch=20,col=alpha(cols[1],0.5))
  points(mags[i],times[i+2]-times[i],pch=20,col=alpha(cols[2],0.5))
  points(mags[i],times[i+3]-times[i],pch=20,col=alpha(cols[3],0.5))
  points(mags[i],times[i+4]-times[i],pch=20,col=alpha(cols[4],0.5))
  points(mags[i],times[i+5]-times[i],pch=20,col=alpha(cols[5],0.5))
}
legend(7.6,max(diff(times,lag=5)),c("1st","2nd","3rd","4th","5th"),pch=20,col=cols)
dev.off()

##mean

maglist=sort(unique(mags))

jpeg(file="successorsMean.jpeg",1400,800)

cols=c("pink","deeppink",'red',"deeppink4","blue")
plot(maglist,rep(0,length.out=length(maglist)),pch=20,ylim=c(0,10),xlim=c(0,8),ylab="Time",xlab="Magnitude")
for (i in 1:length(maglist)){
   points(maglist[i],mean(times[which(mags==maglist[i])+1]-times[which(mags==maglist[i])]),pch=19,col=cols[1])
   points(maglist[i],mean(times[which(mags==maglist[i])+2]-times[which(mags==maglist[i])]),pch=19,col=cols[2])
   points(maglist[i],mean(times[which(mags==maglist[i])+3]-times[which(mags==maglist[i])]),pch=19,col=cols[3])
   points(maglist[i],mean(times[which(mags==maglist[i])+4]-times[which(mags==maglist[i])]),pch=19,col=cols[4])
   points(maglist[i],mean(times[which(mags==maglist[i])+5]-times[which(mags==maglist[i])]),pch=19,col=cols[5])
}
legend(7.6,10,c("1st","2nd","3rd","4th","5th"),pch=19,col=cols)
dev.off()

###bin0.1

maglist=seq(min(mags),max(mags)+0.1,0.1)

jpeg(file="successorsBinp1.jpeg",1200,800)

cols=c("pink","deeppink",'red',"deeppink4","blue")
plot(maglist,rep(0,length.out=length(maglist)),pch=20,ylim=c(0,3),xlim=c(0,8),ylab="Time",xlab="Magnitude")
for (i in 1:length(maglist)){
  points(maglist[i],mean(times[which(mags>=maglist[i] & mags<=maglist[i+1])+1]-times[which(mags>=maglist[i] & mags<=maglist[i+1])]),pch=19,col=cols[1])
  points(maglist[i],mean(times[which(mags>=maglist[i] & mags<=maglist[i+1])+2]-times[which(mags>=maglist[i] & mags<=maglist[i+1])]),pch=19,col=cols[2])
  points(maglist[i],mean(times[which(mags>=maglist[i] & mags<=maglist[i+1])+3]-times[which(mags>=maglist[i] & mags<=maglist[i+1])]),pch=19,col=cols[3])
  points(maglist[i],mean(times[which(mags>=maglist[i] & mags<=maglist[i+1])+4]-times[which(mags>=maglist[i] & mags<=maglist[i+1])]),pch=19,col=cols[4])
  points(maglist[i],mean(times[which(mags>=maglist[i] & mags<=maglist[i+1])+5]-times[which(mags>=maglist[i] & mags<=maglist[i+1])]),pch=19,col=cols[5])
}
legend(7.6,3,c("1st","2nd","3rd","4th","5th"),pch=19,col=cols)
dev.off()

