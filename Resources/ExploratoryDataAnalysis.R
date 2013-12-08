DataFrame<-read.csv("Insert File Pathway Here")
head(DataFrame)

startyear<-1990  ### Change starting year here
DataFrame = DataFrame[DataFrame$year >=startyear,]
head(DataFrame)
head(DataFrame$magnitude,100)


Mag3plus<-DataFrame[which(DataFrame$magnitude>=3),]
Mag3plus$inter<-rep(0,(length(Mag3plus$time)))
for(i in 1:nrow(Mag3plus)){
  Mag3plus$inter[i]<-Mag3plus$time[i+1]-Mag3plus$time[i]
}
head(Mag3plus)
plot(Mag3plus$magnitude,Mag3plus$inter)
