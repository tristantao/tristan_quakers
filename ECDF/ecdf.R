

setwd(
  #  '/home/sunnymh/stat157/final_project'
  # Please enter the directory where this file and data is saved
  '/home/t-rex/Desktop/stat157/TheQuakers/ECDF'
  )

t250 <- read.csv("./250.csv")

cleandata=t250

# code for creating the time disired
dates <-cleandata[["YYYY.MM.DD"]]
times <-cleandata[["HH.mm.SS.ss"]]
datatime <-as.POSIXct(strptime(paste(dates, times), "%Y/%m/%d %H:%M:%OS"))
newtime <-as.numeric(difftime(datatime,datatime[1],units="days"))
cleandata = data.frame(cleandata, 'newtime' = newtime)

# plot the ecdf of earthquacks for each bin
# change this part later for different bins
min = seq(0.5, 3, by = 0.5)
max = seq(1, 3.5, by = 0.5)

# group the newtimes base on MAG
groups = mapply(function(min, max){
  newtime[which(t250$MAG> min & t250$MAG < max)]
},min, max)

# get the plots
png("./plot.png", 600, 1200)
par(mfrow = c(6,1))
for (i in 1:length(groups)){
  if (length(groups[[i]] != 0)){
    test = groups[[i]]
    Fn <- ecdf(test)
    if (i ==1){
      par(mai = c(0, 0.6, 0.3, 0.3))      
      plot(test,Fn(test), col = 'red', xlim = c(-100, 300), pch = 19, cex = 0.5, xaxt = 'n', yaxt="n", main = "ECDF for Earthquakes", ylab = "Frequency")
      axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2))
    }else if (i==6){
      par(mai = c(0.3, 0.6, 0, 0.3))
      plot(test,Fn(test), col = 'red', xlim = c(-100, 300), pch = 19, cex = 0.5, yaxt="n", ylab = "Frequency")
      axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2))
    }else{
      par(mai = c(0, 0.6, 0, 0.3))
      plot(test,Fn(test), col = 'red', xlim = c(-100, 300), pch = 19, cex = 0.5, xaxt = 'n', yaxt="n", ylab = "Frequency")
      axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2))
    }

    for (j in seq(-100, 300, by= 50)){
      abline(v = j, lty =2, col = 'grey')
    }
    text(-75, 0.9, label = paste('MAG:', min[i], '~', max[i]))
  }
}
dev.off()
