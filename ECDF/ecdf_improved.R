args = commandArgs(trailingOnly = TRUE)
data_path = args[1]
print (data_path)
setwd(
  #  '/home/sunnymh/stat157/final_project'
  # Please enter the directory where this file and data is saved
  data_path
  )

# Revise the path if necessary
#setwd("~/STAT157/Final")

# Load data
t250 <- read.csv("./250.csv")
tnew <- read.csv("./1932_2013_mag_3.5.csv")

# Define a function for plotting inspired by Sunny's code
plot_ecdf=function (cleandata) {
      # code for creating the time disired
        dates <-cleandata[["YYYY.MM.DD"]]
          times <-cleandata[["HH.mm.SS.ss"]]
            datatime <-as.POSIXct(strptime(paste(dates, times), "%Y/%m/%d %H:%M:%OS"))
              newtime <-as.numeric(difftime(datatime,datatime[which(min(as.numeric(datatime))==as.numeric(datatime))],
                                              units="days"))
                cleandata = data.frame(cleandata, 'newtime' = newtime)

                  # New Code added by Qi to generate adaptive ECDF plot
                    max_mag=ceiling(max(cleandata$MAG))
                      # no more than 6 rows
                        tgt_ysteps=6
                          # Calculate size of interval, 
                            ystep_size=ceiling((max_mag/(tgt_ysteps+1)*10))/10         # Align to 0.1
                              # Generate lower boundary and upper boundary of each interval
                                min=seq(0,max_mag-ystep_size-0.05, by=ystep_size)
                                  max=seq(ystep_size,max_mag-0.05, by=ystep_size)
                                    
                                      # No more than 8 columns
                                        tgt_xsteps=8
                                          # Calculate size of interval
                                            xstep_size=(floor(((max(newtime))/tgt_xsteps+49)/50))*50     # Align to 50 days
                                              # Calculate location of these ticks and title of these ticks
                                                xaxt_pos=seq(0,max(newtime)+xstep_size-1,by=xstep_size)
                                                  xaxt_date=strsplit(as.character(as.POSIXct(xaxt_pos*3600*24, unit="days", 
                                                                       origin=datatime[which(min(as.numeric(datatime))==as.numeric(datatime))])),
                                                                                            " ")
                                                    xaxt_title=sapply(xaxt_date, function(x) {x[1]})
                                                     
                                                       # group the newtimes base on MAG
                                                         groups = mapply(function(min, max){
                                                                 newtime[which(cleandata$MAG> min & cleandata$MAG < max)]
                                                                   },min, max)
                                                           
                                                             # Count how many groups will be plotted
                                                               groups_nr=0
                                                                 for ( i in 1:length(groups)) {
                                                                         if(length(groups[[i]])) {
                                                                                   groups_nr=groups_nr+1
                                                                                       }
                                                                                         }

                                                                                           ############## get the plots #################
                                                                                             # Divide the working zone, add 1 for title and 1 for horizontal axis
                                                                                               par(mfrow = c(groups_nr+2,1))
                                                                                                 # Generate the title
                                                                                                   par(mai = c(0, 1, 0, 0.3))
                                                                                                     par(col="white")
                                                                                                       plot(0,0,main="",col="white",xlab="",ylab="",xaxt="n",yaxt="n")
                                                                                                         par(col="black")
                                                                                                           text(0,0,"ECDF For EarthQuakes",col="black",cex=3)
                                                                                                             # Generate the major part
                                                                                                               for (i in 1:length(groups)){
                                                                                                                       if (length(groups[[i]] != 0)){
                                                                                                                                 test = groups[[i]]
                                                                                                                                       Fn <- ecdf(test)
                                                                                                                                             if (i ==1){
                                                                                                                                                         par(mai = c(0, 1, 0, 0.3))
                                                                                                                                                                 plot(test,Fn(test), col = 'red', xlim = c(0, max(newtime)), ylim=c(0,1),
                                                                                                                                                                              pch = 19, cex = 0.5, xaxt = 'n', yaxt="n", ylab = "Frequency", cex.lab=2)
                                                                                                                                                                         axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2),cex.axis=2)
                                                                                                                                                                               }else if (i==length(groups)){
                                                                                                                                                                                           par(mai = c(0, 1, 0, 0.3))
                                                                                                                                                                                                   plot(test,Fn(test), col = 'red', xlim = c(0, max(newtime)), ylim=c(0,1),
                                                                                                                                                                                                                pch = 19, cex = 0.5, xaxt="n", yaxt="n", ylab = "Frequency", cex.lab=2)
                                                                                                                                                                                                           axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2),cex.axis=2)
                                                                                                                                                                                                                 }else{
                                                                                                                                                                                                                             par(mai = c(0, 1, 0, 0.3))
                                                                                                                                                                                                                                     plot(test,Fn(test), col = 'red', xlim = c(0, max(newtime)), ylim=c(0,1),
                                                                                                                                                                                                                                                  pch = 19, cex = 0.5, xaxt = 'n', yaxt="n", ylab = "Frequency", cex.lab=2)
                                                                                                                                                                                                                                             axis(2, at=seq(0, 0.8, by = 0.2), seq(0, 0.8, by = 0.2),cex.axis=2)
                                                                                                                                                                                                                                                   }
                                                                                                                                                                                                                                                       
                                                                                                                                                                                                                                                             for (j in xaxt_pos ){
                                                                                                                                                                                                                                                                         abline(v = j, lty =2, col = 'grey')
                                                                                                                                                                                                                                                                               }
                                                                                                                                                                                                                                                                                     for (j in seq(0,0.8,by=0.2)) {
                                                                                                                                                                                                                                                                                                 abline(h = j, lty =2, col = 'grey')
                                                                                                                                                                                                                                                                                                       }
                                                                                                                                                                                                                                                                                                             text(max(newtime)/10, 0.9, label = paste('MAG:', min[i], '~', max[i]),cex=2)
                                                                                                                                                                                                                                                                                                                 }
                                                                                                                                                                                                                                                                                                                   }
                                                                                                                                                                                                                                                                                                                     
                                                                                                                                                                                                                                                                                                                       ########### Add labels to x-axis ###########
                                                                                                                                                                                                                                                                                                                         par(mai = c(1.5, 1, 0, 0.3))
                                                                                                                                                                                                                                                                                                                           # Enforce new plot and fine tune zoom factor of the new plot with an invisible plot
                                                                                                                                                                                                                                                                                                                             par(col="white")
                                                                                                                                                                                                                                                                                                                               plot(x=c(0,1),y=c(0,0),xlim=c(0,max(newtime)),ylim=c(0,0), xaxt="n",yaxt="n",ylab="",xlab="")
                                                                                                                                                                                                                                                                                                                                 par(col="black")
                                                                                                                                                                                                                                                                                                                                   # Plot vertical line for alignment
                                                                                                                                                                                                                                                                                                                                     for (j in xaxt_pos ){
                                                                                                                                                                                                                                                                                                                                             abline(v = j, lty =2, col = 'grey')
                                                                                                                                                                                                                                                                                                                                               }
                                                                                                                                                                                                                                                                                                                                                 # Plot Horizontal Axis with Date Mark
                                                                                                                                                                                                                                                                                                                                                   axis(1, at=xaxt_pos, xaxt_title, las=2, cex.axis=2)
}

# Call the function to generate plot based on data loaded
png("./ecdfplot_250.png", 600, 1200)
plot_ecdf(t250)
dev.off()
png("ecdfplot_new.png",600,1200)
plot_ecdf(tnew)
dev.off()
