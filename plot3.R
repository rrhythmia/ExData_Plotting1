# plot3.R

# -------------------------------------------------------
# ALL BUT PLOT1 WERE SUBMITTED LATE. 
# PROBABLY NO POINTS FOR LATE ONES.
# -------------------------------------------------------

rm(list=ls())
dev.off()  # ensure pars get set to default values
cat('\014')

sname <- "plot3"
fnameGraph <- paste(sname, ".png",sep="")

require(data.table)
require(dplyr)

# Get unzipped file
szippedURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"
fnamez <- "fzipped.csv"
fname <- "household_power_consumption.txt"

if (file.exists(fname) & file.exists(fnamez) ) {
  print(paste(fnamez,"&", fname,"already exist"))
} else {
  print(paste("Downloading to",fnamez,"from",szippedURL))
  ret <- download.file(szippedURL,fnamez)
  print("Unzip")
  ret <- unzip(fnamez,fname)  
}

# Try to read only what we need.

# Bound our rows of interest, by trial&error, since I don't
#   know how to seek just the first instance of the desired date
#  (do you?)
# (the DB starts about Dec 16, 2006) 
# Need: all of Feb 1 and 2, 2007
minutesperday <- 60*24  #  1440
minutesperweek <- 60*24*7  # 10080
rfirstish <- 6*minutesperweek + 4*minutesperday

# colnames,classes
dhead <- read.table(fname,nrows=5,sep=";",
                    na.strings="?",
                    header=TRUE)
dclasses <- sapply(dhead, class)
dnames <- colnames(dhead)

dtish <- read.table(fname, sep=";", na.strings="?",
                    skip=rfirstish-1,
                    nrows=3*minutesperday,
                    col.names=dnames, 
                    colClasses = dclasses)
# verify we have (part of) 1 day on either side of the recs for desired dates
# (ie, 4 levels, with our desired ones as 2 & 3)
tmp <- levels(dtish[,1]) 
stopifnot(length(tmp)==4) # (this is a half-vast check.)

# filter for just recs for the days we want
dt0 <- filter(dtish,Date=="1/2/2007" | Date=="2/2/2007" )

# transform date and time into posixct
# as.POSIXct(strptime("31/1/2007 01:30:00", "%d/%m/%Y %H:%M:%S"))
dt0mut <- mutate(dt0,cposix=as.POSIXct(
  strptime(paste(Date,Time), 
           format="%d/%m/%Y %H:%M:%S")
))
df <- select(dt0mut,c(10,3:9)) # hack to strip out old&interim datetime cols

# I hacked the y range instead of using this. Not sure how to do it cleanly.
ymax <- max(c( max(df$Sub_metering_1), 
               max(df$Sub_metering_2), 
               max(df$Sub_metering_3)))

par(lty="solid",col="black",pch=1,cex=0.8)

# fix left margin.
nmar <- par("mar")
nmar[2] <- 2*nmar[2]
par(mar=nmar)

plot(df$cposix,
     df$Sub_metering_1, # sets the x and y axes scales
         main=" *** Submitted AFTERRRRR the deadline *** ",
         xlab="", 
         ylab="Energy sub metering", # adds titles to the axes
         type="n" # don't, yet.
) 

lines(df$cposix,df$Sub_metering_1,col="black",lwd=2.5)
lines(df$cposix,df$Sub_metering_2,col="red",  lwd=2.5)
lines(df$cposix,df$Sub_metering_3,col="blue", lwd=2.5) 

legend("topright",100, # places a legend at the appropriate place 
      c("Sub_metering_1","Sub_metering_2","Sub_metering_3"), 
      lty=c(1,1,1), # gives the legend appropriate symbols (lines)
      lwd=c(2.5,2.5,2.5),
      col= c("black","red","blue") # legend lines
)
    
## Copy the plot to a file
dev.copy(png, file = fnameGraph)
print("close the PNG device")
dev.off()

