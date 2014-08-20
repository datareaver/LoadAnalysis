loadbyhour <- function(x,name = '',origin = '02/01/2013 00:00',warmup = 14) {
require(chron)
require(reshape2)
#x must be in POSIXlt format
#add in hourly intervals data with 0
x <- x[order(x[,1]),]
intervals <- seq(0,ceiling(as.numeric(x[nrow(x),1]-x[1,1])*24),1)
intervals <- intervals*3600 + as.POSIXlt(origin,tz = "","%m/%d/%Y %H:%M")

#sort times
xint <- data.frame(c(x[,1],x[,2],intervals),c(rep(1,nrow(x)),rep(-1,nrow(x)),
            rep(0,length(intervals))))
xintsort <- xint[order(xint[,1]),]

#Function for generating load at each arrival or departure event
suminc <- 1
inc <- 1
for(i in 2:nrow(xintsort))
{
  suminc <- xintsort[i,2] + suminc
  inc <- c(inc,suminc)
}

#Find state arrival and departure times in hours
statex <- as.numeric(xintsort[2:(nrow(xintsort)),1] - xintsort[1:(nrow(xintsort)-1),1])/3600

#Find departures
depart <- ifelse(xintsort[1:(nrow(xintsort) - 1),2] == -1,1,0)

#combine matrices
xtable <- data.frame( xintsort[1:(nrow(xintsort)-1),], inc[1:(length(inc)-1)],as.matrix(statex),as.matrix(depart),1 )
names(xtable) <- c('Timestamp','inc','Load','StateTime','Departure','Denominator')

#Develop table with time categories
timetable <- data.frame(xtable,years(xtable[,1]),months(xtable[,1]),weekdays(xtable[,1]),as.numeric(format(xtable[,1],"%H")))
names(timetable) <- c('Timestamp',names(timetable[,2:6]),'Year','Month','Weekday','Hour')

#Calculate Load x Time
finaltimetable <- data.frame(timetable,LoadxTime = timetable$Load*timetable$StateTime)
finaltimetable <- finaltimetable[finaltimetable$Timestamp <= intervals[length(intervals)] &
                                     finaltimetable$Timestamp >= x[1,1]+(24*60*60*warmup),]

#set up Load table for plotting
sumtable <- data.frame(ddply(finaltimetable,.(Weekday,Hour),summarize,Load = sum(LoadxTime)/sum(StateTime)))
sumtable$Weekday <- factor(sumtable$Weekday, levels= c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday"))
sumtable <- sumtable[order(sumtable$Weekday),]
sumplottable <- dcast(sumtable,Hour ~ Weekday,value.var = 'Load')

#plot load on graph by WD and HoD
tiff(paste('PatientLoadbyHour',ifelse(name != '','.',''),name,'.tiff',sep = ''),800,600)
par(mfrow = c(1,1))
plot(sumplottable[,2],type = 'l',col = 2,xlab = 'Hour of Day',ylab = 'Mean Patient Load',xlim = c(1,24),ylim = c(0,max(sumplottable)),axes = F,lwd = 3)
for (i in 3:8) {
  lines(sumplottable[,i],col = i,lwd = 3)
}
axis(1,0:23,at = 1:24)
axis(2,seq(0,max(sumplottable),5))
grid()
legend("bottomright",names(sumplottable[,2:8]),col = 2:8,lwd = 3)
dev.off()
write.csv(sumplottable,paste('LoadTable',ifelse(name != '','.',''),name,'.csv',sep = ''))
}