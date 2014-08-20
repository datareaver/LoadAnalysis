loadtoALOS <- function(x,name = '',origin = '02/01/2013 00:00') {
library(chron)
library(plyr)
library(reshape2)
  
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

#combine matrices
xtable <- data.frame( xsort[1:(nrow(xsort)-1),], inc[1:(length(inc)-1)],as.matrix(statex),as.matrix(depart),1 )
names(xtable) <- c('Timestamp','inc','Load','StateTime','Departure','Denominator')

#build summary tables for mean time, sum of depart, sum of total


#build time in each state table and plot CDF of time in each state
statetime <- data.frame(d[,1],d[,2]/sum(d[,2]))
stcdf <- c(0)
temp <- statetime[1,2]
for (i in 2:nrow(statetime))
{ 
    stcdf <- c(stcdf,stcdf[i-1]+temp)
    temp <- statetime[i,2]
}
stcdf <- c(stcdf[2:length(stcdf)],1)
stcdf
statetime <- data.frame(statetime,stcdf)
names(statetime) <- c('Number in System','PDF','CDF')

#calculate the ALOS from the summary tables
ALOS <- cbind(abc[,1],(abc[,1]*abc[,2]*abc[,4])/(abc[,3]),abc[,4])
row.names(ALOS) <- NULL

#remove data points with N < 100, plot, and label
plotx <- ALOS[ALOS[,3] > 100,]
plot(plotx[,1],plotx[,2],type = "l",xlab = "Number in System",ylab = "ALOS")
#text(plotx[,1],plotx[,2],plotx[,3])
return(summary(lm(plotx[,2]~plotx[,1])))

#write output
write.csv(ALOS,file="ALOS.csv")

tiff(paste(paste('LoadbyALOS',ifelse(name != '','.',''),name,'.tiff',sep = ''),800,600)
plot(plotx[,1],plotx[,2],type = "l",xlab = "Number in System",ylab = "ALOS")
text(plotx[,1],plotx[,2],plotx[,3])
dev.off()

jpeg('CDF_timestates.jpeg',800,600)
plot(statetime[,1],statetime[,3],type = "o",col="blue",xlab='Number in System',ylab ='F(n)',main = 'Percentage of Cumulative Time in Each State',lwd = 3,axes = F,ylim = c(0,1))
axis(1,0:length(stcdf))
axis(2,seq(0,1,.05))
grid()
dev.off()
}