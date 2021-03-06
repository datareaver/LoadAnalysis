#add in warmup period


# Setup -------------------------------------------------------------------
library(reshape2)
library(dplyr)
library(lubridate)
library(ggplot2)

mean.load <- function(ts) {

  require(dplyr)
  require(lubridate)
  
  #create unit interval stoppers
  
  max.interval <- as.numeric(ceiling_date(max(ts[[1]]),"h")-
                               ceiling_date(min(ts[[1]]),"h"))
                             
  intervals <- data.frame(Timestamp = floor_date(min(ts[[1]]),"h") + 
                               seq(1,max.interval+1,1)*3600,Change = 0)
  
  arrivals <- data.frame(Timestamp = ts[[1]],Change = 1)
  
  departures <- data.frame(Timestamp = ts[[2]],Change = -1)
  
  
  
  dt <- rbind(arrivals,departures,intervals) %>% 
    arrange(Timestamp) %>%
    mutate(Hours.In.State = as.numeric(lead(Timestamp)-Timestamp)/60) %>%
    filter(!is.na(Hours.In.State))
  
  
  cumsum <- vector(mode = "integer",length = nrow(dt))
  cumsum[1] <- 1
  for(i in 2:nrow(dt))
  {
    cumsum[i] <- cumsum[i-1] + dt$Change[i]
  }
  
  results <-  data.frame(dt,cumsum) %>% 
    mutate(LoadxTime = cumsum*Hours.In.State,Month = month(Timestamp),
           Weekday = wday(Timestamp),Hour = hour(Timestamp)) %>% 
    group_by(Month,Weekday,Hour) %>% 
    summarize( Load = sum(LoadxTime)/sum(Hours.In.State))
  
  results
}

nurse.load <- function(ts) {
  
  require(dplyr)
  require(lubridate)
  
  #create unit interval stoppers

  max.interval <- as.numeric(ceiling_date(max(ts[[1]]),"h")-
                               ceiling_date(min(ts[[1]]),"h"))
  
  intervals <- data.frame(Timestamp = floor_date(min(ts[[1]]),"h") + 
                            seq(1,max.interval+1,1)*3600,Change = 0)
  
  arrivals <- data.frame(Timestamp = ts[[1]],Change = ts[[3]])
  
  departures <- data.frame(Timestamp = ts[[2]],Change = -ts[[3]])
    
  dt <- rbind(arrivals,departures,intervals) %>% 
    arrange(Timestamp) %>%
    mutate(Hours.In.State = as.numeric(lead(Timestamp)-Timestamp)/60) %>%
    filter(!is.na(Hours.In.State))
  
  
  cumsum <- vector(mode = "integer",length = nrow(dt))
  cumsum[1] <- dt[1,"Change"]
  for(i in 2:nrow(dt))
  {
    cumsum[i] <- cumsum[i-1] + dt$Change[i]
  }
  
  results <-  data.frame(dt,cumsum) %>% 
    mutate(LoadxTime = cumsum*Hours.In.State,Month = month(Timestamp),
           Weekday = wday(Timestamp),Hour = hour(Timestamp)) %>% 
    group_by(Month,Weekday,Hour) %>% 
    summarize( Load = sum(LoadxTime)/sum(Hours.In.State))
  
  results
}

nweekdays <- function(startdate,enddate,weekdayx) {
  require(lubridate)
  dates <- startdate + seq(0,enddate-startdate-1)
  sum(wday(dates) == weekdayx)
}

all.wdays <- function(x) {
  data.frame(Weekday = 1:7,
             N = sapply(1:7,nweekdays,startdate = as.Date(min(x$orderDate)),
                        enddate = as.Date(max(x$orderDate))))
}

# Patient Load ------------------------------------------------------------



load("arrivals.RData")
ESI <- c("ESI Level 1","ESI Level 2","ESI Level 3","ESI Level 4","ESI Level 5")

x <- x %>% filter(ESILevel %in% ESI,!is.na(EDArrivalDate),!is.na(PhysicalDepartDate)) 
dt <- split(x,x$ESILevel)
dt$All <- x

esi.load <- lapply(dt,function(x) select(x,EDArrivalDate,PhysicalDepartDate) %>% mean.load()) %>%
  melt(id.vars = c("Month","Weekday","Hour"),measure.vars = 'Load',
       value.name = "Load") %>% group_by(L1) %>% 
  mutate(Type = "Patients.In.System",Normalized.Load = scale(Load))

names(esi.load)[6] <- "Acuity"
rm(x)

# Patient Order Sets ------------------------------------------------------
load("orders.RData")

# 
# w.days <- data.frame(Weekday = 1:7,
#                      N = sapply(1:7,nweekdays,startdate = as.Date('2012-07-01'),
#                                 enddate = as.Date('2014-06-30')))

orders <- h %>% filter(Acuity %in% ESI) %>% 
  group_by(ibex, orderDate, Acuity) %>% summarize() %>%
  mutate(Year = year(orderDate),Month = month(orderDate),
         Weekday = wday(orderDate),Hour = hour(orderDate))

# periods <- split(orders,orders[['Year']])
# w.days <- melt(lapply(periods,all.wdays),id.vars = "Weekday",
#                measure.vars = "N",value.name = "N") %>%
#   group_by(Month,Weekday) %>% summarize(N = sum(N))
# 
# all.orders <- orders %>% group_by(Month,Weekday,Hour) %>%
#   summarize( orders = n() ) %>%
#   left_join(w.days,by="Weekday") %>%
#   mutate(Load = orders/N)

orders.split <- split(orders,orders$Month)

months.orders <- lapply(orders.split,function(x) {
  periods <- split(x,x[['Year']])
  w.days <- melt(lapply(periods,all.wdays),id.vars = "Weekday",
                 measure.vars = "N",value.name = "N") %>%
    group_by(Weekday) %>% summarize(N = sum(N))
  
  x %>% group_by(Acuity,Weekday,Hour) %>%
    summarize( orders = n() ) %>%
    left_join(w.days,by="Weekday") %>%
    mutate(Load = orders/N)
})

months.orders <- melt(months.orders,id.vars = c("Acuity","Weekday","Hour"),
             measure.vars = 'Load',value.name = 'Load')
names(months.orders)[6] <- "Month"

all.orders <- months.orders %>% 
  group_by(Weekday,Hour,variable,Month) %>% 
  summarize(Load = sum(Load)) %>%
  mutate(Acuity = 'All')

orders.load <- rbind(months.orders,all.orders) %>% group_by(Acuity) %>% 
  mutate(Type = "Orders",Normalized.Load = scale(Load))

rm(all.orders,months.orders,orders,h)

# Nurse Acuity ------------------------------------------------------------

# library(RODBC)
# ch <- odbcConnect("CEL_Prod")
# sqlTables(ch)
# n.acuity <- sqlFetch(ch,"EDPatientEMERGE")
# save(n.acuity,file = "nurse_acuity.RData")

# ACUITY
load("nurse_acuity.RData")
n.acuity$DATETIME_IN <- as.POSIXct(n.acuity$DATETIME_IN,format = "%m/%d/%y %I:%M %p")
n.acuity$DATETIME_OUT <- as.POSIXct(n.acuity$DATETIME_OUT,format = "%m/%d/%y %I:%M %p")
 n.acuity <- filter(n.acuity,!is.na(DATETIME_IN),!is.na(DATETIME_OUT)) %>% arrange(DATETIME_IN)
# n.acuity <- n.acuity[!duplicated(n.acuity$ENCOUNTER_ID,fromLast = TRUE),]


x <- n.acuity %>% select(Acuity = PATIENT_TYPE,DATETIME_IN,DATETIME_OUT,Change = ACUITY) %>%
  filter(!is.na(Change),!is.na(DATETIME_IN),!is.na(DATETIME_OUT))

x$Acuity <- paste("ESI Level",x$Acuity)

dt <- split(x,x$Acuity)
dt$All <- x

n.acuity.load <- lapply(dt,function(x) select(x,DATETIME_IN,DATETIME_OUT,Change) %>% nurse.load()) %>%
  melt(id.vars = c("Month","Weekday","Hour"),measure.vars = 'Load',
       value.name = "Load") %>% group_by(L1) %>% 
  mutate(Type = "Nurse.Acuity",Normalized.Load = scale(Load))

names(n.acuity.load)[6] <- "Acuity"

# METHODOLOGY WORKLOAD

x <- n.acuity %>% select(Acuity = PATIENT_TYPE,DATETIME_IN,DATETIME_OUT,Change = METHODOLOGY_WORKLOAD) %>%
  filter(!is.na(Change),!is.na(DATETIME_IN),!is.na(DATETIME_OUT))

x$Acuity <- paste("ESI Level",x$Acuity)
x <- filter(x,!is.na(Acuity),!is.na(DATETIME_IN),!is.na(DATETIME_OUT))

dt <- split(x,x$Acuity)
dt$All <- x

n.work.load <- lapply(dt,function(x) select(x,DATETIME_IN,DATETIME_OUT,Change) %>% nurse.load()) %>%
  melt(id.vars = c("Month","Weekday","Hour"),measure.vars = 'Load',
       value.name = "Load") %>% group_by(L1) %>%
  mutate(Type = "Nurse.Workload",Normalized.Load = scale(Load))

names(n.work.load)[6] <- "Acuity"
rm(n.acuity,x)

# Plot --------------------------------------------------------------------

plot.df <- rbind(esi.load,n.acuity.load,n.work.load,orders.load) %>% 
  ungroup() %>% mutate(Weekday.Hour = (Weekday-1)*24 + Hour) %>% 
  melt(id.vars = c("Month","Weekday","Hour","Acuity","Type","Weekday.Hour"),
       variable.name = "Metric",measure.vars = c("Load","Normalized.Load"))

save(plot.df,file = "Load_Plot.RData")

wd.list <- c("Sun","","Mon","","Tue","","Wed","","Thu","","Fri","","Sat")
labels <- paste(c(rep(seq(0,23,12),7),0),wd.list,sep="\n")

ggplot(filter(plot.df,Type == "ESI",Month == 7),aes(x = Weekday.Hour,y = Normalized.Load,color = Acuity)) +
  geom_point() + labs(x = "Weekly Timeline",y = "Mean Arrivals") +
  scale_x_discrete(breaks=seq(0,168,12),labels = labels) + geom_line()

ggplot(filter(plot.df,Month == 7,Metric == "Load"),aes(x = Weekday.Hour,y = value,color = Type,shape = Acuity))+#,
                                      #group = interaction(Acuity,Type))) +
  geom_point() +labs(x = "Weekly Timeline",y = "Mean Arrivals") +
  scale_x_discrete(breaks=seq(0,168,12),labels = labels)
