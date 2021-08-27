#- STA 380 Review lecture, data wrangling
rm(list = ls())

#Load the data.table ackage 
install.packages("data.table")
library(data.table)


download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/2008.csv", destfile='./project/volume/data/raw/2008.csv', method='curl')
#download.file(url="https://s3.amazonaws.com/stat.184.data/Flights/airports.csv",destfile='./project/volume/data/raw/airports.csv', method='curl')



#Load the data
dat <- fread('./project/volume/data/raw/2008.csv')

dat 
head(dat)

#dat[i,j,by] #dat[row, cols, group by] #by means group by

#-Definition
#-Filtering:subletting or removing observations based on some condition(s)
#>,<. ==, !=, <=, >=, !, & and, | or

#- the following are for subsetting on rows 
dat[DepDelay > 15]
newdat <- dat[DepDelay > 15]

newdat <- dat[Dest == 'YYZ']

newdat <- dat[Dest == 'TPA']

newdat <- dat[DepDelay > 15 & Dest == 'TPA']

newdat <- dat[DepDelay > 15 & Dest == 'TPA',length(Cancelled)]

newdat <- dat[DepDelay > 15 & Dest == 'TPA',.N]




newdat <- dat[,Dest]                    
newdat <- dat[,list(Dest)]

newdat <- dat[,list(Dest,DepDelay)]

newdat2 <- dat[, .(Dest,DepDelay)]


newdat <- dat[DepDelay > 15 & Dest == 'TPA',.(mean(DepDelay))]

newdat <- dat[DepDelay > 15 & Dest == 'TPA',
              .(mean_dep_delay_if_waited_15_minutes = mean(DepDelay))]

newdat <- dat[,
              .(mean_distance = mean(Distance)),
              by = .(Dest)]
newdat <- dat[,
              .N,
              by = .(Dest)]

newdat[order(Dest)]

# Chaining is done in datatable by using more square brackets
newdat <- dat[order(Dest),
              .N,
              by = .(Dest)][order(Dest)]
#a better way to do  
newdat <- dat[,
              .N,
              by = .(Dest)][order(Dest)]
#dat[so something][do something]

newdat <- dat[,
              .N,
              by = .(Dest)][order(-Dest)]

#- We can use expressions in the by
newdat <- dat[,
              .N,
              .(DepDelay > 0,ArrDelay > 0)] #logic operator 
  

newdat <- dat[,
              lapply(.SD,mean),
              by = .(Origin),
              .SDcols = c("DepDelay","Distance")]


newdat <- dat[,
              lapply(.SD,mean),
              by = .(Origin,Dest),
              .SDcols = c("DepDelay","Distance")]

dat[,.(exp_DeP_Del = exp(DepDelay))]  # 2.7^(DepDelay)


dat[,.(total_Delay = DepDelay + ArrDelay)] 

system.time(newdat <- dat[Dest =="TPA"])
system.time(newdat <- dat[Dest =="TPA"])
str(dat)

setkey(dat,Dest)
str(dat)
dat["TPA"]
dat[.("TPA")]
dat[.(c("TPA","CLE"))]

setkey(dat,Dest,Origin)
dat[.("TPA")]
key(dat)
dat[.("TPA","ABE")]

dat[.("TPA","TLH")]

dat[.(,"TLH")]
dat[.(unique(Dest),"ORD")]

dat[.(unique(Dest),"ORD"),.N]


Avg_delay_tab <- dcast(dat,
                       Origin + UniqueCarrier ~ ., 
                       mean, 
                       na.rm = T , value.var=c("DepDelay"))

mean( dat[Origin =="ABE" & UniqueCarrier == "9E",DepDelay],na.rm = T)


m_Avg_delay_tab <- melt(Avg_delay_tab, id = c("Origin","UniqueCarrier"))



dat_airp <- fread("./project/volume/data/raw/airports.csv")


setkey(dat,Origin)

setnames(dat_airp, "iata_code", "Origin")

dat$Origin <- as.character(dat$Origin)
dat_airp$Origin <- as.character(dat_airp$Origin)


all_dat <- merge(dat, dat_airp, all.x = T)




