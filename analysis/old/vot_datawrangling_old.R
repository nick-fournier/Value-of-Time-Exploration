#packages
library(data.table)
library(nnet)
library(mlogit)
library(pbapply)
library(ggplot2)
library(gridExtra)
library(MASS)
#
#### Loading data ####
#Loading fare data
fares <- list(
  rail = fread("./data/fares/fares_commuterrail_2011.csv"),
  railmonth = fread("./data/fares/fares_commuterrail_monthly_2011.csv"),
  transit = fread("./data/fares/fares_transit_2011.csv"),
  taxi = c(start=2.32, permile=2.68, perhr=24.98),#Taxi fares (2011)
  drive = c(permile=0.585) #Driving cost per mile 0.585
)

#Loading XY data
spatial <- list(
  blocks = fread("./data/spatial/census_blocks_xy.csv", colClasses = c(rep("character",7),rep("numeric",2)))[,.(GEOID10,X,Y)],
  stations = fread("./data/spatial/commuter_rail_xy.csv", colClasses = c(rep("character",7),rep("numeric",2)))[,.(STATION,C_RAILSTAT,X,Y)],
  zones = fread("./data/fares/station_zones.csv"),
  blocks2taz = fread("./data/spatial/blocks_to_taz.csv", colClasses = c("character",rep("numeric",3)))[,.(GEOID10,TAZ)],
  taz_xy = fread("./data/spatial/taz_xy.csv", select = c("ID","X","Y"), col.names = c("TAZ","X","Y"))
)

#Loading MTS data #ditching columns we don't need
vehcols <- c("SAMPN","USER")
hhcols <- c("SAMPN","HISP","RACE","INCOME","HHWGT","EXPWGT","HHSIZ","HHVEH")
percols <- c("SAMPN","PERNO","GEND","AGE","EDUCA","PWGT","EXPPWGT")
placecols <- c("SAMPN","PERNO","MODE","MODE2","FARE","TOLLC","TOLLT","TOLLE","TOLLX","EPARK","PRKUN",
               "TRPDUR","TPURP","DEP_HR","DEP_MIN","ARR_HR","ARR_MIN","PLANO","STATE10","COUNTY10","COUSUB10","TRACT10","BLOCK10")
HH <- fread("./data/MTS/HH.csv", select = hhcols)
PER <- fread("./data/MTS/PER.csv", select = percols)
PLACE <- fread("./data/MTS/PLACE.csv", select = placecols)
VEH <- fread("./data/MTS/VEH.csv", select = vehcols)
rm(hhcols, percols, placecols, vehcols)

#Choice alternative cost skims, using workday AM
skims_am <- list(
  'COMMUTER RAIL' = fread("./data/ttskims/AMWcommrail.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'LOCAL BUS' = fread("./data/ttskims/AMWlocalbus.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'SUBWAY' = fread("./data/ttskims/AMWrapidtransit.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'DRIVE' = fread("./data/ttskims/AMsov.csv", col.names = c("OTAZ","DTAZ","TRPDIST","TRPDUR","OVTT","COST"))
  )

skims_md <- list(
  'COMMUTER RAIL' = fread("./data/ttskims/MDWcommrail.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'LOCAL BUS' = fread("./data/ttskims/MDWlocalbus.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'SUBWAY' = fread("./data/ttskims/MDDrapidtransit.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'DRIVE' = fread("./data/ttskims/MDsov.csv", col.names = c("OTAZ","DTAZ","TRPDIST","TRPDUR","OVTT","COST"))
  )

skims_pm <- list(
  'COMMUTER RAIL' = fread("./data/ttskims/PMWcommrail.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'LOCAL BUS' = fread("./data/ttskims/PMWlocalbus.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'SUBWAY' = fread("./data/ttskims/PMWrapidtransit.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'DRIVE' = fread("./data/ttskims/PMsov.csv", col.names = c("OTAZ","DTAZ","TRPDIST","TRPDUR","OVTT","COST"))
  )

skims_nt <- list(
  'COMMUTER RAIL' = fread("./data/ttskims/NTWcommrail.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'LOCAL BUS' = fread("./data/ttskims/NTDlocalbus.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'SUBWAY' = fread("./data/ttskims/NTWrapidtransit.csv", col.names = c("OTAZ","DTAZ","TRPDUR","OVTT","COST")),
  'DRIVE' = fread("./data/ttskims/NTsov.csv", col.names = c("OTAZ","DTAZ","TRPDIST","TRPDUR","OVTT","COST"))
  )

#### Special functions ####
fun.dist <- function(x1,y1, x2,y2) sqrt((x2-x1)^2 + (y2-y1)^2)
fun.neareststat <- function(pX,pY) spatial$stations[which.min(fun.dist(pX,pY,X,Y)), .(STATION,ZONE)]
fun.nearestzone <- function(XY) spatial$stations[which.min(fun.dist(XY[1],XY[2],X,Y)), ZONE]

#### Formatting fare data ####
#Converting fares from matrix to long form
fares[['rail']] <- melt(fares[['rail']], id.vars = "OZONE", variable.name = "DZONE", value.name = "COST")
fares[['railmonth']] <- melt(fares[['railmonth']], id.vars = "OZONE", variable.name = "DZONE", value.name = "COST")
fares[['transit']] <- melt(fares[['transit']], id.vars = "MODE", variable.name = "PASS", value.name = "COST", na.rm = T)
#Formatting for merging later
fares$rail[ , (c("OZONE","DZONE")) := lapply(.SD, function(x) gsub("ZONE ","",x)), .SDcols=c("OZONE","DZONE")]
fares$railmonth[ , (c("OZONE","DZONE")) := lapply(.SD, function(x) gsub("ZONE ","",x)), .SDcols=c("OZONE","DZONE")]

#### Formatting MTS ####

#Clean up VEH dat
setnames(VEH,'USER','PERNO')
VEH <- unique(VEH[PERNO<98,])
VEH[,CAR:=T]
VEH[is.na(CAR), CAR:=F]

#Merging with HH and Person data
MTS <- merge(PER, HH, by = "SAMPN")
MTS <- merge(MTS,PLACE, by = c("SAMPN","PERNO"))
MTS <- merge(MTS,VEH, by = c("SAMPN","PERNO"), all.x = T)
MTS[is.na(CAR), CAR:=F]
rm(HH,PER,PLACE, VEH)

#Trip Origin & Destination
MTS[,COUSUBID := paste(STATE10,sprintf("%03d",COUNTY10),sprintf("%05d",COUSUB10),sep="")]
MTS[,TRACTID := paste(STATE10,sprintf("%03d",COUNTY10),sprintf("%06d",TRACT10),sep="")]
MTS[,GEOID10 := paste(STATE10,sprintf("%03d",COUNTY10),sprintf("%06d",TRACT10),sprintf("%04d",BLOCK10),sep="")]
MTS <- MTS[ , !c("STATE10","COUNTY10","TRACT10","BLOCK10")]
#Label as destination place number
setnames(MTS, "PLANO","DPLANO")
#Making new column with origin place number (previous place)
MTS[ , OPLANO := as.integer(DPLANO-1)]
#making separate origin and destination tables
MTSor <- MTS[,.(SAMPN,PERNO,DPLANO,GEOID10,TRACTID,COUSUBID)]
setnames(MTSor,c("DPLANO","GEOID10","TRACTID","COUSUBID"),c("OPLANO","OGEOID10","OTRACT","OCOUSUB"))
#Rename the destination block id
setnames(MTS,c("GEOID10","TRACTID","COUSUBID"),c("DGEOID10","DTRACT","DCOUSUB"))
#This removes all the starting origin blocks, i.e. there can't be an origin earlier than the first origin.
MTS <- merge(MTS, MTSor, by = c("SAMPN","PERNO","OPLANO"))
#Merging XY coordinates
MTS <- merge(MTS, spatial$blocks, by.x = c("OGEOID10"), by.y = c("GEOID10"))
setnames(MTS, c("X","Y"), c("O.X","O.Y"))
MTS <- merge(MTS, spatial$blocks, by.x = c("DGEOID10"), by.y = c("GEOID10"))
setnames(MTS, c("X","Y"), c("D.X","D.Y"))
rm(MTSor)


####Replace transfer purposes with trip purpose of longest stay, if multi segement trip
MTS[ , ARR_TIME := ARR_HR+ARR_MIN/60]
MTS[ , DEP_TIME := DEP_HR+DEP_MIN/60]
MTS[ , STAY_DUR := (DEP_HR+DEP_MIN/60) - (ARR_HR+ARR_MIN/60)]

#Which ones need fixing?
inferred <- unique(MTS[TPURP %in% c(8,11,96,97),.(SAMPN,PERNO)])
inferred <- MTS[SAMPN %in% inferred$SAMPN & PERNO %in% inferred$PERNO, ]
#Keep all non transfer trips
inferred <- inferred[!(TPURP %in% c(2,8,11,96,97)), ]
#Pick purpose of longest stay
inferred <- inferred[inferred[, .I[which.max(STAY_DUR)], by=.(SAMPN,PERNO)]$V1, .(SAMPN,PERNO,TPURP2=TPURP)]

#Merge back to MTS
MTS <- merge(MTS, inferred, by = c("SAMPN","PERNO"), all.x = T)

#Replace the transfer purpose
MTS[TPURP %in% c(8,11,96,97) & !is.na(TPURP2), TPURP := TPURP2]
MTS[ , TPURP2 := NULL]

#Find approx trip distance
MTS[, TRPDIST := fun.dist(D.X,D.Y, O.X,O.Y)/1609.34] #1609.34m=1mile
#Trip duration as numeric
MTS[ , TRPDUR := as.numeric(TRPDUR)]
#Removing zero distance!
MTS <- MTS[TRPDIST>0,]
#Removing ridiculously long trips
MTS <- MTS[TRPDUR<180 & TRPDUR>5, ]
#Remove NA age
MTS <- MTS[AGE<99,]

#Trip dist breaks
#dists <- c(DIST1=0,DIST2=0.5,DIST3=1,DIST4=2,DIST5=4,DIST6=8,END=128)
dists <- c(DIST1=0,DIST2=0.5,DIST3=1,DIST4=2,DIST5=4,DIST6=8,DIST7=12,END=128)
for(i in 1:(length(dists)-1)) MTS[TRPDIST >= dists[i] & TRPDIST < dists[i+1], TRPDIST_GRPD := names(dists[i])]

#ggplot(MTS[!(MODE %in% 1:2),],aes(x=TRPDIST_GRPD)) + geom_bar()

#Trip duration breaks
durs <- c(DUR1=0,DUR2=10,DUR3=20,DUR4=30,DUR5=40,END=180)
for(i in 1:(length(durs)-1)) MTS[TRPDUR >= durs[i] & TRPDUR < durs[i+1], TRPDUR_GRPD := names(durs[i])]


#Trip purposes
MTS[TPURP %in% c(1,2), TRPPUR := "HOME"]
# 1 Working at home (for pay)
# 2 All other home activities
MTS[TPURP %in% c(3,4,5,6,7,12), TRPPUR := "WORK/SCHOOL"]
# 3 Work/Job
# 4 All other activities at work
# 5 Volunteer Work/Activities
# 12 Work Business Related
# 6 Attending Class
# 7 All other School Activities
MTS[TPURP %in% c(9,10, 8,11,96,97), TRPPUR := "PICKUP/DROPOFF/OTHER"]
# 9 Drop off passenger from car
# 10 Pick up passenger from car
###
# 8 Changed type of transportation
# 11 While Traveling -- Other, Specify
# 96 Loop trip
# 97 Other, SPECIFY
MTS[TPURP %in% c(13,14,15,16,17,19,20), TRPPUR := "SHOPPING/ERRANDS"]
# 13 Service private vehicle (gas, oil lube, etc.)
# 14 Routine shopping (groceries, clothing, convenience store, HH maintenance)
# 15 Shopping for major purchases or specialty items (appliance, electronics, new vehicle, major HH repairs)
# 16 Household errands (bank, dry cleaning, etc.)
# 17 Personal business (visit government office, attorney, accountant)
# 19 Health care (doctor, dentist)
# 20 Civic/Religious activities
MTS[TPURP %in% c(18,21,22,23), TRPPUR := "RECREATION"]
# 18 Eat meal outside of home
# 21 Outdoor recreation/entertainment
# 22 Indoor recreation/entertainment
# 23 Visit friends/relatives


#Gender
MTS[ , GEND := as.character(GEND)]
MTS[ , GEND := ifelse(GEND=="1","MALE","FEMALE")]

#Age breaks
MTS <- MTS[AGE>=16, ]
ages <- c(AGE1=16,AGE2=20,AGE3=30,AGE4=40,AGE5=50,AGE6=60,AGE7=70,END=100)
for(i in 1:(length(ages)-1)) MTS[AGE >= ages[i] & AGE < ages[i+1], AGE_GRPD := names(ages[i])]

#ggplot(MTS[!(MODE %in% 1:2),],aes(x=AGE_GRPD)) + geom_bar()

#Income
MTS[INCOME %in% c(1,2), INCOME := 1]
MTS[INCOME %in% c(3,4), INCOME := 2]
MTS[INCOME > 4 & INCOME < 99, INCOME := as.integer(INCOME - 2)]
MTS[ , INCOME := as.character(INCOME)]

#Time: AM (06:00 - 9:59), MID (10:00-15:59), PM (16:00-18:59), NT (19:00-5:59)
MTS[ , DEP_TIME := DEP_HR + DEP_MIN/60]
hist(MTS[TRPPUR != "HOME", DEP_TIME], breaks=24, xlim = c(0,24))
MTS[DEP_TIME >= 6 & DEP_TIME < 10, TRPTIME := "AM"]
MTS[DEP_TIME >= 10 & DEP_TIME < 16, TRPTIME := "MD"]
MTS[DEP_TIME >= 16 & DEP_TIME < 19, TRPTIME := "PM"]
MTS[DEP_TIME >= 19 | DEP_TIME < 6, TRPTIME := "NT"]

#Fare type
MTS[FARE %in% c(1,97,98,99), PASS := "CASH"]
MTS[FARE==2, PASS := "WEEK"]
MTS[FARE==3, PASS := "MONTH"]
MTS[FARE %in% c(4,7), PASS := "CHARLIE"]
MTS[FARE==5, PASS := "STUDENT"]
MTS[FARE==6, PASS := "TRANSFER"]

#Mode type MODE1 is non-transit type from PLACE file, MODE2 is transit mode type
MTS[ , MODE1 := MODE] #moving old MODE to MODE1, repurposing MODE
MTS[ , MODE := as.character(MODE)]
MTS[ , MODE := NA]
MTS[MODE1==1, MODE:= "WALK"]
MTS[MODE1==2, MODE:= "BIKE"]
MTS[MODE1 %in% c(3,11), MODE:= "DRIVE"]
MTS[MODE1==9, MODE:= "TAXI"]
MTS[MODE2==1, MODE:="LOCAL BUS"]
MTS[MODE2==2, MODE:="INNER EXPRESS"]
MTS[MODE2==3, MODE:="LOCAL BUS"]
MTS[MODE2==4, MODE:="SUBWAY"]
MTS[MODE2==5, MODE:="COMMUTER RAIL"]
#Fixing weirdo bits
MTS[MODE=="INNER EXPRESS" & PASS=="WEEK", PASS:="MONTH"]
MTS[MODE1==5 & is.na(MODE), MODE:="LOCAL BUS"]
MTS[MODE1==6 & is.na(MODE), MODE:="SUBWAY"]
#Removing car/moto passenger, school bus, ferry, or NA.
#unique(MTS[is.na(MODE),MODE1])
MTS <- MTS[!is.na(MODE), !c("MODE1","MODE2")]

#### Transit costs ####
#MTS <- merge(MTS, fares$transit, by = c("MODE","PASS"), all.x=T)
#Ignore pass discount, default to cash fare
MTS <- merge(MTS, fares$transit[PASS=='CASH',!"PASS"], by = "MODE", all.x=T)

#### Commuter Rail Costs ####
#Format station data
spatial$stations <- merge(spatial$stations, spatial$zones, by = "STATION")
#Making sure that the fares match the stations
spatial$stations <- spatial$stations[ZONE %in% unique(fares$rail[,OZONE]), ]

#Just the commuter rails
MTScrail <- MTS[MODE=="COMMUTER RAIL",!"COST"]
#Adding dummy columns to MTS for binding later
MTS[ , c("OZONE","DZONE")] <- as.character(NA)

#Find nearest rail station
# MTScrail[, (c("OSTATION","OZONE")) := mapply(fun.neareststat, O.X,O.Y), by=TID]
# MTScrail[, (c("DSTATION","DZONE")) := mapply(fun.neareststat, D.X,D.Y), by=TID]
#MTScrail[, fun.neareststat(O.X[1],O.Y[1]), by = .(O.X,O.Y)]
MTScrail[, OZONE := apply(MTScrail[,.(O.X,O.Y)], 1, fun.nearestzone)]
MTScrail[, DZONE := apply(MTScrail[,.(D.X,D.Y)], 1, fun.nearestzone)]

# #Merging fare costs to commuter rail
MTScrail <- merge(MTScrail, fares$rail, by = c("OZONE","DZONE"))
#If accounting for pass type
# MTScrail <- rbind(merge(MTScrail[PASS %in% c("CASH","CHARLIE","WEEK","TRANSFER"),], fares$rail, by = c("OZONE","DZONE")),
#                   merge(MTScrail[PASS %in% c("MONTH","STUDENT"),], fares$railmonth, by = c("OZONE","DZONE")))
# MTScrail[PASS == "STUDENT", COST:=COST/2]
# #Adjust weekly/monthly passes for average effective trip rate
# #2 trips/day, 10 days/week, 4 weeks/month
# MTScrail[PASS=="WEEK",COST:=COST/10/2]
# MTScrail[PASS %in% c("MONTH","STUDENT"), COST:=COST/4/10/2]
#Rebinding back into MTS, replacing old
MTS <- rbind(MTS[MODE!="COMMUTER RAIL", ], MTScrail[, colnames(MTS), with=F])
rm(MTScrail)

#### Taxi & driving cost ####
#taxi costs
MTS[MODE=="TAXI", COST:=with(fares, taxi['start'] + taxi['permile']*TRPDIST + taxi['perhr']*TRPDUR/60)]
#Tolls
MTS[is.na(TOLLC) | TOLLC==99, TOLLC:=0]
#Removing tolls that are ridiculous
MTS <- MTS[TOLLC<10, ]
#Parking and parking passes (hour, day, month, year)
MTS[is.na(EPARK) | EPARK==9999, EPARK:=0]
MTS[PRKUN==3, EPARK := EPARK/5] #5 biz days/week
MTS[PRKUN==4, EPARK := EPARK/(5*4)] #5 biz days/week and 4 weeks a month
MTS[PRKUN==5, EPARK := EPARK/260] #260 biz days/year
#Drive costs
MTS[MODE=="DRIVE", COST:=fares$drive['permile']*TRPDIST + TOLLC + EPARK]
MTS[is.na(COST), COST := 0]
#removing ridiculously long outlier travel times (anything slower than 20mph for trips longer than 60minutes)
MTS <- MTS[!(MODE %in% c('DRIVE','TAXI') & (TRPDUR - 3*TRPDIST)>60), ]
#checking graphically
# ggplot(aes(x=TRPDUR, y=TRPDIST), data=MTS[MODE=='DRIVE' & TRPDUR>0,.(AGE,TPURP,TRPDUR,TRPDIST)]) + 
#   geom_point(size=0.1) + stat_smooth(method="lm", formula = y~0+x) + #coord_fixed() + 
#   geom_abline(intercept=-20, slope=20/60)

#Find unique OD pairs shared between MTS and CTPS skims
#First find all unique OD pairs
OD <- unique(MTS[ ,.(OGEOID10,DGEOID10)])
#Merging TAZ ID to blocks
OD <- merge(OD, spatial$blocks2taz, by.x="OGEOID10", by.y="GEOID10")
setnames(OD,"TAZ","OTAZ")
OD <- merge(OD, spatial$blocks2taz, by.x="DGEOID10", by.y="GEOID10")
setnames(OD,"TAZ","DTAZ")
#Keeping just the ones we have and merging TAZ's to MTS
MTS <- merge(MTS, OD, by = c("OGEOID10","DGEOID10"))
#Trip id
MTS[ , TRIPID := 1:nrow(MTS)]

#### Format skims ####
modenames <- names(skims_am)
skims <- list("AM" = skims_am, "MD" = skims_md, "PM" = skims_pm, "NT" = skims_nt)
skimnames <- names(skims)

skims <- lapply(skimnames, function(N) {
  skim <- skims[[N]]
  print(paste("Formatting", N))
  skim <- lapply(modenames, function(mode) {
    cat(paste(mode," "))
    x = skim[[mode]]
    #Keeping only relevant OD pairs
    x = merge(x, unique(OD[,.(OTAZ,DTAZ)]), by = c("OTAZ","DTAZ"))
    #Adding dummy column for MODE
    x[ , MODE := mode]
    #Ditching garbage CTPS costs
    x[ , COST := NULL]
    #Getting distances for modes other than drive, because drive already has trip length
    if(!("TRPDIST" %in% colnames(x))) {
      #Adding X Y coordinates
      x <- merge(x, spatial$taz_xy, by.x="OTAZ",  by.y="TAZ")
      #Renaming
      setnames(x, c("X","Y"), c("O.X","O.Y"))
      #Adding X Y coordinates
      x <- merge(x, spatial$taz_xy, by.x="DTAZ",  by.y="TAZ")
      #Renaming
      setnames(x, c("X","Y"), c("D.X","D.Y"))
      #find zone by nearest station
      x[, TRPDIST := fun.dist(D.X,D.Y, O.X,O.Y)/1609.34] #1609.34m=1mile
    }
    #Assigning drive costs
    if(mode == 'DRIVE') {
      #Average cost to park
      x <- merge(x, setNames(MTS[MODE=='DRIVE', mean(EPARK), by=DTAZ],c("DTAZ","EPARK")), by="DTAZ", all.x = T)
      #Average cost for tolls
      x <- merge(x, setNames(MTS[MODE=='DRIVE', mean(TOLLC), by=.(DTAZ,OTAZ)],c("OTAZ","DTAZ","TOLLC")), by=c("OTAZ","DTAZ"), all.x = T)
      x[is.na(EPARK), EPARK:=0]
      x[is.na(TOLLC), TOLLC:=0]
      x[ , COST := fares$drive['permile']*TRPDIST + TOLLC + EPARK]
    }
    #Assigning commuter rail costs
    if(mode == 'COMMUTER RAIL') {
      #Accounting for transfer costs
      x[ , TRPDUR := TRPDUR + OVTT]
      #Finding OD zones
      x[, (c("OSTATION","OZONE")) := fun.neareststat(O.X,O.Y), by = .(O.X,O.Y)]
      x[, (c("DSTATION","DZONE")) := fun.neareststat(D.X,D.Y), by = .(D.X,D.Y)]
      #Determining commuter rail prices
      x[ , (c("EPARK","TOLLC")) := 0]
      x <- merge(x, fares$rail, by = c("OZONE","DZONE"), all.x = T)
    }
    #Assigning local bus/subway costs
    if(mode %in% c('LOCAL BUS','SUBWAY')) {
      #Accounting for transfer costs
      x[ , TRPDUR := TRPDUR + OVTT]
      x <- merge(x, fares$transit[PASS=="CASH",], by = "MODE", all.x=T)
    }
    #Select output cols
    x[ , (c("EPARK","TOLLC")) := 0]
    x <- x[,.(OTAZ,DTAZ,TRPDUR,TRPDIST,EPARK,TOLLC,COST,MODE)]
    #add time of day column
    x$TRPTIME <- N
    #Remove super long times way outside of MTS
    #x <- x[TRPDUR<200, ]
    return(x)
  })
  names(skim) <- modenames
  #adding taxi
  skim[['TAXI']] <- skim[['DRIVE']]
  skim[['TAXI']]$COST <- skim[['TAXI']][ , with(fares, taxi['start'] + taxi['permile']*TRPDIST + taxi['perhr']*TRPDUR/60)]
  skim[['TAXI']]$MODE <- "TAXI"
  #adding walk
  skim[['WALK']] <- skim[['DRIVE']]
  skim[['WALK']]$MODE <- "WALK"
  skim[['WALK']]$TRPDUR <- skim[['WALK']][ , 60*TRPDIST/3.1]
  skim[['WALK']]$EPARK <- 0
  skim[['WALK']]$TOLLC <- 0
  skim[['WALK']]$COST <- 0
  #adding bike
  skim[['BIKE']] <- skim[['DRIVE']]
  skim[['BIKE']]$MODE <- "BIKE"
  skim[['BIKE']]$TRPDUR <- skim[['BIKE']][ , 60*TRPDIST/13]
  skim[['BIKE']]$EPARK <- 0
  skim[['BIKE']]$TOLLC <- 0
  skim[['BIKE']]$COST <- 0
  print("")
  return(skim)
})
names(skims) <- skimnames
modenames <- names(skims$AM)

#### Calibrating skims/MTS ####
#Merging & compare function
check <- lapply(skimnames, function(n) {
  print(paste("Checking",n,"skims"))
  skm <- skims[[n]]
  mts <- MTS[TRPTIME==n,]
  dat <- rbind(merge(skm[['DRIVE']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['SUBWAY']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['LOCAL BUS']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['SUBWAY']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['COMMUTER RAIL']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['TAXI']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['WALK']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')),
               merge(skm[['BIKE']], mts, by = c("MODE","OTAZ","DTAZ"), suffixes=c('.skm','.mts')))
  dat <- dat[,.(MODE,TRPDIST.skm,TRPDIST.mts,TRPDUR.skm,TRPDUR.mts,COST.skm,COST.mts)]
  # #keeping only non zero
  dat <- dat[TRPDIST.skm>0 & TRPDUR.skm>0,]
  #Function to check fit
  comp <- lapply(modenames, function(mode) {
    cat(paste(mode," "))
    #distance results
    dist.mod <- lm(TRPDIST.mts~TRPDIST.skm, data=dat[MODE==mode,])
    dist.fit <- c(coef(dist.mod),summary(dist.mod)$r.squared)
    dist.plot <- ggplot(data=dat[MODE==mode,], aes(x=TRPDIST.skm, y=TRPDIST.mts)) + 
      geom_point(size=0.2) + stat_smooth(method="lm") + theme_classic() + coord_fixed() +
      xlim(0,NA) + ylim(0,NA) + labs(y="Dist (MTS)",x="Dist (Skims)") +
      annotate("text", label = paste("y =",round(dist.fit[1],1),"+",round(dist.fit[2],1),"b"),x=15, y=0)
    #time results
    time.mod <- lm(TRPDUR.mts~TRPDUR.skm, data=dat[MODE==mode,])
    time.fit <- c(coef(time.mod),summary(time.mod)$r.squared)
    time.plot <- ggplot(data=dat[MODE==mode,], aes(x=TRPDUR.skm, y=TRPDUR.mts)) + 
      geom_point(size=0.2) + stat_smooth(method="lm") + theme_classic() + coord_fixed() +
      xlim(0,NA) + ylim(0,NA) + labs(y="Time (MTS)",x="Time (Skims)") +
      annotate("text", label = paste("y =",round(time.fit[1],1),"+",round(time.fit[2],1),"b"),x=100, y=0)
    #cost results
    cost.mod <- lm(COST.mts~COST.skm, data=dat[MODE==mode,])
    cost.fit<- c(coef(cost.mod),summary(cost.mod)$r.squared)
    cost.plot <- ggplot(data=dat[MODE==mode,], aes(x=COST.skm, y=COST.mts)) +
      geom_point(size=0.2) + stat_smooth(method="lm") + theme_classic() + coord_fixed() +
      xlim(0,NA) + ylim(0,NA) + labs(y="Cost (MTS)",x="Cost (Skims)") +
      annotate("text", label = paste("y =",round(cost.fit[1],1),"+",round(cost.fit[2],1),"b"),x=5, y=0)
    #output set
    mod = list(dist.mod, time.mod, cost.mod)
    plot = arrangeGrob(grobs=list(dist.plot, time.plot, cost.plot), ncol=3)
    fit = as.data.table(rbind(dist.fit, time.fit, cost.fit), keep.rownames = T)
    colnames(fit) <- c("var","Intercept","Slope","R-squared")
    #output
    out = list( fit=fit, model=mod, distplot=dist.plot, costplot=cost.plot, timeplot=time.plot, allplot = plot)
  })
  print("")
  names(comp) <- names(skm)
  return(comp)
  })
names(check) <- skimnames

#Fixing minor errors manually
#for(n in skimnames) skims[[n]]$TAXI[ , TRPDUR := TRPDUR + check[[n]]$TAXI$fit[var=='time.fit',Intercept]]

plot(check$AM$`LOCAL BUS`$timeplot)
plot(check$AM$`COMMUTER RAIL`$timeplot)
plot(check$AM$SUBWAY$timeplot)
plot(check$AM$DRIVE$timeplot)
plot(check$AM$TAXI$timeplot)
plot(check$AM$BIKE$timeplot)
plot(check$AM$WALK$timeplot)

#Checking fit of skims vs MTS
check_res <- as.data.table(rbindlist(lapply(skimnames, function(skm) {
  rbindlist(lapply(modenames, function(m) {
    check[[skm]][[m]]$fit[ ,2:3] <- round(check[[skm]][[m]]$fit[ ,2:3],2)
    check[[skm]][[m]]$fit$MODE <- m
    check[[skm]][[m]]$fit$TOD <- skm
    return(check[[skm]][[m]]$fit)
  }))
})))
check_res

#### Finding the alternative choice costs ####
mnldt <- rbindlist(lapply(skimnames, function(t) {
  #Remove "HOME" non-trips, walk, bike, express bus for now; subsetting by TOD
  # mnldt <- MTS[TRPPUR != "HOME" & !(MODE %in% c("WALK","BIKE","INNER EXPRESS","TAXI")) & TRPTIME == t, ]
  mnldt <- MTS[TRPPUR != "HOME" & !(MODE %in% c("WALK","BIKE","INNER EXPRESS")) & TRPTIME == t, ]
  # mnldt <- MTS[TRPPUR != "HOME" & MODE  != "INNER EXPRESS" & TRPTIME == t, ]
  #Removing zone alternatives we don't have
  mnldt <- mnldt[OGEOID10 %in% spatial$blocks2taz$GEOID10 & DGEOID10 %in% spatial$blocks2taz$GEOID10, ]
  #Merging costs to ODs
  alts <- rbindlist(lapply(unique(mnldt$MODE), function (x) {
    merge(mnldt[,.(TRIPID,OTAZ,DTAZ,TRPTIME)],skims[[t]][[x]], by=c("OTAZ","DTAZ","TRPTIME"), all.x = T)
    }))
  #Removing alts that were actually chosen in MTS
  alts <- alts[!mnldt[,.(TRIPID,MODE,TRPTIME)], on=.(TRIPID,MODE,TRPTIME)]
  #Merging person data to alt data
  alts <- merge(alts[,.(TRIPID,TRPDUR,TRPDIST,TRPTIME,EPARK,TOLLC,COST,MODE)],
                mnldt[,!c("TRPDUR","TRPDIST","TRPTIME","EPARK","TOLLC","COST","MODE")], by = "TRIPID")
  #Setting TRUE for the mode taken and false for the alternative not taken
  mnldt[ , CHOICE:=T]
  alts[ , CHOICE:=F]
  #Combining the data set and organizing
  mnldt <- rbind(mnldt,alts)
  mnldt <- mnldt[order(TRIPID),]
  mnldt <- mnldt[INCOME!="99" & GEND!="9" & !is.na(TRPPUR), ]
  mnldt[ , GEND := as.character(GEND)]
  mnldt[ , TRPPUR := as.character(TRPPUR)]
  mnldt[ , TRPTIME := as.character(TRPTIME)]
  #Checking if any are missing, all should be number of modes
  summary(as.data.table(table(mnldt$TRIPID)))
  unique(mnldt$MODE)
  print(paste("using",nrow(mnldt[CHOICE==T,]),"trip samples for the",t,"time period"))

  return(mnldt)
}))
print(paste("using",nrow(mnldt[CHOICE==T,]),"trip samples for whole day"))

#Combining modes to avoid singularity due to flat PT fare prices. 
mnldt[ , MODE_GRPD := MODE]
mnldt[MODE %in% c("COMMUTER RAIL","LOCAL BUS","SUBWAY"), MODE_GRPD := "PT"]
#Carrying over the choice
mnldt[ , CHOICE_GRPD := CHOICE]
mnldt[TRIPID %in% mnldt[MODE %in% c("COMMUTER RAIL","LOCAL BUS","SUBWAY") & CHOICE==T, TRIPID] & MODE_GRPD=='PT', CHOICE_GRPD := T]

#Fixing cases where there is no PT alternative! (0 TRPDUR means impossible, changing to 999 to weed out)
# mnldt[TRIPID %in% mnldt[MODE_GRPD=='PT', sum(TRPDUR),by=TRIPID][V1==0, TRIPID], .(TRIPID,TRPDUR,MODE,CHOICE)]
mnldt[TRPDUR==0, TRPDUR := 999]

#Ditching the extra PT choices, keeping only the one chosen OR the fastest one possible
mnldt[ , KEEP:=F] #start setting all to false
ids <- mnldt[MODE %in% c('DRIVE','TAXI') & CHOICE==T, TRIPID]
mnldt[mnldt[TRIPID %in% ids & MODE_GRPD=='PT', .I[which.min(TRPDUR)], by = TRIPID]$V1, KEEP:=T]
mnldt[MODE_GRPD %in% c('DRIVE','TAXI'), KEEP:=T] #keep drive and taxi alts
mnldt[MODE_GRPD == 'PT' & CHOICE==T, KEEP:=T] #keep the PT choice if chosen
#mnldt <- mnldt[KEEP==T,]
base_mnldt <- mnldt
#

#No transfer after 2 hours?
# base_mnldt[TRPDUR>120 & MODE %in% c("SUBWAY","LOCAL BUS"), COST := COST + 0.1]
base_mnldt[TRPDUR>120 & MODE %in% c("SUBWAY","LOCAL BUS"), COST := 2*COST]

# base_mnldt[TRPDUR>60 & MODE =="SUBWAY", COST := 4]
# base_mnldt[TRPDUR>60 & MODE =="LOCAL BUS", COST := 3]
# base_mnldt[TRPDUR>120 & MODE =="SUBWAY", COST := 6]
# base_mnldt[TRPDUR>120 & MODE =="LOCAL BUS", COST := 4.5]
# base_mnldt[TRPDUR>180 & MODE =="SUBWAY", COST := 8]
# base_mnldt[TRPDUR>180 & MODE =="LOCAL BUS", COST := 6]

#Set back
# base_mnldt[MODE =="SUBWAY", COST := 2]
# base_mnldt[MODE =="LOCAL BUS", COST := 1.5]

#Check if done right, should all be same
table(base_mnldt[KEEP==T,MODE_GRPD])
hist(base_mnldt[, TRPDUR],breaks=100)



#### Saving ####
print("Saving MNL data table")
save(base_mnldt, MTS, file = "./analysis/dat_mnldt.RData")
print("Saving skim data")
save(check, skims, file = "./analysis/dat_skimdat.RData")

#Cleanup
rm(list=ls())
gc()




