#packages
library(data.table)
library(nnet)
library(mlogit)
library(pbapply)
library(ggplot2)
library(gridExtra)
library(MASS)
library(stringr)
library(brms)
library(fitdistrplus)
#library(mnlogit)

#Loading data
load("dat_mnldt.RData")
mnldt <- base_mnldt


#Removing places where dist is ridiculous.
mnldt <- mnldt[!(TRIPID %in% mnldt[TRPDUR>60*3,TRIPID]), ]
#hist(merge(mnldt[TRPDUR>10, max(TRPDUR), by = TRIPID], mnldt)[ , abs(V1-TRPDUR)/TRPDUR, by = TRIPID][V1<10, V1], breaks=50)
ids = merge(mnldt[TRPDUR>10, max(TRPDUR), by = TRIPID], mnldt)[TRPDUR>10, abs(V1-TRPDUR)/TRPDUR, by = TRIPID][V1>10, TRIPID]
mnldt <- mnldt[!(TRIPID %in% ids),]
rm(ids, base_mnldt)


#Trips by mode and type?
#table(mnldt[CHOICE==T,.(MODE,TRPPUR)])

#Removing TAXI
#mnldt <- mnldt[MODE!="TAXI" & !(TRIPID %in% mnldt[MODE=="TAXI" & CHOICE==T,TRIPID]),]

#Removing travel and rec from purpose
#mnldt <- mnldt[!(TRIPID %in% mnldt[TRPPUR %in% c("RECREATION"),TRIPID]), ]

#dist of travel costs
#ggplot(data=mnldt, aes(x=TRPDUR/60, y=COST, color=MODE)) + geom_point()

#Ratio of vehicles to household size
mnldt[ , VEHRAT := HHVEH/HHSIZ]
mnldt[HHVEH>0 , HHCAR := T]


#function to find VOT error
fun.voterr <- function(Bt,SEt,Bc,SEc) {
  mat <- as.matrix(expand.grid(c=c(1,-1),t=c(1,-1)))
  voterr <- 60*(Bt + mat[,"t"]*SEt)/(Bc + mat[,"c"]*SEc)
  return(c(min=min(voterr),max=max(voterr)))
}

fun.midpt <- function(breakseq) {
  sapply(1:(length(breakseq)-1), function(i) mean(c(breakseq[i],breakseq[i+1])))
}

#Adding and formatting useful columns
varlabelvals = list(INCOME = data.table(VAR = sort(unique(mnldt$INCOME)),
                                        LABEL = c("< 25,000","25,000-49,999",
                                                  "50,000-74,999","75,000-99,999","100,000-149,999","$\\geq$ 150,000"),
                                        VAL = fun.midpt(c(0    ,25000,50000,75000,100000,150000,200000))),
                    AGE_GRPD = data.table(VAR = sort(unique(mnldt$AGE_GRPD)),
                                          LABEL =  c("16-20","20-29","30-39","40-49","50-59","60-69","$\\geq$ 70"),
                                          VAL = mnldt[order(AGE_GRPD), median(AGE), by = AGE_GRPD]$V1),
                    TRPDIST_GRPD = data.table(VAR = sort(unique(mnldt$TRPDIST_GRPD)),
                                              LABEL =  c("< 0.5","0.5-1","1-2","2-4","4-8","8-12","$\\geq$ 12"),
                                              VAL = mnldt[order(TRPDIST_GRPD), median(TRPDIST), by = TRPDIST_GRPD]$V1),
                    TRPDUR_GRPD = data.table(VAR = sort(unique(mnldt$TRPDUR_GRPD)),
                                             LABEL =  c("< 10","10-20","20-30","30-40","$\\geq$ 40"),
                                             VAL = mnldt[order(TRPDUR_GRPD), median(TRPDUR), by = TRPDUR_GRPD]$V1),
                    TRPTIME = data.table(VAR = c("AM","MD","PM","NT"),
                                         LABEL =  c("AM (06:00-9:59)","Midday (10:00-15:59)",
                                                    "PM (16:00-18:59)","Night (19:00-5:59)"),
                                         VAL = mnldt[order(TRPTIME), median(DEP_TIME), by = TRPTIME]$V1),
                    MODE = data.table(VAR = c("COMMUTER RAIL","SUBWAY","LOCAL BUS","DRIVE","TAXI"),
                                         LABEL =  c("Rail","Subway","Bus","Drive","Taxi"),
                                         VAL = NA),
                    TRPPUR = data.table(VAR = c("RECREATION","SHOPPING/ERRANDS","WORK/SCHOOL","PICKUP/DROPOFF/OTHER"),
                                      LABEL =  c("Recreation","Shopping/errands","Work/school","Other"),
                                      VAL = NA),
                    GEND = data.table(VAR = c("FEMALE","MALE"),
                                        LABEL =  c("Female","Male"),
                                        VAL = NA))


#Numeric income
mnldt <- merge(mnldt, varlabelvals$INCOME[,.(VAR,VAL)], by.x='INCOME', by.y='VAR')
setnames(mnldt, 'VAL', 'INCOME_VAL')

#### Model building ####
mnlmodlist <- list()
mnlmodlistjt <- list()

# #Distributions
# hist(mnldt$COST, breaks=100)
# hist(mnldt$TRPDUR, breaks=100)
# hist(mnldt$TRPDIST, breaks=200)
# hist(mnldt$INCOME_VAL)
# barplot(table(mnldt$INCOME))
# hist(mnldt$AGE, breaks=200)
# summary(mnldt$COST)
# summary(mnldt$TRPDUR)
# mnldt[CHOICE==T, .N, by=MODE]
# mnldt[CHOICE==T, .N, by=TRPPUR]


#### Unified models ####
#Alternative specific - COST, TRPDUR, TRPPUR, TRPTIME, TRPDIST
#Individual specific - AGE, INCOME
mnlfullmod <- mlogit(CHOICE~1|AGE+INCOME+TRPPUR+TRPTIME+GEND|COST+TRPDUR, data=mnldt,
                              shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
#                      nests = list("CAR" = "DRIVE", "NOCAR" = c("LOCAL BUS","SUBWAY","COMMUTER RAIL","TAXI")),
#                      un.nest.el = T)
summary(mnlfullmod)

#Estimate for average VOT
mnlavgmod <- mlogit(CHOICE~COST+TRPDUR,#|AGE_GRPD+INCOME+TRPTIME,#+TRPPUR, 
                    data=mnldt[MODE!="TAXI" & !(TRIPID %in% mnldt[MODE=="TAXI" & CHOICE==T,TRIPID]),],
                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
                    # nests = list("CAR" = "DRIVE", "NOCAR" = c("LOCAL BUS","SUBWAY","COMMUTER RAIL","TAXI")),
                    # un.nest.el = T)
#summary(mnlavgmod)
as.data.table(t(summary(mnlavgmod)$CoefTable), keep.rownames = T)[rn=='Estimate', 60*TRPDUR/COST]
#

# mnlfulljointmod <- mlogit(CHOICE~AGE_GRPD:INCOME:TRPPUR:TRPTIME:(COST+TRPDUR)|1, data=mnldt,
#                      shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
# #                      nests = list("CAR" = "DRIVE", "NOCAR" = c("LOCAL BUS","SUBWAY","COMMUTER RAIL","TAXI")),
# #                      un.nest.el = T)
# summary(mnlfulljointmod)

#### Joint models ####

#All combos of jt models
t(combn(c("GEND","INCOME","TRPDIST","AGE","TRPPUR","TRPTIME"), 2))

#Estimating them
mnlmodlistjt[['INCAGE']] <- mlogit(CHOICE~(COST+TRPDUR):INCOME:AGE_GRPD, data=mnldt,
                                   shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['DISTAGE']] <- mlogit(CHOICE~(COST+TRPDUR):TRPDIST_GRPD:AGE_GRPD|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['INCDIST']] <- mlogit(CHOICE~(COST+TRPDUR):INCOME:TRPDIST_GRPD|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

mnlmodlistjt[['INCPURP']] <- mlogit(CHOICE~(COST+TRPDUR):INCOME:TRPPUR|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['INCTIME']] <- mlogit(CHOICE~(COST+TRPDUR):INCOME:TRPTIME|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['DISTPURP']] <- mlogit(CHOICE~(COST+TRPDUR):TRPDIST_GRPD:TRPPUR|1, data=mnldt,
                                     shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

mnlmodlistjt[['DISTTIME']] <- mlogit(CHOICE~(COST+TRPDUR):TRPDIST_GRPD:TRPTIME|1, data=mnldt,
                                     shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['AGEPURP']] <- mlogit(CHOICE~(COST+TRPDUR):AGE_GRPD:TRPPUR|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['AGETIME']] <- mlogit(CHOICE~(COST+TRPDUR):AGE_GRPD:TRPTIME|1, data=mnldt,
                                    shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

mnlmodlistjt[['PURPTIME']] <- mlogit(CHOICE~(COST+TRPDUR):TRPPUR:TRPTIME|1, data=mnldt,
                                     shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

mnlmodlistjt[['GENDINCOME']] <- mlogit(CHOICE~(COST+TRPDUR):GEND:INCOME|1, data=mnldt,
                                     shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['GENDTRPDIST']] <- mlogit(CHOICE~(COST+TRPDUR):GEND:TRPDIST_GRPD|1, data=mnldt,
                                       shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['GENDAGE']] <- mlogit(CHOICE~(COST+TRPDUR):GEND:AGE_GRPD|1, data=mnldt,
                                       shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['GENDTRPPUR']] <- mlogit(CHOICE~(COST+TRPDUR):GEND:TRPPUR|1, data=mnldt,
                                       shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
mnlmodlistjt[['GENDTRPTIME']] <- mlogit(CHOICE~(COST+TRPDUR):GEND:TRPTIME|1, data=mnldt,
                                       shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')


# mnlmodlist[['INCDIST']] <- mnlogit(CHOICE~(COST+TRPDUR):INCOME:TRPDIST_GRPD|1, data=mnldt, shape='long', choiceVar='MODE')
# mnlmodlist[['INCAGE']] <- mnlogit(CHOICE~(COST+TRPDUR):INCOME:AGE_GRPD|1, data=mnldt, shape='long', choiceVar='MODE')
# mnlmodlist[['DISTAGE']] <- mnlogit(CHOICE~(COST+TRPDUR):AGE_GRPD:TRPDIST_GRPD|1, data=mnldt, shape='long', choiceVar='MODE')
# summary(mnlmodlistjt$INCDIST)
# summary(mnlmodlistjt$INCAGE)
# summary(mnlmodlistjt$DISTAGE)

#### Separate models ####
#by mode
modemod <- lapply(unique(mnldt$MODE), function(m) {
  print(m)
  mnldt[ , RESPONSE := F]
  mnldt[MODE==m & CHOICE==T, RESPONSE := T]
  mod <- glm(RESPONSE ~ COST+TRPDUR, data = mnldt, family = 'binomial')
  #Extracting coefficients
  votmod <- as.data.table(summary(mod)$coefficients,keep.rownames = T)[grepl("COST|TRPDUR", rn), ]
  #cleanup the names
  setnames(votmod, c('rn','Estimate','Std. Error','z value','Pr(>|z|)'), c('VAR','BETA','SE','z-value','p-value'))
  #Recasting to wide, manually
  cost <- votmod[VAR=='COST',-1]
  time <- votmod[VAR=='TRPDUR',-1]
  #names
  names(cost) <- paste(names(cost),"COST",sep="_")
  names(time) <- paste(names(time),"TRPDUR",sep="_")
  vot <- data.table(VAR=m,time,cost,R2 = with(summary(mod), 1-(deviance/null.deviance)))

  #Calculating VOT
  vot[ , VOT := 60*BETA_TRPDUR/BETA_COST]
  vot[ , ERR_HI := with(vot, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['max',]]
  vot[ , ERR_LO := with(vot, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['min',]]
  vot[ , VARGROUP := 'MODE']
  vot[ , N := mnldt[CHOICE==T & MODE==m, .N]]
  vot[ , LABEL := as.character()]
  vot[ , VAL := as.numeric()]
  return(vot)
})
modemod <- rbindlist(modemod)

#Null model for calculating McFadden R2 1 - logLik(mod) / logLik(nullmod)
nullmod <- summary(mlogit(CHOICE~1, data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE'))
#By mode
mnlmodlist[['MODE']] <- mlogit(CHOICE~0|0|(COST+TRPDUR),
                               data=mnldt[ ,.(CHOICE,MODE,COST=-1*COST,TRPDUR)],
                               shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By income
mnlmodlist[['INCOME']] <- mlogit(CHOICE~(COST+TRPDUR):INCOME|1,
                                 data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By purpose
mnlmodlist[['TRPPUR']] <- mlogit(CHOICE~(COST+TRPDUR):TRPPUR|1,
                                 data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By age
mnlmodlist[['AGE_GRPD']] <- mlogit(CHOICE~(COST+TRPDUR):AGE_GRPD|1,
                                   data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By dist
mnlmodlist[['TRPDIST_GRPD']] <- mlogit(CHOICE~(COST+TRPDUR):TRPDIST_GRPD|1, 
                                       data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By TOD
mnlmodlist[['TRPTIME']] <- mlogit(CHOICE~(COST+TRPDUR):TRPTIME|1, data=mnldt,
                                  shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

#By Gender
mnlmodlist[['GEND']] <- mlogit(CHOICE~(COST+TRPDUR):GEND|1, data=mnldt,
                                  shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

# #By duration
# mnlmodlist[['TRPDUR_GRPD']] <- mlogit(CHOICE~(COST+TRPDUR):TRPDUR_GRPD|1,
#                                       data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')

# summary(mnlmodlist$TRPDUR_GRPD) 
# summary(mnlmodlist$MODE)
# summary(mnlmodlist$INCOME)
# summary(mnlmodlist$TRPPUR)
# summary(mnlmodlist$AGE_GRPD)
# summary(mnlmodlist$TRPDIST_GRPD)
# summary(mnlmodlist$TRPTIME)   

# #By gender
# mnlmodlist[['GEND']] <- mlogit(CHOICE~(COST+TRPDUR):GEND|1, data=mnldt, shape='long', alt.var='MODE', choice='CHOICE', reflevel='DRIVE')
# summary(mnlmodlist$GEND)   

#### Formatting VOT output results ####

### Reshaping average VOT model
votavg <- summary(mnlavgmod)$CoefTable
votavg <- as.data.table(t(votavg),keep.rownames = T)
votavg <- votavg[,grepl("COST|TRPDUR|rn", colnames(votavg)),with=F]
#Renaming
votavg[rn=='Pr(>|z|)', rn := 'p-value']
#Calculating VOT and adding other useful things
votavg[rn=='Estimate', VOT := 60*TRPDUR/COST]
votavg[rn=='Estimate', ERR_HI := fun.voterr(votavg[rn=='Estimate',TRPDUR],votavg[rn=='Std. Error',TRPDUR],
                               votavg[rn=='Std. Error',COST],votavg[rn=='Estimate',COST])['max']]
votavg[rn=='Estimate', ERR_LO := fun.voterr(votavg[rn=='Estimate',TRPDUR],votavg[rn=='Std. Error',TRPDUR],
                               votavg[rn=='Std. Error',COST],votavg[rn=='Estimate',COST])['min']]

### Reshaping joint models
votmodlistjt <- lapply(names(mnlmodlistjt), function(varname) {
  #print(varname)
  #Extracting coefficients & merging standard errors
  votmodjt <- as.data.table(summary(mnlmodlistjt[[varname]])$CoefTable,keep.rownames = T)[grepl("COST|TRPDUR", rn), ]
  #cleanup the names
  votmodjt[grepl('^COST:|:COST$',rn), VOTVAR := "COST"]
  votmodjt[grepl('^TRPDUR:|:TRPDUR$',rn), VOTVAR := "TRPDUR"]
  votmodjt[ , rn := gsub(paste("^COST:|:COST$|^TRPDUR:|:TRPDUR$",'',sep="|"),"",rn)]
  votmodjt[ , rn := gsub(paste("TRPDIST_GRPD|AGE_GRPD|TRPPUR|TRPTIME|GEND|INCOME",'',sep="|"),"",rn)]
  #votmodjt[ , rn := gsub("INCOMEINCOME","INCOME",rn)]
  
  setnames(votmodjt, c('rn','Estimate','Std. Error','z-value','Pr(>|z|)'), c('VAR','BETA','SE','z-value','p-value'))
  #splitting columns
  nvar = (1+str_count(votmodjt$VAR[1],":"))
  votmodjt <- cbind(votmodjt, str_split_fixed(votmodjt$VAR,":",nvar))
  #Recasing to wide
  votmodjt <- dcast(votmodjt, V1+V2~VOTVAR, value.var = c('BETA','SE','z-value','p-value'))
  #Calculating VOT and adding other useful things
  votmodjt[ , VOT := 60*BETA_TRPDUR/BETA_COST]
  votmodjt[ , ERR_HI := with(votmodjt, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['max',]]
  votmodjt[ , ERR_LO := with(votmodjt, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['min',]]
  votmodjt[ , VARGROUP := varname]
  
  #Organizing joint names... ugh this was bad
  if(varname == "GENDINCOME") vars = c("GEND","INCOME")
  if(varname == "GENDTRPDIST") vars = c("GEND","TRPDIST_GRPD")
  if(varname == "GENDAGE") vars = c("GEND","AGE_GRPD")
  if(varname == "GENDTRPPUR") vars = c("GEND","TRPPUR")
  if(varname == "GENDTRPTIME") vars = c("GEND","TRPTIME")
  if(varname == "INCDIST") vars = c("INCOME","TRPDIST_GRPD")
  if(varname == "INCAGE") vars = c("INCOME","AGE_GRPD")
  if(varname == "INCPURP") vars = c("INCOME","TRPPUR")
  if(varname == "INCTIME") vars = c("INCOME","TRPTIME")
  if(varname == "DISTAGE") vars = c("TRPDIST_GRPD","AGE_GRPD")
  if(varname == "DISTPURP") vars = c("TRPDIST_GRPD","TRPPUR")
  if(varname == "DISTTIME") vars = c("TRPDIST_GRPD","TRPTIME")
  if(varname == "AGEPURP") vars = c("AGE_GRPD","TRPPUR" )
  if(varname == "AGETIME") vars = c("AGE_GRPD","TRPTIME")
  if(varname == "PURPTIME") vars = c("TRPPUR","TRPTIME")
  
  #Frequencies to be merged
  nfreq <- lapply(vars, function(x) mnldt[CHOICE==T,.N, by=x])
  names(nfreq) <- vars
  
  #Which column is each variable in, V1 or V2?
  for(var in vars){
    col = which(rbind(lapply(votmodjt[,!"VARGROUP"], function(x) any(grepl(paste(varlabelvals[[var]]$VAR, collapse = "|"), x))))==T)
    col = colnames(votmodjt)[col]
    votmodjt <- merge(votmodjt, nfreq[[var]], by.x=col, by.y=var, all = T)
    votmodjt <- merge(votmodjt, varlabelvals[[var]], by.x = col, by.y = "VAR", all = T)
    setnames(votmodjt, c('LABEL','VAL','N'), paste(col,c('LABEL','VAL','N'),sep="_"))
  }
  
  #R squared
  votmodjt[ , R2 := as.numeric(summary(mnlmodlistjt[[varname]])$mfR2)]
  #done
  return(votmodjt)
})
names(votmodlistjt) <- names(mnlmodlistjt)
votmodjt <- rbindlist(votmodlistjt,use.names = T)



### Reshaping full joint model results with intercept
votmodlistjtintercept <- lapply(names(mnlmodlistjt), function(varname) {
  #Extracting coefficients & merging standard errors
  votmodjt <- as.data.table(summary(mnlmodlistjt[[varname]])$CoefTable,keep.rownames = T)#[grepl("COST|TRPDUR", rn), ]
  #cleanup the names
  votmodjt[grepl('^COST:|:COST$',rn), VOTVAR := "COST"]
  votmodjt[grepl('^TRPDUR:|:TRPDUR$',rn), VOTVAR := "TRPDUR"]
  votmodjt[ , rn := gsub(paste("^COST:|:COST$|^TRPDUR:|:TRPDUR$",'',sep="|"),"",rn)]
  votmodjt[ , rn := gsub(paste("TRPDIST_GRPD|AGE_GRPD|TRPPUR|TRPTIME|GEND|INCOME",'',sep="|"),"",rn)]
  #votmodjt[ , rn := gsub("INCOMEINCOME","INCOME",rn)]
  
  setnames(votmodjt, c('rn','Estimate','Std. Error','z-value','Pr(>|z|)'), c('VAR','BETA','SE','z-value','p-value'))
  #splitting columns
  nvar = (1+str_count(votmodjt$VAR[1],":"))
  votmodjt <- cbind(votmodjt, str_split_fixed(votmodjt$VAR,":",nvar))
  #Putting intercept into COST arbitrarily
  votmodjt[grepl("intercept",VAR), VOTVAR := "COST"]
  #Recasing to wide
  votmodjt <- dcast(votmodjt, V1+V2~VOTVAR, value.var = c('BETA','SE','z-value','p-value'))
  #Calculating VOT and adding other useful things
  votmodjt[ , VOT := 60*BETA_TRPDUR/BETA_COST]
  votmodjt[ , ERR_HI := with(votmodjt, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['max',]]
  votmodjt[ , ERR_LO := with(votmodjt, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['min',]]
  votmodjt[ , VARGROUP := varname]
  
  #Organizing joint names... ugh this was bad
  if(varname == "GENDINCOME") vars = c("GEND","INCOME")
  if(varname == "GENDTRPDIST") vars = c("GEND","TRPDIST_GRPD")
  if(varname == "GENDAGE") vars = c("GEND","AGE_GRPD")
  if(varname == "GENDTRPPUR") vars = c("GEND","TRPPUR")
  if(varname == "GENDTRPTIME") vars = c("GEND","TRPTIME")
  if(varname == "INCDIST") vars = c("INCOME","TRPDIST_GRPD")
  if(varname == "INCAGE") vars = c("INCOME","AGE_GRPD")
  if(varname == "INCPURP") vars = c("INCOME","TRPPUR")
  if(varname == "INCTIME") vars = c("INCOME","TRPTIME")
  if(varname == "DISTAGE") vars = c("TRPDIST_GRPD","AGE_GRPD")
  if(varname == "DISTPURP") vars = c("TRPDIST_GRPD","TRPPUR")
  if(varname == "DISTTIME") vars = c("TRPDIST_GRPD","TRPTIME")
  if(varname == "AGEPURP") vars = c("AGE_GRPD","TRPPUR" )
  if(varname == "AGETIME") vars = c("AGE_GRPD","TRPTIME")
  if(varname == "PURPTIME") vars = c("TRPPUR","TRPTIME")
  
  #Frequencies to be merged
  nfreq <- lapply(vars, function(x) mnldt[CHOICE==T,.N, by=x])
  names(nfreq) <- vars
  
  
  #Which column is each variable in, V1 or V2?
  for(var in vars){
    col = which(rbind(lapply(votmodjt[,!"VARGROUP"], function(x) any(grepl(paste(varlabelvals[[var]]$VAR, collapse = "|"), x))))==T)
    col = colnames(votmodjt)[col]
    votmodjt <- merge(votmodjt, nfreq[[var]], by.x=col, by.y=var, all = T)
    votmodjt <- merge(votmodjt, varlabelvals[[var]], by.x = col, by.y = "VAR", all = T)
    setnames(votmodjt, c('LABEL','VAL','N'), paste(col,c('LABEL','VAL','N'),sep="_"))
  }
  
  #R squared
  votmodjt[ , R2 := as.numeric(summary(mnlmodlistjt[[varname]])$mfR2)]
  #Order rows
  votmodjt <- votmodjt[order(V2),]
  
  #done
  return(votmodjt)
})
names(votmodlistjtintercept) <- names(votmodlistjtintercept)
votmodjtintercept <- rbindlist(votmodlistjtintercept,use.names = T)

### Reshaping individual models
votmodlist <- lapply(names(mnlmodlist), function(varname) {
  print(paste("Calculating and formatting",varname))
  mnlmod <- mnlmodlist[[varname]]
  #Extracting coefficients
  votmod <- as.data.table(summary(mnlmod)$CoefTable,keep.rownames = T)[grepl("COST|TRPDUR", rn), ]
  #cleanup the names
  votmod[grepl('^COST:|:COST$',rn), VOTVAR := "COST"]
  votmod[grepl('^TRPDUR:|:TRPDUR$',rn), VOTVAR := "TRPDUR"]
  votmod[ , rn := gsub(paste("COST|TRPDUR|:",varname,sep="|"),"",rn)]
  setnames(votmod, c('rn','Estimate','Std. Error','z-value','Pr(>|z|)'), c('VAR','BETA','SE','z-value','p-value'))
  #Recasing to wide
  votmod <- dcast(votmod, VAR~VOTVAR, value.var = c('BETA','SE','z-value','p-value'))
  #Calculating VOT and adding other useful things
  votmod[ , VOT := 60*BETA_TRPDUR/BETA_COST]
  votmod[ , ERR_HI := with(votmod, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['max',]]
  votmod[ , ERR_LO := with(votmod, mapply(fun.voterr, BETA_TRPDUR, SE_TRPDUR, BETA_COST, SE_COST))['min',]]
  votmod[ , VARGROUP := varname]
  votmod <- merge(votmod, mnldt[CHOICE==T,.N, by=varname], by.='VAR', by.y=varname, all = T)
  votmod[ , LABEL := as.character()]
  votmod[ , VAL := as.numeric()]
  
  if(is.null(summary(mnlmod)$mfR2))
    votmod[ , R2 := as.numeric(1-logLik(mnlmod) / logLik(nullmod))]
  else 
    votmod[ , R2 := as.numeric(summary(mnlmod)$mfR2)]
  
  #Sorting names to match mode data
  votmod <- votmod[ , colnames(modemod), with=F]
  
  return(votmod)
})
names(votmodlist) <- names(mnlmodlist)

#Adding labels to specific data
labs = names(votmodlist)[names(votmodlist) %in% names(varlabelvals)]
votmodlist[labs] <- lapply(labs, function(n) {
  return(merge(votmodlist[[n]][,-c("LABEL","VAL")], varlabelvals[[n]], by = 'VAR'))
})
rm(labs)

#Binding
votmoddt <- rbindlist(votmodlist)
votmoddt

#### Savings ####
#Cleanup
save(mnldt, votavg, votmoddt, votmodjt, votmodjtintercept, votmodlist, votmodlistjt, 
     mnlavgmod, mnlfullmod, mnlmodlist, mnlmodlistjt, varlabelvals,
     file = "dat_estimate.RData")

rm(list=ls())
# rm(list=setdiff(ls(), c('mnldt','votmoddt','votmodjt','votmodlist','votmodlistjt','votmodjtint',
#                         'mnlfullmod','mnlmodlist','mnlmodlistjt','varlabelvals')))



