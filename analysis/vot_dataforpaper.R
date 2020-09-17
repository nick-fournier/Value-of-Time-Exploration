
#### LOAD DATA AND LIBRARIES ####
load("dat_estimate.RData")

library(knitr)
library(kableExtra)
library(ggplot2)
library(extrafont)
library(data.table)
library(stringr)
library(mlogit)
library(tools)

windowsFonts(Times=windowsFont("TT Times New Roman"))

#### FUNCTIONS ####
#Significance notation
fun.signif <- function(x) {
  sig = rep("", length(x))
  sig[x < 0.05] <- "*"
  sig[x < 0.01] <- "**"
  sig[x < 0.001] <- "***"
  sig[x >= 0.05] <- ""
  sig[x == " "] <- ""
  return(as.character(sig))
}
#formatting of significance notation
fun.formatsignif <- function(x) {
  paste(ifelse(as.numeric(x)<2.2e-16,"$<$ 2.2e-16",formatC(x,digits=1,format="e")), fun.signif(x))
}
#cbind with NAs
cbind.na <- function (..., deparse.level = 1) {
  na <- nargs() - (!missing(deparse.level))    
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)   
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }
  if (na == 0) 
    return(NULL)
  if (na == 1) {         
    if (isS4(..1)) 
      return(cbind2(..1))
    else return(matrix(...))  ##.Internal(cbind(deparse.level, ...)))
  }
  if (deparse.level) {       
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == "") {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 
            2) 
          deparse(r)
      }
      else r
    }
  }   
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0) {
    r <- argl[[2]]
    fix.na <- FALSE
  }
  else {
    nrs <- unname(lapply(argl, nrow))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- cbind(rep(argl[[na]], length.out = nr), 
    #        deparse.level = 0)
    #}       
    if (deparse.level) {
      if (fix.na) 
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl))) 
        iV <- iV & (nmi == "")
      ii <- if (fix.na) 
        2:(na - 1)
      else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]]) if (!is.null(nmi <- Nms(i))) 
          names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring nrows
    nRow <- as.numeric(sapply(argl, function(x) NROW(x)))
    maxRow <- max(nRow, na.rm = TRUE)  
    argl <- lapply(argl, function(x)  if (is.null(nrow(x))) c(x, rep(NA, maxRow - length(x)))
                   else rbind.na(x, matrix(, maxRow - nrow(x), ncol(x))))
    r <- do.call(cbind, c(argl[-1L], list(deparse.level = deparse.level)))
  }
  d2 <- dim(r)
  r <- cbind2(argl[[1]], r)
  if (deparse.level == 0) 
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2) 
    return(r)
  Ncol <- function(x) {
    d <- dim(x)
    if (length(d) == 2L) 
      d[2L]
    else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Ncol(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Ncol(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(colnames(r))) 
      colnames(r) <- rep.int("", ncol(r))
    setN <- function(i, nams) colnames(r)[i] <<- if (is.null(nams)) 
      ""
    else nams
    if (nn1) 
      setN(1, N1)
    if (nn2) 
      setN(1 + l1, N2)
    if (fix.na) 
      setN(ncol(r), Nna)
  }
  r
}
#rbind with NAs
rbind.na <- function (..., deparse.level = 1) {
  na <- nargs() - (!missing(deparse.level))
  deparse.level <- as.integer(deparse.level)
  stopifnot(0 <= deparse.level, deparse.level <= 2)
  argl <- list(...)
  while (na > 0 && is.null(argl[[na]])) {
    argl <- argl[-na]
    na <- na - 1
  }    
  if (na == 0) 
    return(NULL)
  if (na == 1) {
    if (isS4(..1)) 
      return(rbind2(..1))
    else return(matrix(..., nrow = 1)) ##.Internal(rbind(deparse.level, ...)))
  }
  if (deparse.level) {
    symarg <- as.list(sys.call()[-1L])[1L:na]
    Nms <- function(i) {
      if (is.null(r <- names(symarg[i])) || r == "") {
        if (is.symbol(r <- symarg[[i]]) || deparse.level == 
            2) 
          deparse(r)
      }
      else r
    }
  }
  
  ## deactivated, otherwise no fill in with two arguments
  if (na == 0) {
    r <- argl[[2]]
    fix.na <- FALSE
  }
  else {
    nrs <- unname(lapply(argl, ncol))
    iV <- sapply(nrs, is.null)
    fix.na <- identical(nrs[(na - 1):na], list(NULL, NULL))
    ## deactivated, otherwise data will be recycled
    #if (fix.na) {
    #    nr <- max(if (all(iV)) sapply(argl, length) else unlist(nrs[!iV]))
    #    argl[[na]] <- rbind(rep(argl[[na]], length.out = nr), 
    #        deparse.level = 0)
    #}
    if (deparse.level) {
      if (fix.na) 
        fix.na <- !is.null(Nna <- Nms(na))
      if (!is.null(nmi <- names(argl))) 
        iV <- iV & (nmi == "")
      ii <- if (fix.na) 
        2:(na - 1)
      else 2:na
      if (any(iV[ii])) {
        for (i in ii[iV[ii]]) if (!is.null(nmi <- Nms(i))) 
          names(argl)[i] <- nmi
      }
    }
    
    ## filling with NA's to maximum occuring ncols
    nCol <- as.numeric(sapply(argl, function(x) if (is.null(ncol(x))) length(x)
                              else ncol(x)))
    maxCol <- max(nCol, na.rm = TRUE)  
    argl <- lapply(argl, function(x)  if (is.null(ncol(x))) c(x, rep(NA, maxCol - length(x)))
                   else cbind(x, matrix(, nrow(x), maxCol - ncol(x))))  
    
    ## create a common name vector from the
    ## column names of all 'argl' items
    namesVEC <- rep(NA, maxCol)  
    for (i in 1:length(argl)) {
      CN <- colnames(argl[[i]])          
      m <- !(CN %in% namesVEC)
      namesVEC[m] <- CN[m]          
    }  
    
    ## make all column names from common 'namesVEC'
    for (j in 1:length(argl)) {    
      if (!is.null(ncol(argl[[j]]))) colnames(argl[[j]]) <- namesVEC
    }
    
    r <- do.call(rbind, c(argl[-1L], list(deparse.level = deparse.level)))        
  }
  
  d2 <- dim(r)
  
  ## make all column names from common 'namesVEC'
  colnames(r) <- colnames(argl[[1]])
  
  r <- rbind2(argl[[1]], r)
  
  if (deparse.level == 0) 
    return(r)
  ism1 <- !is.null(d1 <- dim(..1)) && length(d1) == 2L
  ism2 <- !is.null(d2) && length(d2) == 2L && !fix.na
  if (ism1 && ism2) 
    return(r)
  Nrow <- function(x) {
    d <- dim(x)
    if (length(d) == 2L) 
      d[1L]
    else as.integer(length(x) > 0L)
  }
  nn1 <- !is.null(N1 <- if ((l1 <- Nrow(..1)) && !ism1) Nms(1))
  nn2 <- !is.null(N2 <- if (na == 2 && Nrow(..2) && !ism2) Nms(2))
  if (nn1 || nn2 || fix.na) {
    if (is.null(rownames(r))) 
      rownames(r) <- rep.int("", nrow(r))
    setN <- function(i, nams) rownames(r)[i] <<- if (is.null(nams)) 
      ""
    else nams
    if (nn1) 
      setN(1, N1)
    if (nn2) 
      setN(1 + l1, N2)
    if (fix.na) 
      setN(nrow(r), Nna)
  }
  r
}
#Non-linear r squared
nls.rsquared <- function (mdl, y, param) {
  adj <- (sum(!is.na(y)) - 1)/(sum(!is.na(y)) - param)
  sum.sq <- (sum(!is.na(y)) - 1) * var(y, na.rm = TRUE)
  rsq <- 1 - (adj * (deviance(mdl)/sum.sq))
  return(rsq)
}

#page width
tscale = 6.5
#Writing functions
#fun.norm <- function(x,modcoefs) as.vector(modcoefs['a']+modcoefs['b']*dnorm(x, mean=modcoefs['mu'], sd=modcoefs['sd']))
fun.norm <- function(x,modcoefs) as.vector(modcoefs['b']*dnorm(x, mean=modcoefs['mu'], sd=modcoefs['sd']))
fun.linear <- function(x, modcoefs) as.vector(modcoefs[1] + modcoefs[2]*x)
fun.loglin <- function(x, modcoefs) as.vector(exp(modcoefs[1] + modcoefs[2]*x))
fun.exp <- function(x, modcoefs) as.vector(modcoefs['a'] + modcoefs['b']*exp(modcoefs['c']*x))


#### GENERAL FORMATTING ####
#Fitting functions
fit_inc <- lm(log(VOT)~VAL, weights = 1/(ERR_HI-ERR_LO)^2, data=votmodlist[['INCOME']])
fit_dist <- lm(log(VOT)~VAL, data=votmodlist[['TRPDIST_GRPD']])
# fit_age <- nls(VOT~a + b/(sd*sqrt(2*pi))*exp(-0.5*((VAL-mu)/sd)^2), start=c(a=15, b=2000, mu=50, sd=25),
#                weights = 1/(ERR_HI-ERR_LO)^2, data = votmodlist[['AGE_GRPD']])
fit_age <- nls(VOT~b/(sd*sqrt(2*pi))*exp(-0.5*((VAL-mu)/sd)^2), start=c(b=3200, mu=50, sd=25),
               weights = 1/(ERR_HI-ERR_LO)^2, data = votmodlist[['AGE_GRPD']])
#Making combination grid
Nsegs = 100
votgrid <- as.data.table(expand.grid(
  AGE = seq(15,75,length.out = Nsegs),
  INCOME = seq(0,2e5,length.out = Nsegs),
  DIST = seq(0,15,length.out = Nsegs)
))

#Calculating VOT
votgrid[ , AGE.VOT := fun.norm(AGE,coef(fit_age))]
votgrid[ , INCOME.VOT := fun.loglin(INCOME,coef(fit_inc))]
votgrid[ , DIST.VOT := fun.loglin(DIST,coef(fit_dist))]

#Calculate mean
votgrid[ , COMBO.VOT := (AGE.VOT+INCOME.VOT+DIST.VOT)/3]
votgrid[ , AGE.INC.VOT := (AGE.VOT+INCOME.VOT)/2]
votgrid[ , AGE.DIST.VOT := (AGE.VOT+DIST.VOT)/2]
votgrid[ , DIST.INC.VOT := (DIST.VOT+INCOME.VOT)/2]


#Make sure things arent whacky
votmodlistjt <- lapply(votmodlistjt, function(x) {
  x[VOT>400, ]$VOT <- 400
  return(x)
})


#### TABLES
tables <- list()

#### TABLE OF CATEGORICAL DATA ####
  data.summary <- do.call("cbind.na", lapply(c("MODE","INCOME","TRPPUR","GEND"), function(n) {
    out <- merge(varlabelvals[[n]], mnldt[CHOICE==T,.N, by=n], by.x = 'VAR', by.y = n)
    out <- out[, .(LABEL,N)]
    out[ , N:= formatC(N, big.mark=",", digits=0)]
    return(out)
  }))
  
  #Renaming
  colnames(data.summary)[grepl("LABEL", colnames(data.summary))] <- "Bin"
  
  #Formatting NAs
  data.summary <- data.summary[ , lapply(.SD, function(x) ifelse(is.na(x),"",x))]
  
  tables$data.summary <- kable(data.summary, "latex", booktabs = T, escape = F, linesep = "") %>%
    add_header_above(c("Mode" = 2, "Income" = 2, "Purpose" = 2, "Gender" = 2)) %>%
    kable_styling(position = "left", font_size=9) #%>%
    #footnote(symbol = "Numbers reflect only reported trips in MTS, not the imputed alternatives")

#### TABLE OF BINNED DATA ####
  #Combining categorical labels
  data.binned <- do.call("cbind.na", lapply(c("AGE_GRPD","TRPDIST_GRPD","TRPTIME"), function(n) {
    out <- merge(varlabelvals[[n]], mnldt[CHOICE==T,.N, by=n], by.x = 'VAR', by.y = n)
    out <- out[, .(LABEL,VAL,N)]
    out[ , N := formatC(N, big.mark=",", digits=0)]
    out[ , VAL := round(VAL,2)]
    if(n=='TRPTIME') {
      out[ , VAL:=NULL]
      out <- out[sapply(c("AM (06:00-9:59)", "Midday (10:00-15:59)", "PM (16:00-18:59)", "Night (19:00-5:59)"),
                        function(x) which(x == LABEL)), ]
    }
    return(out)
  }))
  
  #Renaming
  colnames(data.binned)[grepl("LABEL", colnames(data.binned))] <- "Bin"
  #colnames(data.binned)[grepl("LABEL", colnames(data.binned))] <- c("Age","Distance","Travel-time","Time-of-day")
  colnames(data.binned)[grepl("LABEL", colnames(data.binned))] <- c("Age","Distance","Time-of-day")
  colnames(data.binned)[grepl("VAL", colnames(data.binned))] <- "Median"
  
  #Formatting NAs
  data.binned <- data.binned[ , lapply(.SD, function(x) ifelse(is.na(x),"",x))]
  
  tables$data.binned <- kable(data.binned, "latex", booktabs = T, escape = F, linesep = "") %>%
    #add_header_above(c("Age" = 3, "Distance" = 3, "Travel time" = 3, "Time-of-day" = 2)) %>%
    add_header_above(c("Age (years)" = 3, "Distance (miles)" = 3, "Time-of-day" = 2)) %>%
    kable_styling(position = "left", font_size=9) #%>%
    #footnote(symbol = "Numbers reflect only reported trips in MTS, not the imputed alternatives")


#### TABLE OF JOINT MODEL ERRORS ####
#Summarizing results
jtstats <- rbindlist(lapply(unique(votmodjt$VARGROUP), function(x) {
  dat <- merge(data.table('Model'=x, t(unclass(votmodjt[VARGROUP==x, summary(ERR_HI-ERR_LO)]))),
               unique(votmodjt[ , R2, by=VARGROUP]), by.x = 'Model', by.y = 'VARGROUP')
  
  dat <- cbind(dat, "signif"=as.data.table(summary(mnlmodlistjt[[x]])$CoefTable)[ , paste(sum(`Pr(>|z|)`<0.05), "of", .N)])
  return(dat)
}))
  
#Renaming
colnames(jtstats) <- c('MODELNAME','Minimum','First Quantile','Median','Mean','Third Quantile','Maximum',
                       'McFadden $R^2$',"Significant variables")

combos <-data.table(MODELNAME = c("INCAGE","INCDIST","DISTAGE","INCPURP","INCTIME",
                     "DISTPURP","DISTTIME","AGEPURP","AGETIME","PURPTIME",
                     "GENDINCOME","GENDTRPDIST","GENDAGE","GENDTRPPUR","GENDTRPTIME"),
           Model = c('Income$\\times$Age','Income$\\times$Distance','Distance$\\times$Age','Income$\\times$Trip Purpose',
                     'Income$\\times$Time of day','Distance$\\times$Trip purpose','Distance$\\times$Time of day',
                     'Age$\\times$Trip purpose','Age$\\times$Time of day','Trip purpose$\\times$Time of day',
                     "Gender$\\times$Income","Gender$\\times$Distance","Gender$\\times$Age",
                     "Gender$\\times$Trip purpose","Gender$\\times$Time of day"))

jtstats <- merge(combos,jtstats, by = "MODELNAME")[,!"MODELNAME"]

#Rounding
cols <- c("Minimum","First Quantile","Median","Mean","Third Quantile","Maximum","McFadden $R^2$")
jtstats[ , (cols) := lapply(.SD, function(x) round(x,2)), .SDcols = cols]

#output
tables$jointerrors <- kable(jtstats, "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" " = 1, "Value of time error" = 6), escape=F) %>%
  column_spec(3, width = "0.5in") %>%
  column_spec(6, width = "0.5in") %>%
  column_spec(8, width = "0.5in") %>%
  column_spec(9, width = "0.5in") %>%
  kable_styling(position = "left", font_size=8)

#### TABLE OF FULL JOINT MODEL RESULTS ####
#Summarizing results
jtregres <- votmodjtintercept#[,!c("ERR_LO","ERR_HI","VOT")]

#pretty names
for(var in names(varlabelvals)) {
  for(i in 1:nrow(varlabelvals[[var]])) {
    # if(var == "INCOME") { 
    #   jtregres$V1 <- gsub(paste0("INCOME",varlabelvals[[var]]$VAR[i]), varlabelvals[[var]]$LABEL[i], jtregres$V1, fixed = T)
    #   jtregres$V2 <- gsub(paste0("INCOME",varlabelvals[[var]]$VAR[i]), varlabelvals[[var]]$LABEL[i], jtregres$V2, fixed = T)
    # }
    # else { 
      jtregres$V1 <- gsub(varlabelvals[[var]]$VAR[i], varlabelvals[[var]]$LABEL[i], jtregres$V1, fixed = T)
      jtregres$V2 <- gsub(varlabelvals[[var]]$VAR[i], varlabelvals[[var]]$LABEL[i], jtregres$V2, fixed = T)
    # }
  }
}

#Relabel time
jtregres[ , V1 := gsub(" \\(.*","",V1)]
jtregres[ , V2 := gsub(" \\(.*","",V2)]

#Fix < error
jtregres[ , (c("V1","V2")) := lapply(.SD, function(x) gsub("<","$<$",x)), .SDcols = c("V1","V2")]

#Combine variables into one column
jtregres[!grepl("intercept",V2), Variables := paste0(V1,"$\\times$",V2)]
jtregres[grepl("intercept",V2), Variables := paste (V1,V2)]

#blanking all but first in group
jtregres[-jtregres[!duplicated(VARGROUP), .I, by = VARGROUP]$I, R2:=NA]

#Renaming
# combos <- data.table(VARGROUP = c("INCAGE","INCDIST","DISTAGE","INCPURP","INCTIME",
#                                   "DISTPURP","DISTTIME","AGEPURP","AGETIME","PURPTIME"),
#                     Model = c('Income$\\\\times$Age','Income$\\\\times$Distance','Distance$\\\\times$Age','Income$\\\\times$Trip Purpose',
#                               'Income$\\\\times$Time of day','Distance$\\\\times$Trip purpose','Distance$\\\\times$Time of day',
#                               'Age$\\\\times$Trip purpose','Age$\\\\times$Time of day','Trip purpose$\\\\times$Time of day'))
combos <-data.table(VARGROUP = c("INCAGE","INCDIST","DISTAGE","INCPURP","INCTIME",
                                  "DISTPURP","DISTTIME","AGEPURP","AGETIME","PURPTIME",
                                  "GENDINCOME","GENDTRPDIST","GENDAGE","GENDTRPPUR","GENDTRPTIME"),
                    Model = c('Income$\\times$Age','Income$\\times$Distance','Distance$\\times$Age','Income$\\times$Trip Purpose',
                              'Income$\\times$Time of day','Distance$\\times$Trip purpose','Distance$\\times$Time of day',
                              'Age$\\times$Trip purpose','Age$\\times$Time of day','Trip purpose$\\times$Time of day',
                              "Gender$\\times$Income","Gender$\\times$Distance","Gender$\\times$Age",
                              "Gender$\\times$Trip purpose","Gender$\\times$Time of day"))

jtregres <- merge(combos,jtregres, by = "VARGROUP")[,!"VARGROUP"]

#Rounding
cols <- c("BETA_COST","BETA_TRPDUR","SE_COST","SE_TRPDUR","z-value_COST","z-value_TRPDUR","VOT","ERR_HI","ERR_LO","R2")
jtregres[ , (cols) := lapply(.SD, function(x) round(x,2)), .SDcols = cols]

#Formatting
jtregres[ , `p-value_TRPDUR` := fun.formatsignif(`p-value_TRPDUR`)]
jtregres[ , `p-value_COST` := fun.formatsignif(`p-value_COST`)]

#No NA
jtregres[ , (colnames(jtregres)) := lapply(.SD, function(x) gsub("NA","",x)), .SDcols = colnames(jtregres)]
jtregres[ , (colnames(jtregres)) := lapply(.SD, function(x) ifelse(is.na(x),"",x)), .SDcols = colnames(jtregres)]


#R^2 in packed title row
prows <- rbindlist(lapply(unique(jtregres$Model), function(x) {
  data.table("rn" = x, t(range(jtregres[Model == x, which=T])), "R2"=jtregres[Model == x & R2 != "", R2])
}))
prows[ , rn := paste0(rn, ", McFadden $R^2 = ",R2,"$")]
prows[, rn := gsub("\\\\","\\\\\\\\",rn)]

#Combine error range
jtregres[ , "Error range" := paste(ERR_LO,"--",ERR_HI)]

#Col order
setnames(jtregres,
         c("Model","Variables","BETA_COST","BETA_TRPDUR",
           "SE_COST","SE_TRPDUR",#"z-value_COST","z-value_TRPDUR",
           "p-value_COST","p-value_TRPDUR","VOT","ERR_HI","ERR_LO","R2"),
         c("Model","Variables","$\\beta_C$","$\\beta_T$",
           "SE$_C$","SE$_T$",#"$z-value_C$","$z-value_T$",
           "p-value$_C$","p-value$_T$","VOT","Error$_{min}$","Error$_{max}$","$R^2$"))

#Ditch column
jtregres <- jtregres[ , c("Model","Variables","$\\beta_C$","SE$_C$","p-value$_C$",
                          "$\\beta_T$","SE$_T$","p-value$_T$"), with=F]

#output
tables$jtregresults <- kable(jtregres[,!"Model"], format="latex",
                             booktabs = T, escape = F, longtable = T,
                             caption = "Joint multinomial model results", linesep = "") %>%
  add_header_above(c(" " = 1, "Travel cost " = 3, "Travel time" = 3)) %>%
  pack_rows(prows[1,rn], prows[1,V1], prows[1,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[2,rn], prows[2,V1], prows[2,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[3,rn], prows[3,V1], prows[3,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[4,rn], prows[4,V1], prows[4,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[5,rn], prows[5,V1], prows[5,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[6,rn], prows[6,V1], prows[6,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[7,rn], prows[7,V1], prows[7,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[8,rn], prows[8,V1], prows[8,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[9,rn], prows[9,V1], prows[9,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[10,rn], prows[10,V1], prows[10,V2], hline_after = T, escape = F) %>%
  kable_styling(position = "center", font_size=8, latex_options = c("repeat_header")) %>%
  footnote(symbol = c("Significance level codes: '***' 99.99%, '**' 99%, '*' 95%"))


#### TABLE OF FULL MODEL REGRESSION RESULTS ####

#Extracting coefficients
fullmodtable <- as.data.table(summary(mnlfullmod)$CoefTable,keep.rownames = T)

#Splitting variables and putting mode into its own column
fullmodtable <- data.table(str_split_fixed(fullmodtable$rn,":",2), fullmodtable[,!"rn"])

#Cleaning up the variables a bit
for(x in 1:nrow(varlabelvals$MODE)) fullmodtable[ , V1 := gsub(varlabelvals$MODE[x,VAR],varlabelvals$MODE[x,LABEL],V1)]

#Adding nice labels
fullmodtable <- merge(fullmodtable, rbindlist(lapply(c("INCOME","TRPTIME","TRPPUR","AGE_GRPD"), function(x) {
  varlabelvals[[x]][,.(VAR=paste0(x,VAR),LABEL)]
})), by.x="V2", by.y="VAR", all = T)

#Main variable type
fullmodtable[grepl("INCOME",V2), LABEL := paste0("Income - ",LABEL)]
fullmodtable[grepl("AGE_GRPD",V2), LABEL := paste0("Age - ", LABEL)]
fullmodtable[grepl("AGE",V2), LABEL := "Age"]
fullmodtable[grepl("TRPPUR",V2), LABEL := paste0("Trip purpose - ",LABEL)]
fullmodtable[grepl("TRPDUR",V2), LABEL := "Trip duration"]
fullmodtable[grepl("GEND",V2), LABEL := "Gender"]
fullmodtable[grepl("COST",V2), LABEL := "Cost"]
fullmodtable[grepl("TRPTIME",V2), LABEL := paste0("Time of day - ",LABEL)]
fullmodtable[grepl("intercept",V2), LABEL := "Intercept"]

#Remove extras
fullmodtable <- fullmodtable[!is.na(V1),!"V2"]

#notation
cols <- c("Estimate","Std. Error","z-value")
fullmodtable[ , (cols) := lapply(.SD, function(x) formatC(x, digits=2, format="f")), .SDcols = cols]
fullmodtable[ , `Pr(>|z|)` := fun.formatsignif(`Pr(>|z|)`)]

#Setnames
setnames(fullmodtable, c('V1','LABEL','Estimate','Std. Error','z-value','Pr(>|z|)'),
         c('Mode','Variable','$\\beta$','SE','z-value','p-value'))


fullmodtable[ , Variable := gsub(" \\(.*","",Variable)]

#Add R2
r2 <- paste0("McFadden $R^2=", round(as.numeric(summary(mnlfullmod)$mfR2),2),"$")

#reorder cols and rows
fullmodtable <- fullmodtable[ , c('Mode','Variable','$\\beta$','SE','z-value','p-value'), with = F]
fullmodtable <- fullmodtable[Mode!='Drive',][order(Mode),]


#making wide
widefullmodtable <- fullmodtable[Mode != "Drive", !'z-value']
widefullmodtable <- cbind(widefullmodtable[Mode=='Taxi',!c("Mode")],
                          widefullmodtable[Mode=='Bus',!c("Mode","Variable")],
                          widefullmodtable[Mode=='Subway',!c("Mode","Variable")],
                          widefullmodtable[Mode=='Rail',!c("Mode","Variable")])

#wide table
tables$widefullmodtable <- kable(widefullmodtable,
                              "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" " = 1, "Taxi" = 3, "Bus" = 3, "Subway" = 3, "Rail" = 3), bold = T) %>%
  kable_styling(position = "left", font_size=8) %>%
  footnote(general = r2, 
           symbol = c("Significance level codes: '***' 99.99\\\\%, '**' 99\\\\%, '*' 95\\\\%"),
           general_title = "", escape = F)


#Stacking
widemodtable <- cbind(fullmodtable[Mode %in% c("Bus","Taxi"), ], fullmodtable[Mode %in% c("Rail","Subway"), !"Variable"])

#Half table
tables$fullmodtable1 <- kable(widemodtable[Mode=="Bus",!"Mode"],
                             "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" " = 1, "Bus" = 4, "Rail" = 4), bold = T) %>%
  kable_styling(position = "left", font_size=8)

#Half table
tables$fullmodtable2 <- kable(rbind(widemodtable[Mode=="Taxi",!"Mode"], data.table(t(c(r2, rep("",ncol(widemodtable)-3)))), use.names=F),
                             "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" " = 1, "Taxi" = 4, "Subway" = 4), bold = T) %>%
  row_spec(15, hline_after = T) %>%
  kable_styling(position = "left", font_size=8) %>%
  footnote(symbol = c("Significance level codes: '***' 99.99%, '**' 99%, '*' 95%"))


# #Generate LaTex table
# longfullmodtable <- rbind(fullmodtable, data.table(t(c("",r2, rep("",ncol(fullmodtable)-2)))), use.names=F)
# #Full table
# tables$fullmodtable <- kable(longfullmodtable[,!"Mode"], "latex", booktabs = T, escape = F, linesep = "") %>%
#   add_header_above(c(" " = 1, "Travel time" = 4, "Travel cost" = 4)) %>%
#   pack_rows("Bus", 1,15, hline_after = T) %>%
#   pack_rows("Rail",16,30, hline_after = T) %>%
#   pack_rows("Subway",31,45, hline_after = T) %>%
#   pack_rows("Taxi",46,60, hline_after = T) %>%
#   row_spec(60, hline_after = T) %>%
#   kable_styling(position = "left", font_size=8) %>%
#   footnote(symbol = c("Significance level codes: '***' 99.99%, '**' 99%, '*' 95%"))

rm(r2)

#### TABLE OF SEPARATE REGRESSION RESULTS ####
#Combining categorical labels
regtable <- votmoddt[ , .(VARGROUP,LABEL,VOT,BETA_TRPDUR,SE_TRPDUR,`z-value_TRPDUR`,`p-value_TRPDUR`,
                          BETA_COST,SE_COST,`z-value_COST`,`p-value_COST`,R2)]
#Formatting
regtable[ , VOT := round(VOT,1)]

cols <- colnames(regtable)[grepl("BETA|z-value|R2",colnames(regtable))]
regtable[ , (cols) := lapply(.SD, function(x) formatC(x,digits=2,format='f')), .SDcols = cols]

cols <- colnames(regtable)[grepl("SE",colnames(regtable))]
regtable[ , (cols) := lapply(.SD, function(x) formatC(x,digits=2,format='e')), .SDcols = cols]

regtable[ , `p-value_TRPDUR` := fun.formatsignif(`p-value_TRPDUR`)]
regtable[ , `p-value_COST` := fun.formatsignif(`p-value_COST`)]


#Renaming
combos <- data.table(VARGROUP = c("MODE","INCOME","TRPPUR","AGE_GRPD","TRPDIST_GRPD","TRPTIME","GEND"),
                     Model = c("Mode model","Income model","Trip purpose model","Age model",
                               "Travel distance model","Time-of-day model","Gender"))

regtable <- merge(combos,regtable, by = "VARGROUP")

#R^2 in packed title row
prows <- rbindlist(lapply(unique(regtable$Model), function(x) {
  data.table("rn" = x, t(range(regtable[Model == x, which=T])), "R2"=unique(regtable[Model == x, R2]))
}))
prows[ , rn := paste0(rn, ", $McFadden~R^2 = ",R2,"$")]

#Relabel time
ord = sapply(c("AM (06:00-9:59)","Midday (10:00-15:59)","PM (16:00-18:59)","Night (19:00-5:59)"),
             function(x) which(x == regtable[VARGROUP=='TRPTIME', LABEL]))

regtable[VARGROUP=='TRPTIME', ] <- regtable[VARGROUP=='TRPTIME', ][ord, ]
regtable[VARGROUP=='TRPTIME', LABEL := gsub(" \\(.*","",LABEL)]

#blanking all but first in group
regtable[-regtable[!duplicated(VARGROUP), .I, by = VARGROUP]$I, R2:=" "]
regtable[,VARGROUP:=NULL]

#Renaming
colnames(regtable)[grepl("LABEL", colnames(regtable))] <- "Variable"
colnames(regtable)[grepl("BETA", colnames(regtable))] <- "$\\beta$"
colnames(regtable)[grepl("SE", colnames(regtable))] <- "SE"
colnames(regtable)[grepl("z-value", colnames(regtable))] <- "z-value"
colnames(regtable)[grepl("p-value", colnames(regtable))] <- "p-value"
colnames(regtable)[grepl("R2", colnames(regtable))] <- "McFadden $R^2$"

tables$regresults <- kable(regtable[,!c("Model","McFadden $R^2$")],
                           "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c(" " = 2, "Travel time" = 4, "Travel cost" = 4)) %>%
  pack_rows(prows[1,rn], prows[1,V1], prows[1,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[2,rn], prows[2,V1], prows[2,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[3,rn], prows[3,V1], prows[3,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[4,rn], prows[4,V1], prows[4,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[5,rn], prows[5,V1], prows[5,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[6,rn], prows[6,V1], prows[6,V2], hline_after = T, escape = F) %>%
  pack_rows(prows[7,rn], prows[7,V1], prows[7,V2], hline_after = T, escape = F) %>%
  #column_spec(11, width = "0.5in") %>%
  kable_styling(position = "left", font_size=8) %>%
  footnote(symbol = c("Significance level codes: '***' 99.99%, '**' 99%, '*' 95%"))
#

#### TABLE OF PARAMETER FIT ####
#Combining categorical labels
paratable <- as.data.table(rbind(
  c(c(1,1e4)*coef(fit_inc),NA,NA,NA,summary(fit_inc)$r.squared),
  c(coef(fit_dist),NA, NA,NA,summary(fit_dist)$r.squared),
  c(NA,NA,coef(fit_age),nls.rsquared(fit_age, votmodlist[['AGE_GRPD']]$VOT,4))))

colnames(paratable) <- c('$\\alpha$','$\\beta$','$b$','$\\mu$','$\\sigma$','$R^2$')

#Formatting
paratable <- paratable[ , lapply(.SD, function(x) formatC(x,digits=3,format='f'))]
# paratable[ , (c('$\\alpha$','$\\beta$','$R^2$')) := lapply(.SD, function(x) formatC(x,digits=3,format='f')), .SDcols = c('$\\alpha$','$\\beta$','$R^2$')]
# paratable[ , (c('$b$','$\\mu$','$\\sigma$')) := lapply(.SD, function(x) formatC(x,digits=2,format='f')), .SDcols = c('$b$','$\\mu$','$\\sigma$')]
# 
paratable <- paratable[ , lapply(.SD, function(x) ifelse(is.na(x)|grepl('NA',x)," ",x))]
#Add labels
paratable <- cbind(data.table("Variable" = c('Income','Travel distance','Age'),
                              "Unit" = c('\\$10,000','Miles','Years'),
                              "Function" = c('$VOT(x)=e^{\\alpha+\\beta x}$',
                                             '$VOT(x)=e^{\\alpha+\\beta x}$',
                                             '$VOT(x)=\\frac{b}{\\sigma\\sqrt{2\\pi}}e^{-\\frac{(x-\\mu)^2}{2\\sigma^2}}$')),
                   paratable)

#Output
tables$parafit <- kable(paratable, "latex", booktabs = T, escape = F, linesep = "") %>%
  add_header_above(c("Model" = 3, "Parameters" = 5)) %>%
  kable_styling(position = "left", font_size=9)

#### ARRANGED JOINT PLOT ####
library(egg)
fnt <- "Times"
jointplots <- list()
fntsize = 7

#Income by age
jointplots[["INCAGE"]] <- ggplot(votmodlistjt[['INCAGE']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL,
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(AGE_GRPD))) +
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize), 
        text=element_text(family=fnt))
#Distance by age
jointplots[["DISTAGE"]] <- ggplot(votmodlistjt[['DISTAGE']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_y_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(AGE_GRPD))) +
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),
        text=element_text(family=fnt))
#Income by distance
jointplots[["INCDIST"]] <- ggplot(votmodlistjt[['INCDIST']][VOT>0,],
                                  aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPDIST_GRPD))) +
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt),)
#Income by purpose
jointplots[["INCPURP"]] <- ggplot(votmodlistjt[['INCPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(angle = 30, size=fntsize), 
        text=element_text(family=fnt)) 
#Income by time of day
jointplots[["INCTIME"]] <- ggplot(votmodlistjt[['INCTIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL,
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),
        text=element_text(family=fnt))
#Purpose by distance
jointplots[["PURPDIST"]] <- ggplot(votmodlistjt[['DISTPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),
        text=element_text(family=fnt))
#Distance by time of day
jointplots[["DISTTIME"]] <- ggplot(votmodlistjt[['DISTTIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +	
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),
        text=element_text(family=fnt))
#Age by purpose
jointplots[["AGEPURP"]] <- ggplot(votmodlistjt[['AGEPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_x_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +		
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(AGE_GRPD)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize), 
        text=element_text(family=fnt)) 
#Age by time of day
jointplots[["AGETIME"]] <- ggplot(votmodlistjt[['AGETIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_x_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(AGE_GRPD)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=fntsize),
        axis.text.y = element_text(size=fntsize),
        text=element_text(family=fnt))

#Purpose by time of day
jointplots[["PURPTIME"]] <- ggplot(votmodlistjt[['PURPTIME']][VOT>0,], aes(x=V2_LABEL,y=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_x_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPTIME)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

#Gender by income
jointplots[["GENDINCOME"]] <- ggplot(votmodlistjt[['GENDINCOME']][VOT>0,], aes(y=V2_LABEL,x=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Income", limits = varlabelvals$INCOME$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_x_discrete("Gender", limits = varlabelvals$GEND$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$GEND$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(GEND)/nrow(INCOME))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

#Gender by Distance
jointplots[["GENDTRPDIST"]] <- ggplot(votmodlistjt[['GENDTRPDIST']][VOT>0,], aes(y=V2_LABEL,x=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_x_discrete("Gender", limits = varlabelvals$GEND$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$GEND$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(GEND)/nrow(TRPDIST_GRPD))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

#Gender by Age
jointplots[["GENDAGE"]] <- ggplot(votmodlistjt[['GENDAGE']][VOT>0,], aes(y=V2_LABEL,x=V1_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_x_discrete("Gender", limits = varlabelvals$GEND$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$GEND$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(GEND)/nrow(AGE_GRPD))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

#Gender by trip purpse
jointplots[["GENDTRPPUR"]] <- ggplot(votmodlistjt[['GENDTRPPUR']][VOT>0,], aes(y=V2_LABEL,x=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_x_discrete("Gender", limits = varlabelvals$GEND$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$GEND$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(GEND)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

#Gender by time of day
jointplots[["GENDTRPTIME"]] <- ggplot(votmodlistjt[['GENDTRPTIME']][VOT>0,], aes(y=V2_LABEL,x=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_x_discrete("Gender", limits = varlabelvals$GEND$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$GEND$LABEL)) +
  scale_fill_distiller("", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(GEND)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        legend.background = element_blank(),
        axis.text.x = element_text(size=fntsize),
        axis.text.y = element_text(size=fntsize),  
        text=element_text(family=fnt))

legendplot <- ggplot(votmodlistjt[['GENDTRPTIME']][VOT>0,], aes(x=V2_LABEL,y=V1_LABEL,fill=VOT)) + 
  geom_tile() +
  scale_fill_distiller("VOT ($/hr)", palette = "BuGn", direction = 1, limits = c(0,450)) +
  theme(legend.position='bottom',
        legend.box = "horizontal",
        legend.background = element_blank(),
        text=element_text(family=fnt)) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))

#Gradient scale
#jointplots[["LEGEND"]]

jointplots[["LEGEND"]] <- cowplot::plot_grid(cowplot::get_legend(legendplot))


#All pl0ts in one
jointplot <- ggarrange(plots=jointplots, ncol = 3)

#### SAVING ####

#Joint plot
ggsave("../writing/figure/jointplot.pdf", jointplot, width = 7.5, height=10)

#Data
save(mnldt, votavg, votmoddt, votmodjt, votmodlist, votmodlistjt, votmodjtintercept,
     varlabelvals, fit_dist, fit_age, fit_inc,
     fun.exp, fun.linear, fun.loglin, fun.norm, nls.rsquared, Nsegs, jointplot,
     file = "../writing/localdata/dat_models.RData")
save(votgrid, file = "../writing/localdata/dat_votgrid.RData")
save(tables, file = "../writing/localdata/dat_tables.RData")

#Tables as CSV
writeLines(paste("\\documentclass{article}\\usepackage[utf8]{inputenc}
\\usepackage[letterpaper, margin=0.5in]{geometry}
\\usepackage{booktabs,longtable,pdflscape,float,tabularx}
\\begin{document}",
"\\begin{table}[H]\\centering\\caption{Categorical data from the Massachusetts Travel Survey}",
tables$data.summary,"\\end{table}",
"\\begin{table}[H]\\centering\\caption{Discrete bins of continuous data from the Massachusetts Travel Survey}",
tables$data.binned,"\\end{table}",
"\\begin{table}[H]\\centering\\setlength{\\tabcolsep}{2pt}\\caption{Multinomial model results}",
tables$widefullmodtable,"\\end{table}",
"\\begin{table}[H]\\centering\\setlength{\\tabcolsep}{4pt}\\caption{Summary statistics for joint VOT error}",
tables$jointerrors,"\\end{table}",
"\\begin{table}[H]\\centering\\caption{Summary of individual variable model estimation results}",
tables$regresults,"\\end{table}",
"\\begin{table}[H]\\centering\\caption{Summary of parametric fitting results}",
tables$parafit,"\\end{table}\\newpage",
tables$jtregresults,
"\\end{document}"), "restable_joint.tex")





