##
library(ggplot2)
library(data.table)
library(gridExtra)
library(extrafont)

#loading data
load("dat_estimatevot.RData")
load("dat_estimatemnlmodjt.RData")

#name the font in system
windowsFonts(Times=windowsFont("TT Times New Roman"))


#### Plotting ####
fun.norm <- function(x,modcoefs) as.vector(modcoefs['a']+modcoefs['b']*dnorm(x, mean=modcoefs['mu'], sd=modcoefs['sd']))
fun.linear <- function(x, modcoefs) as.vector(modcoefs[1] + modcoefs[2]*x)
fun.loglin <- function(x, modcoefs) as.vector(exp(modcoefs[1] + modcoefs[2]*x))
fun.exp <- function(x, modcoefs) as.vector(modcoefs['a'] + modcoefs['b']*exp(modcoefs['c']*x))

# fit_inc <- lm(VOT~VAL, weights = N, data=votmodlist[['INCOME']])
# fit_inc <- nls(VOT~a+b*exp(c*VAL), start=c(a=25, b=1, c=0.000025),
#                  weights = N, data=votmodlist[['INCOME']])
# fit_dist <- lm(VOT~VAL, weights = N, data=votmodlist[['TRPDIST_GRPD']])
# fit_dist <- nls(VOT~a+b*exp(c*VAL), start=c(a=1, b=1, c=0.5),
#                 weights = N, data=votmodlist[['TRPDIST_GRPD']])

# fit_age <- nls(VOT~a+b*dnorm(VAL, mu, sd), start=c(a=15, b=2000, mu=70, sd=25),
#               weights = 1/(ERR_HI-ERR_LO)^2, data = votmodlist[['AGE_GRPD']][VAR!='AGE2',])

fit_inc <- lm(log(VOT)~VAL, weights = 1/(ERR_HI-ERR_LO)^2, data=votmodlist[['INCOME']])
fit_dist <- lm(log(VOT)~VAL, data=votmodlist[['TRPDIST_GRPD']])
fit_age <- nls(VOT~a + b/(sd*sqrt(2*pi))*exp(-0.5*((VAL-mu)/sd)^2), start=c(a=15, b=2000, mu=50, sd=25),
               weights = 1/(ERR_HI-ERR_LO)^2, data = votmodlist[['AGE_GRPD']])


#VOT plots
ggplot(votmodlist[['INCOME']], aes(x=VAL,y=log(VOT))) + 
  geom_smooth(aes(weight=1/(ERR_HI-ERR_LO)^2), method='lm', se=F, color="gray50") + 
  geom_point(aes(size=N)) + 
  geom_errorbar(aes(ymin=log(ERR_LO), ymax=log(ERR_HI)), width=0.5*10000) + 
  scale_x_continuous("Income", labels = scales::comma) +
  scale_y_continuous("Logarithmic value of travel time ($/hr)", limits = c(2,5)) +
  #stat_function(fun=function(x) fun.loglin(x, coef(fit_inc))) +
  #stat_function(fun=function(x) fun.exp(x, coef(fit_inc))) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['TRPDIST_GRPD']], aes(x=VAL,y=VOT)) +
  #geom_smooth(aes(weight=N), method='lm', se=F, color="gray50") + 
  geom_point(aes(size=N)) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  scale_x_continuous("Trip distance (miles)") +
  #scale_y_continuous("Logarithmic value of travel time ($/hr)", limits = c(0,400)) +
  #stat_function(fun=function(x) fun.exp(x, coef(fit_dist))) +
  stat_function(fun=function(x) fun.loglin(x, coef(fit_dist))) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['AGE_GRPD']], aes(x=VAL,y=VOT)) + 
  geom_point(aes(size=N)) + geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  stat_function(fun=function(x) fun.norm(x, coef(fit_age))) +
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,80)) +
  scale_x_continuous("Age") +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['TRPDUR_GRPD']], aes(x=VAL,y=VOT)) +
  geom_smooth(method='loess', span=2, se=F, color="gray50") + 
  geom_point(aes(size=N)) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  scale_x_continuous("Trip duration (minutes)") +
  scale_y_continuous("Logarithmic value of travel time ($/hr)", limits = c(0,80)) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['MODE']], aes(x=VAR,y=VOT)) + 
  geom_point(aes(size=N)) + geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,20)) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['TRPPUR']], aes(x=VAR,y=VOT)) + 
  geom_point(aes(size=N)) + geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,50)) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['TRPTIME']], aes(x=VAR,y=VOT)) + 
  geom_point(aes(size=N)) + geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) +
  scale_x_discrete("Time of day", limits = c("AM","MD","PM","NT"), labels = c(AM="AM","MD","PM","NT")) +
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,100)) +
  theme_classic() + theme(text=element_text(family="Times"))

ggplot(votmodlist[['GEND']], aes(x=VAR,y=VOT)) + 
  geom_point(aes(size=N)) + geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI)) + 
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,60)) +
  theme_classic() + theme(text=element_text(family="Times"))



#Making combination grid
votgrid <- as.data.table(expand.grid(
  AGE = seq(0,75,length.out = 100),
  INCOME = seq(0,2e5,length.out = 100),
  DIST = seq(0,15,length.out = 100)
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

ggplot(unique(votgrid[,.(AGE,INCOME,AGE.INC.VOT)]), aes(x=AGE,y=INCOME,z=AGE.INC.VOT)) + 
  geom_tile(aes(fill=AGE.INC.VOT)) + geom_contour(color='gray') + 
  scale_fill_distiller(palette = "Spectral") + theme_bw() +
  theme(legend.position='bottom', text=element_text(family="Times"))

ggplot(unique(votgrid[,.(AGE,DIST,AGE.DIST.VOT)]), aes(x=AGE,y=DIST,z=AGE.DIST.VOT)) + 
  geom_tile(aes(fill=AGE.DIST.VOT)) + geom_contour(color='gray') +
  scale_fill_distiller(palette = "Spectral") + theme_bw() +
  theme(legend.position='bottom', text=element_text(family="Times"))

ggplot(unique(votgrid[,.(DIST,INCOME,DIST.INC.VOT)]), aes(x=INCOME,y=DIST,z=DIST.INC.VOT)) + 
  geom_tile(aes(fill=DIST.INC.VOT)) + geom_contour(color='gray') +
  scale_fill_distiller(palette = "Spectral") + theme_bw() +
  theme(legend.position='bottom', text=element_text(family="Times"))


#Distributions
#Distributions
ggplot(mnldt[CHOICE==T,], aes(x=COST, y=..density..)) + geom_density(adjust=5) + geom_histogram(bins=200) + xlim(0,30)
ggplot(mnldt[CHOICE==T,], aes(x=TRPDUR, y=..density..)) + geom_density(adjust=5) + geom_histogram(bins=100) + xlim(0,200)

ggplot(mnldt[CHOICE==T,], aes(x=TRPDIST, y=..density..*0.5)) + 
  geom_histogram(binwidth = 0.5, alpha = 0.25, color='black') + 
  geom_density(adjust=1, fill='#084594', alpha=0.5) + 
  scale_x_continuous("Travel distance (miles)", limits = c(0,15), breaks = seq(0,20,by=5), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=1), 
                     limits = c(0,0.175), breaks = seq(0,1,by=0.04), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family="Times"))

ggplot(mnldt[CHOICE==T,], aes(x=AGE, y=..density..*0.5)) + 
  geom_histogram(binwidth = 2, alpha = 0.5, color='black') + 
  geom_density(adjust=2, fill='#084594', alpha=0.5) + 
  scale_x_continuous("Age", limits = c(16,90), breaks = seq(10,90,by=10), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=0.01), limits = c(0,0.0175), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family="Times"))


ggplot(merge(mnldt[CHOICE==T,], varlabelvals$INCOME[,.(VAR,LABEL)], by.x='INCOME',by.y="VAR"), aes(x=LABEL)) + 
  scale_x_discrete("Household income", limits = varlabelvals$INCOME$LABEL) +
  scale_y_continuous("Frequency", labels = scales::comma) +
  geom_bar() + theme_classic() +
  theme(legend.position='bottom', text=element_text(family="Times"))

hist(mnldt$INCOME_VAL)
barplot(table(mnldt$INCOME))
hist(mnldt$AGE, breaks=200)
summary(mnldt$COST)
summary(mnldt$TRPDUR)




###plots
ggplot(votmodlistjt$INCAGE[VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_fill_distiller(palette = 'Spectral') + 
  scale_y_discrete(limits = varlabelvals$AGE_GRPD$LABEL) +
  scale_x_discrete(limits = varlabelvals$INCOME$LABEL)

ggplot(votmodlistjt$DISTAGE[VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_fill_distiller(palette = 'Spectral') + 
  scale_x_discrete(limits = varlabelvals$TRPDIST_GRPD$LABEL) +
  scale_y_discrete(limits = varlabelvals$AGE_GRPD$LABEL)

ggplot(votmodlistjt$INCDIST[VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_fill_distiller(palette = 'Spectral') + 
  scale_y_discrete(limits = varlabelvals$TRPDIST_GRPD$LABEL) +
  scale_x_discrete(limits = varlabelvals$INCOME$LABEL)



tmp <- votmodlistjt$INCAGE
tmp[ERR_LO<0, ERR_LO := 0]
tmp[ERR_HI>400, ERR_HI := 400]
ggplot(tmp, aes(color=V2_LABEL,y=VOT,x=V1_VAL)) +
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=5000) +
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,400)) +
  theme_classic()

tmp <- votmodlistjt$DISTAGE
tmp[ERR_LO<0, ERR_LO := 0]
tmp[ERR_HI>400, ERR_HI := 400]
ggplot(tmp, aes(color=V2_LABEL,y=VOT,x=V1_VAL)) +
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI, alpha=0.5), width=0.9) +
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,400)) +
  theme_classic()

tmp <- votmodlistjt$INCDIST
tmp[ERR_LO<0, ERR_LO := 0]
tmp[ERR_HI>400, ERR_HI := 400]
ggplot(votmodlistjt$INCDIST[VOT>0,], aes(color=V2_LABEL,y=VOT,x=V1_VAL)) +
  geom_point() + geom_line() + 
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), alpha=0.5, width=5000) +
  scale_y_continuous("Value of travel time ($/hr)", limits = c(0,400)) +
  theme_classic()

#scatplots
ggplot(mnldt[CHOICE==T,], aes(x=MODE, y=TRPDUR)) + geom_violin()
ggplot(mnldt[CHOICE==T,], aes(x=MODE, y=HHVEH)) + geom_violin()

ggplot(merge(mnldt[CHOICE==T, .N, by=.(INCOME,MODE)], mnldt[CHOICE==T, .N, by=INCOME], by='INCOME')[,N.x/N.y, by=.(INCOME,MODE)],
       aes(x=INCOME, y=V1, fill=MODE)) + geom_col(position = position_dodge())

ggplot(merge(mnldt[CHOICE==T, .N, by=.(TRPTIME,MODE)], mnldt[CHOICE==T, .N, by=TRPTIME], by='TRPTIME')[,N.x/N.y, by=.(TRPTIME,MODE)],
       aes(x=TRPTIME, y=V1, fill=MODE)) + geom_col(position = position_dodge())

ggplot(mnldt[CHOICE==T,], aes(x=TRPDUR, y=COST, color=MODE)) + geom_point(size=0.1) + 
  geom_smooth(method='lm') + theme_classic() + ylim(0,50)

ggplot(mnldt[CHOICE==T,], aes(x=TRPDUR, y=COST, color=INCOME)) + geom_point(size=0.1) + 
  geom_smooth(method='lm') + theme_classic() + ylim(0,50)

ggplot(mnldt[CHOICE==T & MODE_TYPE %in% c('SUBWAY','COMMUTER RAIL','LOCAL BUS'),], aes(x=TRPDUR, y=COST, color=MODE_TYPE)) + 
  geom_point(size=0.1) + theme_classic()




#### OLD PLOTS ####

plots <- list()

# #plot income dist
# plots[['incdist']] <- ggplot(data=data.table(HINCP=incdistfit$data)) +
#   geom_histogram(aes(x=PINCP, fill="Synthetic population"), bins=100, alpha=0.5) +
#   stat_function(aes(linetype="fit"), color="black", fun = plotfun(incdistfit)) +
#   scale_y_continuous("Frequency", labels = scales::percent, expand = c(0,0)) +
#   #scale_x_continuous("Income", labels=scales::dollar, limits = c(0,3e5), breaks=seq(0,5e5,1e5), expand = c(0,0)) +
#   scale_color_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
#   scale_fill_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
#   scale_linetype_manual(NULL,values = c("fit"="solid"), labels = c("fit"= paste("Fitted", hvotdistfit$distname, "distribution"))) +
#   theme_classic() + theme(legend.position = "bottom",text=element_text(family="Times", size=12))
# plots[['incdist']]


#plot VOT fit from MTS
plots[['mtsvot']] <- ggplot(data=mts_vot, aes(x=INCOMEVAL, y=VOT, weight=POPPROP, size=N)) + geom_point() +
  geom_abline(intercept = coef(mts_votmod)[1], slope=coef(mts_votmod)[2]) +
  annotate("text", x=1.5e5, y=15, label = paste("Intercept=",round(coef(mts_votmod)[1],4)), family="Times") +
  annotate("text", x=1.5e5, y=8, label = paste("'Slope'==frac(1,",1/coef(mts_votmod)[2],")"), family="Times", parse=T) +
  annotate("text", x=1.5e5, y=0, label = paste("R^{2}~'='~",round(summary(mts_votmod)$r.squared,4)), parse=T, family="Times") +
  scale_x_continuous("Income group defined in MTS ($)", breaks=mts_vot$INCOMEVAL, labels=mts_vot$BRACKET) + 
  #scale_y_continuous("Value of time ($/hr)", breaks=seq(0,40,5), limits = c(0,40)) +
  scale_size("Observations", breaks = 100*round((10^(seq(log10(600), log10(3500), length.out = 5)))/100)) +
  theme_classic() + theme(legend.text.align = 0,
                          text=element_text(family="Times", size=11),
                          #egend.position = "right",
                          #legend.position = c(0.8,0.5),
                          legend.background = element_blank(),
                          axis.text.x = element_text(angle = 30, hjust = 1)) 
plots[['mtsvot']]

#Plot mode share split by VOT from synth pop
plots[['votmodeshare']] <- ggplot() + 
  geom_point(data = melt(comdistdat, measure.vars = c("wt","bt","dt","dh")), aes(x=VOT, y=value, color=variable, size=N)) +
  geom_abline(data = rbindlist(lapply(names(votmodefit), function(x) data.table(t(votmodefit[[x]]), MODE=x))),
              aes(intercept=intercept, slope=slope, color=MODE)) +
  scale_size("Observations") +
  scale_x_continuous("Value of time ($/hr)", seq(0,max(comdistdat$binval),25), limits = c(0,max(comdistdat$binval))) +
  scale_y_continuous("Mode share (%)", seq(0,1,0.1), limits = c(0,1), labels = scales::percent) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer("Mode", labels = c("wt"="Walk","bt"="Bike","dt"="Drive+train","dh"="Drive+highway"), palette = "Set2") +
  theme_classic() + theme(text=element_text(family="Times", size=12))
#plots[['votmodeshare']]

#Plot fit functions
plotfun <- function(fit) {
  switch (fit$distname,
          norm = function(x) dnorm(x, mean = fit$estimate['mean'], sd = fit$estimate['sd']),
          lnorm = function(x) dlnorm(x, meanlog = fit$estimate['meanlog'], sdlog = fit$estimate['sdlog']),
          trunc_lnorm = function(x) dtrunc_lnorm(x, meanlog = fit$estimate['meanlog'], sdlog = fit$estimate['sdlog']),
          weibull = function(x) dweibull(x, shape = fit$estimate['shape'], scale = fit$estimate['scale']),
          cauchy = function(x) dcauchy(x, location = fit$estimate['location'], scale = fit$estimate['scale']),
          gamma = function(x) dgamma(x, shape = fit$estimate['shape'], rate = fit$estimate['rate'])
          )}

#plot(votdistfit, breaks=100)
plots[['votdist']] <- ggplot(data=melt(synth_pop[CVOT>0,.(HVOT,PVOT)], measure.vars = c("HVOT","PVOT")))+
  geom_density(aes(x=value, fill=variable), alpha = 0.5) +
  scale_y_continuous("Probability density distribution", labels = scales::percent_format(accuracy=1), limits = c(0,0.08), breaks = seq(0,0.1,0.01), expand = c(0,0)) +
  scale_x_continuous("Value of time ($/hr)", limits = c(0,105), breaks=seq(0,100,10), expand = c(0,0)) +
  scale_fill_brewer("Value of time basis:", palette = "Set1", 
                    labels = c("HVOT"="Household income", "PVOT"="Personal income")) +
  scale_color_brewer(NULL, palette = "Set1") +
  theme_classic() + theme(legend.position = "bottom",text=element_text(family="Times", size=12))
#plots[['votdist']]


#Plotting results
plots[['pvotdistfit']] <- ggplot(data=synth_pop[PVOT>0, ]) +
  geom_histogram(aes(x=PVOT, fill="Synthetic population"), stat = "density", alpha=0.5) +
  geom_density(aes(x=PVOT, color="Synthetic population"), linetype="dashed", stat = "density") +
  stat_function(aes(linetype="fit"), color="black", fun = plotfun(pvotdistfit)) +
  scale_y_continuous("Probability density distribution", labels = scales::percent_format(accuracy=1), limits = c(0,0.1), expand = c(0,0)) +
  scale_x_continuous("Value of time ($/hr)", limits = c(0,100), breaks=seq(0,100,10), expand = c(0,0)) +
  scale_color_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
  scale_fill_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
  scale_linetype_manual(NULL,values = c("fit"="solid"), labels = c("fit"= paste("Fitted", pvotdistfit$distname, "distribution"))) +
  theme_classic() + theme(legend.position = "bottom",text=element_text(family="Times", size=12))
#plots[['pvotdistfit']]

#Plotting results
plots[['cvotdistfit']] <- ggplot(data=synth_pop[CVOT>0, ]) +
  geom_histogram(aes(x=CVOT, fill="Synthetic population"), stat = "density", alpha=0.5) +
  geom_density(aes(x=CVOT, color="Synthetic population"), linetype="dashed", stat = "density") +
  stat_function(aes(linetype="fit"), color="black", fun = plotfun(cvotdistfit)) +
  scale_y_continuous("Probability density distribution", labels = scales::percent_format(accuracy=1), limits = c(0,0.08), expand = c(0,0)) +
  scale_x_continuous("Value of time ($/hr)", limits = c(0,100), breaks=seq(0,100,10), expand = c(0,0)) +
  scale_color_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
  scale_fill_manual(NULL, values = c("Synthetic population"="#4DAF4A")) +
  scale_linetype_manual(NULL,values = c("fit"="solid"), labels = c("fit"= paste("Fitted", cvotdistfit$distname, "distribution"))) +
  theme_classic() + theme(legend.position = "bottom",text=element_text(family="Times", size=12))
#plots[['cvotdistfit']]


#Household inc based VOT
plots[['hvotdistfit']] <- ggplot(data=synth_pop[CVOT>0, ]) +
  geom_histogram(aes(x=HVOT, fill="Population"), stat = "density", alpha=0.5) +
  geom_density(aes(x=HVOT, color="Population"), linetype="dashed", stat = "density") +
  stat_function(aes(linetype="fit"), color="black", fun = plotfun(hvotdistfit)) +
  scale_y_continuous("Probability density distribution", labels = scales::percent_format(accuracy=1), limits = c(0,0.05), expand = c(0,0)) +
  scale_x_continuous("Value of time ($/hr)", limits = c(0,101), breaks=seq(0,100,10), expand = c(0,0)) +
  scale_color_manual(NULL, values = c("Population"="#4DAF4A")) +
  scale_fill_manual(NULL, values = c("Population"="#4DAF4A")) +
  scale_linetype_manual(NULL,values = c("fit"="solid"),labels = c("fit"=expression("Fitted log-normal:"~ln(mu)==2.85*","~ln(sigma)==0.66))) +
  theme_classic() + theme(legend.position = "bottom",text=element_text(family="Times", size=12))
#plots[['hvotdistfit']]

#####bulk plotting ####
ggsave(paste("./plots/votplots/grVOT_mtsvot.pdf",sep = ""), plots[['mtsvot']], width = 6, height=3)
ggsave(paste("./plots/votplots/grVOT_hvotdistfit.pdf",sep = ""), plots[['hvotdistfit']], width = 6, height=3)
ggsave(paste("./plots/votplots/grVOT_votmodeshare.pdf",sep = ""), plots[['votmodeshare']], width = 5, height=4)
ggsave(paste("./plots/votplots/grVOT_votdist.pdf",sep = ""), plots[['votdist']], width = 5, height=4)
ggsave(paste("./plots/votplots/grVOT_pvotdistfit.pdf",sep = ""), plots[['pvotdistfit']], width = 5, height=4)
ggsave(paste("./plots/votplots/grVOT_cvotdistfit.pdf",sep = ""), plots[['cvotdistfit']], width = 5, height=4)

#Now save svg versions
ggsave(paste("./plots/votplots/svg/grVOT_mtsvot.svg",sep = ""), plots[['mtsvot']], width = 6, height=3)
ggsave(paste("./plots/votplots/svg/grVOT_hvotdistfit.svg",sep = ""), plots[['hvotdistfit']], width = 6, height=3)
ggsave(paste("./plots/votplots/svg/grVOT_votmodeshare.svg",sep = ""), plots[['votmodeshare']], width = 5, height=4)
ggsave(paste("./plots/votplots/svg/grVOT_votdist.svg",sep = ""), plots[['votdist']], width = 6, height=3)
ggsave(paste("./plots/votplots/svg/grVOT_pvotdistfit.svg",sep = ""), plots[['pvotdistfit']], width = 5, height=4)
ggsave(paste("./plots/votplots/svg/grVOT_cvotdistfit.svg",sep = ""), plots[['cvotdistfit']], width = 5, height=4)



#for(x in names(plots)) ggsave(paste("./plots/votplots/grVOT_",x,".pdf",sep = ""), plots[[x]], width = 5, height=4)
#for(x in names(plots)) ggsave(paste("./plots/votplots/svg/grVOT_",x,".svg",sep = ""), plots[[x]], width = 5, height=4)
rm(list=ls())


