


#Libraries
library(ggplot2)
library(data.table)
library(extrafont)
library(egg)

#Load data
load("./writing/localdata/dat_models.RData")
load("./writing/localdata/dat_votgrid.RData")
load("./writing/localdata/dat_tables.RData")

#Destinations
dir <- "./presentation/graphics/"
fnt <- "Arial"
jointplots <- list()
indieplots <- list()
smuthplots <- list()
distrplots <- list()

#### Data distribution plots #####
#
hours <- mnldt[CHOICE==T,.(DEP_HR,DEP_MIN)]
hours[DEP_HR==24, DEP_HR:=0]
hours[ , TIME := as.POSIXct(paste(DEP_HR,DEP_MIN,sep=":"),format='%H:%M', tz="GMT")]

distrplots$TIME <- ggplot(hours, aes(x=TIME, y=..density..*60*30)) + 
  #geom_histogram(binwidth = 60*30, alpha = 0.5, color='black') +
  geom_density(adjust=1, fill='#fdbb84', alpha=0.5) + 
  scale_x_datetime("Time of day", breaks=scales::date_breaks("6 hour"), labels=scales::date_format(format = "%H")) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy = 1), expand = c(0,0)) +
  theme_classic() + 
  theme(text=element_text(family=fnt),
        legend.background = element_blank())
#

distrplots$COST <- ggplot(mnldt[CHOICE==T & !(MODE %in% c("SUBWAY","LOCAL BUS")),], aes(x=COST, y=..density..*0.5)) + 
  #geom_histogram(binwidth = 0.5, alpha = 0.25, color='black') + 
  geom_density(adjust=2, fill='#fdbb84', alpha=0.5) + 
  scale_x_continuous("Travel cost ($)", labels = scales::dollar, breaks = seq(0,20,by=5),
                     limits = c(0,16), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=1), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank())

#
distrplots$TRPDUR <- ggplot(mnldt[CHOICE==T,], aes(x=TRPDUR, y=..density..*0.25)) + 
  #geom_histogram(binwidth = 5, alpha = 0.25, color='black') + 
  geom_density(adjust=3, fill='#fdbb84', alpha=0.5) + 
  scale_x_continuous("Travel time (minutes)", limits = c(0,60), breaks = seq(0,90,by=10), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=0.1), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank())

#
distrplots$TRPDIST <- ggplot(mnldt[CHOICE==T,], aes(x=TRPDIST, y=..density..*0.5)) + 
  #geom_histogram(binwidth = 0.5, alpha = 0.25, color='black') + 
  geom_density(adjust=1, fill='#fdbb84', alpha=0.5) + 
  scale_x_continuous("Travel distance (miles)", limits = c(0,15), breaks = seq(0,20,by=5), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=1), 
                     limits = c(0,0.16), breaks = seq(0,0.16,by=0.04), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank())

#
distrplots$AGE <- ggplot(mnldt[CHOICE==T,], aes(x=AGE, y=..density..*0.5)) + 
  #geom_histogram(binwidth = 2, alpha = 0.5, color='black') + 
  geom_density(adjust=2, fill='#fdbb84', alpha=0.5) + 
  scale_x_continuous("Age", limits = c(16,90), breaks = seq(10,90,by=20), expand = c(0,0)) +
  scale_y_continuous("Percent", labels = scales::percent_format(accuracy=0.01), limits = c(0,0.015), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank())
#

#### Individual plots ####

indieplots$MODE <- ggplot(votmodlist[['MODE']], aes(x=VAR,y=VOT)) +
  geom_col(fill='#fdbb84', color='black')+  
  #geom_point() + #aes(size=N)) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width = 0.2) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,60), expand = c(0,0)) +
  scale_x_discrete(" ", limits = votmodlist[['MODE']][order(-VOT),][VOT>0,VAR],
                   label = votmodlist[['MODE']][order(-VOT),][VOT>0,LABEL]) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank(),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(margin=margin(t=6,r=0,b=0,l=0)),
        axis.text.x = element_text(angle=30, hjust=1))

#
indieplots$TRPPUR <- ggplot(votmodlist[['TRPPUR']], aes(x=VAR,y=VOT)) +
  geom_col(fill='#fdbb84', color='black')+
  #geom_point() + #aes(size=N)) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=0.2) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,100), expand = c(0,0)) +
  scale_x_discrete(NULL, limits = votmodlist[['TRPPUR']][order(-VOT),VAR],
                   label = votmodlist[['TRPPUR']][order(-VOT),LABEL]) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank(),
        axis.title.y = element_text(size=10),
        axis.text.x = element_text(angle=30, hjust=1))


#
indieplots$TRPTIME <- ggplot(votmodlist[['TRPTIME']], aes(x=LABEL,y=VOT)) +
  geom_col(fill='#fdbb84', color='black')+
  #geom_point() + #aes(size=N)) +#aes(size=N)) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=0.2) +
  scale_x_discrete(" ", limits = varlabelvals$TRPTIME$LABEL, label = c("AM","Midday","PM","Night")) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,125), expand = c(0,0)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank(),
        axis.title.y = element_text(size=10),
        axis.title.x = element_text(margin=margin(t=6,r=0,b=0,l=0)),
        axis.text.x = element_text(angle=30, hjust=1))

#
indieplots$INCOME <- ggplot(votmodlist[['INCOME']], aes(x=VAL/1000,y=VOT)) +
  stat_function(fun=function(x) fun.loglin(1000*x, coef(fit_inc)), color = 'gray50') +
  geom_point()+#aes(size=N)) +
  annotate("text", x=150, y=10, label = paste("R^2==",round(summary(fit_inc)[['r.squared']],2)), parse=T, size=3, family = fnt) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=0.1*200) +
  scale_x_continuous("Income (thousands)", limits = c(0,200), label = scales::dollar) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,100)) +
  theme_classic() +
  theme(text=element_text(family=fnt),
        legend.background = element_blank())


#
indieplots$AGE_GRPD <- ggplot(votmodlist[['AGE_GRPD']], aes(x=VAL,y=VOT)) +
  stat_function(fun=function(x) fun.norm(x, coef(fit_age)), color = 'gray50') +
  geom_point()+#aes(size=N)) +
  annotate("text", x=70, y=10,
           label = paste("R^2==",round(nls.rsquared(fit_age, votmodlist[['AGE_GRPD']][['VOT']],4),3)), parse=T, size=3, family = fnt) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=0.1*80) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,80)) +
  scale_x_continuous("Age (years)", limits = c(0,100)) +
  theme_classic() + theme(text=element_text(family=fnt),
                          legend.background = element_blank(),
                          legend.position = "none")

#
indieplots$TRPDIST_GRPD <- ggplot(votmodlist[['TRPDIST_GRPD']], aes(x=VAL,y=VOT)) +
  stat_function(fun=function(x) fun.loglin(x, coef(fit_dist)), color = 'gray50') +
  geom_point()+#aes(size=N)) +
  annotate("text", x=12, y=40, label = paste("R^2==",round(summary(fit_dist)[['r.squared']],2)), parse=T, size=3, family = fnt) +
  geom_errorbar(aes(ymin=ERR_LO, ymax=ERR_HI), width=0.1*15) +
  scale_x_continuous("Travel distance (miles)", limits = c(0, 18)) +
  scale_y_continuous("value of time ($/hr)", limits = c(0,310)) +
  theme_classic() + theme(text=element_text(family=fnt),
                          legend.background = element_blank(),
                          legend.position = "none")


#### Smooth Joint Plots ####
smuthplots$INCAGE <- ggplot(unique(votgrid[,.(AGE,INCOME,AGE.INC.VOT)]), aes(y=AGE,x=INCOME/1000,z=AGE.INC.VOT)) +
    geom_tile(aes(fill=AGE.INC.VOT)) + geom_contour(color='gray', bins=10) +
    scale_x_continuous("Income (thousands)", label = scales::dollar, expand = c(0,0)) +
    scale_y_continuous("Age", breaks = seq(0,80,by=10), expand = c(0,0)) +
    scale_fill_distiller("VOT ($/hr)", palette = "OrRd", direction = 1, limits = c(10,160)) + theme_bw() +
    theme(legend.position='none',
          text=element_text(family=fnt),
          legend.background = element_blank(),
          plot.margin=unit(c(2,8,2,2),"pt"))
  

#
smuthplots$DISTAGE <- ggplot(unique(votgrid[,.(AGE,DIST,AGE.DIST.VOT)]), aes(y=AGE,x=DIST,z=AGE.DIST.VOT)) +
    geom_tile(aes(fill=AGE.DIST.VOT)) + geom_contour(color='gray', bins=10) +
    scale_x_continuous("Travel distance (miles)", breaks = seq(0,15,by=3), expand = c(0,0)) +
    scale_y_continuous("Age", breaks = seq(0,80,by=10), expand = c(0,0)) +
    scale_fill_distiller("VOT", palette = "OrRd", direction = 1, limits = c(10,160)) +
    theme_bw() + theme(legend.position='right',
                       text=element_text(family=fnt),
                       legend.background = element_blank(),
                       plot.margin=unit(c(2,4,2,2),"pt"))
  
#
smuthplots$INCDIST <- ggplot(unique(votgrid[,.(DIST,INCOME,DIST.INC.VOT)]), aes(x=INCOME/1000,y=DIST,z=DIST.INC.VOT)) +
    geom_tile(aes(fill=DIST.INC.VOT)) + geom_contour(color='gray', bins=10) +
    scale_x_continuous("Income (thousands)", label = scales::dollar, expand = c(0,0)) +
    scale_y_continuous("Travel distance (miles)", breaks = seq(0,15,by=3), expand = c(0,0)) +
    scale_fill_distiller("VOT ($/hr)", palette = "OrRd", direction = 1, limits = c(10,165)) + theme_bw() +
    theme(legend.position='none',
          text=element_text(family=fnt),
          legend.background = element_blank(),
          plot.margin=unit(c(2,8,2,2),"pt"))


#### Joint plots ####
jointplots$INCAGE <- ggplot(votmodlistjt[['INCAGE']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL,
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(AGE_GRPD))) +
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7), 
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt))
        

#
jointplots$DISTAGE <- ggplot(votmodlistjt[['DISTAGE']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_y_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(AGE_GRPD))) +
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),
        #axis.title.x = element_text(margin=unit(c(20,0,0,0),"pt")),
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt))

#
jointplots$INCDIST <- ggplot(votmodlistjt[['INCDIST']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPDIST_GRPD))) +
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),  
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt),)
        

#
jointplots$INCPURP <- ggplot(votmodlistjt[['INCPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(angle = 30, size=7), 
        text=element_text(family=fnt)) 

#
jointplots$INCTIME <- ggplot(votmodlistjt[['INCTIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_x_discrete("Income", limits = varlabelvals$INCOME$LABEL,
                   labels = c(varlabelvals$INCOME$LABEL[-6],"\u2265 150,000")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(INCOME)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt))

#
jointplots$PURPDIST <- ggplot(votmodlistjt[['DISTPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),
        #axis.title.x = element_text(margin=unit(c(20,0,0,0),"pt")),
        text=element_text(family=fnt))

#
jointplots$DISTTIME <- ggplot(votmodlistjt[['DISTTIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +	
  scale_x_discrete("Travel distance", limits = varlabelvals$TRPDIST_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$TRPDIST_GRPD$LABEL[-7],"\u2265 12")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPDIST_GRPD)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt))

#
jointplots$AGEPURP <- ggplot(votmodlistjt[['AGEPURP']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() +
  scale_x_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +		
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels = gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(AGE_GRPD)/nrow(TRPPUR))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7), 
        text=element_text(family=fnt)) 

#
jointplots$AGETIME <- ggplot(votmodlistjt[['AGETIME']][VOT>0,], aes(x=V1_LABEL,y=V2_LABEL,fill=VOT)) + geom_tile() + 
  scale_y_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_x_discrete("Age (years)", limits = varlabelvals$AGE_GRPD$LABEL, expand = c(0,0),
                   labels = c(varlabelvals$AGE_GRPD$LABEL[-7],"\u2265 70")) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) + 
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(AGE_GRPD)/nrow(TRPTIME))) + 
  theme(legend.position='none',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(angle = 30, hjust=1, size=7),
        axis.text.y = element_text(size=7),
        #axis.title.y = element_text(margin=unit(c(0,12,0,0),"pt")),
        text=element_text(family=fnt))

#
jointplots$PURPTIME <- ggplot(votmodlistjt[['PURPTIME']][VOT>0,], aes(x=V2_LABEL,y=V1_LABEL,fill=VOT)) + geom_tile() + 
  scale_x_discrete("Time of day", limits = varlabelvals$TRPTIME$LABEL, expand = c(0,0),
                   labels = sapply(strsplit(varlabelvals$TRPTIME$LABEL," \\("), function(x) x[1])) +
  scale_y_discrete("Trip purpose", limits = varlabelvals$TRPPUR$LABEL, expand = c(0,0),
                   labels= gsub("/","/\n",varlabelvals$TRPPUR$LABEL)) +
  scale_fill_distiller("", palette = "OrRd", direction = 1, limits = c(0,450)) +
  theme_bw() + coord_fixed(ratio=with(varlabelvals, nrow(TRPTIME)/nrow(TRPPUR))) + 
  theme(legend.position='left',
        #legend.margin=margin(t = -3, b=-6, unit='pt'),
        legend.background = element_blank(),
        axis.text.x = element_text(size=8),
        axis.text.y = element_text(size=8),  
        text=element_text(family=fnt))

jointplots.arranged <- ggarrange(plots=jointplots[-which("PURPTIME"==names(jointplots))], nrow=3)



#### Saving ####

for(name in names(distrplots)) {
  # if(name %in% c("TRPDUR","TRPDIST","COST")) 
    ggsave(paste0(dir,"DISTR_",name,".svg"), distrplots[[name]], width = 1.875, height=1.5, bg = "transparent")
  # else 
  #   ggsave(paste0(dir,"DISTR_",name,".svg"), distrplots[[name]], width = 2.5, height=1.5)
}

#Individual plots
for(name in names(indieplots)) ggsave(paste0(dir,"INDIE_",name,".svg"), indieplots[[name]], width = 2.5, height=2, bg = "transparent")

#Joint plots
for(name in names(jointplots)) {
  if(name == "PURPTIME")
    ggsave(paste0(dir,"JOINT_",name,".svg"), jointplots[[name]], width = 3, height=2, bg = "transparent")
  # else if(name %in% c("DISTTIME","AGEPURP","AGETIME"))
  #   ggsave(paste0(dir,"JOINT_",name,".svg"), jointplots[[name]], width = 2.25, height=2)
  else
    ggsave(paste0(dir,"JOINT_",name,".svg"), jointplots[[name]], width = 2, height=2, bg = "transparent")
}
ggsave(paste0(dir,"JOINT_ARRANGED.svg"), jointplots.arranged, width = 6, height = 6, bg = "transparent")

#Smoothed plots
for(name in names(smuthplots)) {
  if(name == "DISTAGE") 
    ggsave(paste0(dir,"SMUTH_",name,".svg"), smuthplots[[name]], width = 2.875, height=2, bg = "transparent")
  else 
    ggsave(paste0(dir,"SMUTH_",name,".svg"), smuthplots[[name]], width = 2, height=2, bg = "transparent")
}







