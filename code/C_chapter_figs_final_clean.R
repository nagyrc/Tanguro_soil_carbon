#this code was written by Dr. Chelsea Nagy (University of Colorado- Boulder) for the publication "Soil carbon dynamics in soybean cropland and forests in Mato Grosso, Brazil".

#this code makes the figures for the manuscript (except those made in ArcGIS or Illustrator: Fig.1, Fig. S1, Fig. S2 )

library(ggplot2)
library(doBy)

#this brings in the main dataset
C14<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/Tanguro_14C_data.csv")


#################################################
#select only the samples that were not acidified
main<-C14[which(C14$acidified=="no"),]

#only samples from complete profiles
profiles<-main[which(main$profiles=="yes"),]

#only bulk soil samples
profilesBS<-profiles[which(profiles$samle_type=="BS"),]

#will need a subset later of only samples collected in 2013
main2013<-main[which(main$year_samp=="2013"),]

#only samples in complete profiles that were collected in 2013
profiles2013<-main2013[which(main2013$profiles=="yes"),]

#only bulk soil samples in complete profiles that were collected in 2013
profiles2013BS<-profiles2013[which(profiles2013$samle=="BS"),]

#low clay profiles from 2013
lc_prof<-profiles2013BS[which(profiles2013BS$clay_cat=="low"),]

#high clay profiles from 2013
hc_prof<-profiles2013BS[which(profiles2013BS$clay_cat=="high"),]

#add inverse depth to dataframe in low clay sites
lc_prof$inv_dep<-lc_prof$depth*-1

#add inverse depth to dataframe in high clay sites
hc_prof$inv_dep<-hc_prof$depth*-1


############################################
#for Fig.2a in manuscript

#summary by function of 14C by land use by year sampled (2009 or 2013) by depth
tt1<-summaryBy(X.14C~land_use+year_samp+depth,data=profilesBS,FUN=c(mean,sd,length))

as.numeric(tt1$X.14C.length)
tt1$X.14C.se<-tt1$X.14C.sd/sqrt(tt1$X.14C.length)

#add inverse depth
tt1$inv_dep<-tt1$depth*-1

#turn year sampled into character
tt1$year_sampc<-as.character(tt1$year_samp)

head(tt1)

#Fig.2a here
p16<-ggplot()+
  geom_point(data=tt1, size=3, aes(x=X.14C.mean, y=inv_dep, color=year_sampc,shape=land_use))+
  geom_errorbarh(data=tt1,aes(xmax = X.14C.mean + X.14C.se, xmin=X.14C.mean - X.14C.se,height=2,y=inv_dep, x=X.14C.mean, color=year_sampc))+
  geom_path(data=tt1, aes(x=X.14C.mean, y=inv_dep, color=year_sampc, shape=land_use),size=1)+
  xlim(-500,200)+
  ylim(-250,0)+
  xlab('Bulk Soil 14C')+
  ylab('Depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("dark gray","black")) 
p16


#Fig.2b

#reshape dataframe wide
w2<-reshape(profilesBS,timevar="year_samp",idvar=c("point","depth"),direction="wide")

#create values for 1:1 line
x<-as.vector(c(-500,-400,-200,-100,0,100,200))
y<-as.vector(c(-500,-400,-200,-100,0,100,200))

xy<-as.data.frame(cbind(x,y))

head(w2)

#Fig.2b
p16<-ggplot()+
  geom_point(data=w2, aes(x=X.14C.2009, y=X.14C.2013, color=land_use.2013))+
  geom_line(data=xy, aes(x=x, y=y),size=1, color="black")+
  xlim(-600,300)+
  ylim(-600,300)+
  xlab('2009 Bulk Soil 14C')+
  ylab('2013 Bulk Soil 14C')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("gray","black"))
p16



#Fig.3a in manuscript; low clay
p16<-ggplot()+
  geom_point(data=lc_prof, aes(x=X.14C, y=inv_dep, color=point))+
  geom_path(data=lc_prof, aes(x=X.14C, y=inv_dep, color=point),size=1)+
  xlim(-500,200)+
  ylim(-250,0)+
  xlab('Bulk Soil 14C')+
  ylab('Depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","black")) 
p16



#Fig.3b in manuscript; high clay
p16<-ggplot()+
  geom_point(data=hc_prof, aes(x=X.14C, y=inv_dep, color=point))+
  geom_path(data=hc_prof, aes(x=X.14C, y=inv_dep, color=point),size=1)+
  xlim(-500,200)+
  ylim(-250,0)+
  xlab('Bulk Soil 14C')+
  ylab('Depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("#CCCCCC", "#999999", "#666666","black")) 
p16




#start of Fig.4

#bulk soil and CO2 samples only
fig4sub<-profiles2013[which(profiles2013$samle_type=="BS"|profiles2013$samle_type=="CO2"),]

#reshape dataframe: make wide
w<-reshape(fig4sub,timevar="samle_type",idvar=c("point","depth"),direction="wide")

#create data for 1:1 line
x<-as.vector(c(-400,-200,-100,0,100,200,300))
y<-as.vector(c(-400,-200,-100,0,100,200,300))

xy<-as.data.frame(cbind(x,y))

w$depthc<-as.character(w$depth)

wsub<-w[which(w$depthc=="10"|w$depthc=="50"|w$depthc=="100"),]

#reorder factors
wsub$deptho <- factor(wsub$depthc, levels=c("10", "50", "100"), labels=c("10", "50", "100"))

#Fig.4
p16<-ggplot()+
  geom_point(data=wsub, aes(x=X.14C.BS, y=X.14C.CO2, color=deptho))+
  geom_line(data=xy, aes(x=x, y=y),size=1, color="black")+
  xlim(-400,300)+
  ylim(-400,300)+
  xlab('14C Bulk Soil')+
  ylab('14C CO2')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("#CCCCCC", "#999999", "black")) 
p16






#start of Fig.5


#Fig. 5a
#summary by function
tt2<-summaryBy(d13C~land_use+year_samp+depth,data=profilesBS,FUN=c(mean,sd,length))
as.numeric(tt2$d13C.length)
tt2$d13C.se<-tt2$d13C.sd/sqrt(tt2$d13C.length)

tt2$inv_dep<-tt2$depth*-1

tt2$year_sampc<-as.character(tt2$year_samp)

#try ordering the dataset by depth
ttz<- tt2[with(tt2, order(land_use,year_samp,depth)), ]

#Fig.5a
p16<-ggplot()+
  geom_point(data=ttz, size=3,aes(x=d13C.mean, y=inv_dep, color=year_sampc,shape=land_use))+
  geom_errorbarh(data=ttz,aes(xmax = d13C.mean + d13C.se, xmin=d13C.mean - d13C.se,height=2,y=inv_dep, x=d13C.mean, color=year_sampc))+
  geom_path(data=ttz, aes(x=d13C.mean, y=inv_dep, color=year_sampc, shape=land_use),size=1)+
  xlim(-30,-20)+
  ylim(-250,0)+
  xlab('13C')+
  ylab('depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("dark gray","black")) 
p16




#Fig.5b

#create data for 1:1 line
x<-as.vector(c(-30,-20, -10, 0))
y<-as.vector(c(-30,-20,-10,0))

xy<-as.data.frame(cbind(x,y))

#Fig. 5b
p16<-ggplot()+
  geom_point(data=w2, aes(x=d13C.2009, y=d13C.2013, color=land_use.2013))+
  geom_line(data=xy, aes(x=x, y=y),size=1, color="black")+
  xlim(-30,-20)+
  ylim(-30,-20)+
  xlab('2009 Bulk Soil 13C')+
  ylab('2013 Bulk Soil 13C')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("gray","black"))
p16

#end main manuscript figure prep
##################################################




##################################################
#start supplemental figure prep

#Fig.S3
w2$diff14<-w2$X.14C.2013-w2$X.14C.2009

w2$inv_dep<-w2$depth*-1

#Fig.S3
p16<-ggplot()+
  geom_point(data=w2, aes(x=diff14, y=inv_dep, color=point,shape=land_use.2013))+
  xlim(-150,150)+
  ylim(-250,0)+
  xlab('Change in Bulk Soil 14C from 2009 to 2013')+
  ylab('depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("#99CCFF","#3399FF","#0099CC", "#0000CC", "#CCCCCC", "#999999", "#666666", "black"))
p16
#warnings are no data





#####################note: code for making of Fig. S4 is below code for making Fig. S5 

#Fig.S5
w2$diff13<-w2$d13C.2013-w2$d13C.2009

#Fig.S5
p16<-ggplot()+
  geom_point(data=w2, aes(x=diff13, y=inv_dep, color=point,shape=land_use.2013))+
  xlim(-2,2)+
  ylim(-250,0)+
  xlab('Change in Bulk Soil 13C from 2009 to 2013')+
  ylab('depth (cm)')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("#99CCFF","#3399FF","#0099CC", "#0000CC", "#CCCCCC", "#999999", "#666666", "black"))
p16
#warnings are no data






#this is for Figure S4; need to bring in data from 2 pool model
forest<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/figS4_forest_model.csv")

head(forest)

colnames(forest)<- c("Year","Atm","BS14C","CO214C","Pool1","Pool2")

forestptsx<-as.data.frame(c(2013,2013))
forestptsy<-as.data.frame(c(107.8,73.8))
forestptsf<-as.data.frame(c("BS 2013","CO2 2013"))

forestpts<-cbind(forestptsx,forestptsy,forestptsf)

colnames(forestpts)<- c("year", "d14C","data")
str(forestpts)

#Fig. S4
p16<-ggplot()+
  geom_point(data=forestpts, size=4,aes(x=year, y=d14C, color=data))+
  geom_path(data=forest, aes(x=Year, y=Atm),color="black",size=1)+
  geom_path(data=forest, aes(x=Year, y=BS14C),linetype="dotted",color="blue",size=1)+
  geom_path(data=forest, aes(x=Year, y=CO214C),linetype="dotted",color="green",size=1)+
  geom_path(data=forest, aes(x=Year, y=Pool1),color="green",size=1)+
  geom_path(data=forest, aes(x=Year, y=Pool2),color="blue",size=1)+
  xlim(1950,2015)+
  ylim(-100,900)+
  xlab('Year')+
  ylab('14C ()')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),panel.grid.major = element_blank(),panel.border=element_blank(),axis.line.x=element_line(color="black"),axis.line.y=element_line(color="black"))+
  scale_color_manual(values=c("blue", "green")) 
p16


