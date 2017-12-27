#this code was written by Dr. Chelsea Nagy (University of Colorado- Boulder) for the publication "Soil carbon dynamics in soybean cropland and forests in Mato Grosso, Brazil".

library(ggplot2)
library(doBy)
library(sm)
library(quantreg)
library(car)

C14<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/final_code_and_data/data/Tanguro_14C_data_for_analysis_BS.csv")
str(C14)
head(C14)

#excluding samples that had been acidified for test of carbonates
main<-C14[which(C14$acidified=="no"),]

#selecting bulk soil samples only
mainBS<-main[which(main$samle=="BS"),]

#subset samples collected in 2009
mainBS2009<-mainBS[which(mainBS$year_samp=="2009"),]
#subset samples collected in 2013
mainBS2013<-mainBS[which(mainBS$year_samp=="2013"),]

#to calculate the # of years in soybean cultivation
mainBS2013$yrssoyBS<-2013-mainBS2013$year_conv
mainBS2013$yrssoyBSn<-gsub("-7", "0", yrssoyBS)
mainBS2013$yrssoyBSnn<-as.numeric(mainBS2013$yrssoyBSn)
is.numeric(mainBS2013$yrssoyBSnn)
#true

#subset samples collected in 2013 by depth: 0-10, 10-20, 40-50, 90-100, 190-200
depth10_13<-mainBS2013[which(mainBS2013$depth=="10"),]
depth20_13<-mainBS2013[which(mainBS2013$depth=="20"),]
depth50_13<-mainBS2013[which(mainBS2013$depth=="50"),]
depth100_13<-mainBS2013[which(mainBS2013$depth=="100"),]
depth200_13<-mainBS2013[which(mainBS2013$depth=="200"),]

#subset samples collected in 2009 by depth: 0-10, 10-20, 40-50, 90-100, 190-200
depth10_09<-mainBS2009[which(mainBS2009$depth=="10"),]
depth20_09<-mainBS2009[which(mainBS2009$depth=="20"),]
depth50_09<-mainBS2009[which(mainBS2009$depth=="50"),]
depth100_09<-mainBS2009[which(mainBS2009$depth=="100"),]
depth200_09<-mainBS2009[which(mainBS2009$depth=="200"),]



#To make Table 1 in manuscript- %C, BD, C content
#######################################
###
#%C

#%C for 0-10 cm
means<-with (depth10_13, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 2.42, soy: 1.16
round(ses, digits=2)
# forest: 0.20, soy: 0.12

#test for normality
test.lm33<-lm(perc_C~land_use, data=depth10_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.09642

C10.aov<-aov(perc_C~land_use, data=depth10_13)
anova(C10.aov)
#p=3.331e-05



#% C for 0-20 cm
means<-with (depth20_13, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(depth20_13, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(depth20_13, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.04, soy: 0.87
round(ses, digits=2)
# forest: 0.11, soy: 0.05

#test for normality
test.lm33<-lm(perc_C~land_use, data=depth20_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.1064

C10.aov<-aov(perc_C~land_use, data=depth20_13)
anova(C10.aov)
#p=0.1645



#%C for 40-50 cm
means<-with (depth50_13, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(depth50_13, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(depth50_13, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 0.69, soy: 0.57
round(ses, digits=2)
# forest: 0.13, soy: 0.04

#test for normality
test.lm33<-lm(perc_C~land_use, data=depth50_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.0013
#not normal; need non-parametric equivalent

plot(perc_C~land_use, data=depth50_13)
hist(depth50_13$perc_C)

#try log transformation
test.lm33<-lm(log(perc_C)~land_use, data=depth50_13)
shapiro.test(residuals(test.lm33))
#p=0.6226

C10.aov<-aov(log(perc_C)~land_use, data=depth50_13)
anova(C10.aov)
#p=0.2654



#%C for 90-100 cm
means<-with (depth100_13, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(depth100_13, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(depth100_13, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 0.57, soy: 0.40
round(ses, digits=2)
# forest: 0.21, soy: 0.04

#test for normality
test.lm33<-lm(perc_C~land_use, data=depth100_13)
par(mfrow=c(2,2))
plot(test.lm33)
#few outliers

shapiro.test(residuals(test.lm33))
#p=1.225e-07
#not normal; need non-parametric equivalent

#try log transformation
test.lm33<-lm(log(perc_C)~land_use, data=depth100_13)
shapiro.test(residuals(test.lm33))
#p=0.004884
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(perc_C)~land_use, data=depth100_13)
shapiro.test(residuals(test.lm33))
#still not normal

#non-parametric
kruskal.test(perc_C~land_use,data=depth100_13)
#p=0.7882


#%C for 190-200 cm
means<-with (depth200_13, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(depth200_13, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 0.47, soy: 0.36
round(ses, digits=2)
# forest: 0.18, soy: 0.05

#test for normality
test.lm33<-lm(perc_C~land_use, data=depth200_13)
par(mfrow=c(2,2))
plot(test.lm33)
#couple outliers

shapiro.test(residuals(test.lm33))
#p=1.829e-08
#non-normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(perc_C)~land_use, data=depth200_13)
shapiro.test(residuals(test.lm33))
#p=0.0095
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(perc_C)~land_use, data=depth200_13)
shapiro.test(residuals(test.lm33))
#p=5.081e-6
#still not normal

#non-parametric
kruskal.test(perc_C~land_use,data=depth200_13)
#p=0.9671



#%C across whole profile, all depths
means<-with (mainBS2013, tapply(perc_C, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(perc_C, land_use, sd, na.rm=TRUE))
ns<-with(mainBS2013, tapply(perc_C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.04, soy: 0.67
round(ses, digits=2)
# forest: 0.14, soy: 0.039

#test for normality
test.lm33<-lm(perc_C~land_use, data=mainBS2013)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=5.43e-10
#not normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(perc_C)~land_use, data=mainBS2013)
shapiro.test(residuals(test.lm33))
#p=0.04
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(perc_C)~land_use, data=mainBS2013)
shapiro.test(residuals(test.lm33))
#p=6.14e-06
#still not normal

#non-parametric
kruskal.test(perc_C~land_use,data=mainBS2013)
#p=0.0533






###
#BD for Table 1 in manuscript

#BD for 0-10 cm
means<-with (depth10_13, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.18, soy: 1.49
round(ses, digits=2)
# forest: 0.05, soy: 0.03

#test for normality
test.lm33<-lm(BD~land_use, data=depth10_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.7682
#looks fine

C10.aov<-aov(BD~land_use, data=depth10_13)
anova(C10.aov)
#p=4.48e-05



#BD for 10-20 cm
means<-with (depth20_13, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(depth20_13, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(depth20_13, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.37, soy: 1.62
round(ses, digits=2)
# forest: 0.07, soy: 0.02

#test for normality
test.lm33<-lm(BD~land_use, data=depth20_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.9551
#looks fine

C10.aov<-aov(BD~land_use, data=depth20_13)
anova(C10.aov)
#p=0.05



#BD for 40-50 cm
means<-with (depth50_13, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(depth50_13, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(depth50_13, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.48, soy: 1.58
round(ses, digits=2)
# forest: 0.07, soy: 0.03

#test for normality
test.lm33<-lm(BD~land_use, data=depth50_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.08
#not great, but ok

C10.aov<-aov(BD~land_use, data=depth50_13)
anova(C10.aov)
#p=0.4409



#BD for 90-100 cm
means<-with (depth100_13, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(depth100_13, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(depth100_13, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.38, soy: 1.41
round(ses, digits=2)
# forest: 0.108, soy: 0.0265

#test for normality
test.lm33<-lm(BD~land_use, data=depth100_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.6088
#this is fine

C10.aov<-aov(BD~land_use, data=depth100_13)
anova(C10.aov)
#p=0.8936



#BD for 190-200 cm
means<-with (depth200_13, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(depth200_13, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.22, soy: 1.38
round(ses, digits=2)
# forest: 0.05, soy: 0.04

#test for normality
test.lm33<-lm(BD~land_use, data=depth200_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.2773
#this is fine

C10.aov<-aov(BD~land_use, data=depth200_13)
anova(C10.aov)
#p=0.42



#BD across whole profile- all depths
means<-with (mainBS2013, tapply(BD, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(BD, land_use, sd, na.rm=TRUE))
ns<-with(mainBS2013, tapply(BD, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1.32, soy: 1.49
round(ses, digits=2)
# forest: 0.04, soy: 0.01

#test for normality
test.lm33<-lm(BD~land_use, data=mainBS2013)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.1008
#this is ok

C10.aov<-aov(BD~land_use, data=mainBS2013)
anova(C10.aov)
#p=0.0003





###
#C content for Table 1
head(mainBS2013)

#C content for 0-10 cm
means<-with (depth10_13, tapply(C_cont_in_interval_g_m2, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(C_cont_in_interval_g_m2, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(C_cont_in_interval_g_m2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 2826, soy: 1739
round(ses, digits=2)
# forest: 170.88, soy: 190.72

#test for normality
test.lm33<-lm(C_cont_in_interval_g_m2~land_use, data=depth10_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.4028
#this is fine

C10.aov<-aov(C_cont_in_interval_g_m2~land_use, data=depth10_13)
anova(C10.aov)
#p=0.0094



#C content for 10-20 cm
means<-with (depth20_13, tapply(C_cont_in_interval_g_m2, land_use, mean, na.rm=TRUE))
sds<-with(depth20_13, tapply(C_cont_in_interval_g_m2, land_use, sd, na.rm=TRUE))
ns<-with(depth20_13, tapply(C_cont_in_interval_g_m2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1380.66, soy: 1556.88
round(ses, digits=2)
# forest: 194.37, soy: 110.64

#test for normality
test.lm33<-lm(C_cont_in_interval_g_m2~land_use, data=depth20_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.3225
#this is fine

C10.aov<-aov(C_cont_in_interval_g_m2~land_use, data=depth20_13)
anova(C10.aov)
#p=0.667



#C content for 40-50 cm
means<-with (depth50_13, tapply(C_cont_in_interval_g_m2, land_use, mean, na.rm=TRUE))
sds<-with(depth50_13, tapply(C_cont_in_interval_g_m2, land_use, sd, na.rm=TRUE))
ns<-with(depth50_13, tapply(C_cont_in_interval_g_m2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1175.24, soy: 1093.94
round(ses, digits=2)
# forest: 302.77, soy: 108.98

#test for normality
test.lm33<-lm(C_cont_in_interval_g_m2~land_use, data=depth50_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.01
#not normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(C_cont_in_interval_g_m2)~land_use, data=depth50_13)
shapiro.test(residuals(test.lm33))
#p=0.03
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(C_cont_in_interval_g_m2)~land_use, data=depth50_13)
shapiro.test(residuals(test.lm33))
#p=0.0168
#still not normal

#non-parametric
kruskal.test(C_cont_in_interval_g_m2~land_use, data=depth50_13)
#p=0.7728


#C content for 90-100 cm
means<-with (depth100_13, tapply(C_cont_in_interval_g_m2, land_use, mean, na.rm=TRUE))
sds<-with(depth100_13, tapply(C_cont_in_interval_g_m2, land_use, sd, na.rm=TRUE))
ns<-with(depth100_13, tapply(C_cont_in_interval_g_m2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 1116.25, soy: 768.79
round(ses, digits=2)
# forest: 475.95, soy: 128.53

#test for normality
test.lm33<-lm(C_cont_in_interval_g_m2~land_use, data=depth100_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.0072
#not normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(C_cont_in_interval_g_m2)~land_use, data=depth100_13)
shapiro.test(residuals(test.lm33))
#p=0.0048
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(C_cont_in_interval_g_m2)~land_use, data=depth100_13)
shapiro.test(residuals(test.lm33))
#p=0.00466
#still not normal

#non-parametric
kruskal.test(C_cont_in_interval_g_m2~land_use, data=depth100_13)
#p=0.5637


#C content for 190-200 cm
means<-with (depth200_13, tapply(C_cont_in_interval_g_m2, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(C_cont_in_interval_g_m2, land_use, sd, na.rm=TRUE))
ns<-with(depth200_13, tapply(C_cont_in_interval_g_m2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 729.41, soy: 840.00
round(ses, digits=2)
# forest: 291.00, soy: 201.01

#test for normality
test.lm33<-lm(C_cont_in_interval_g_m2~land_use, data=depth200_13)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.0014
#not normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(C_cont_in_interval_g_m2)~land_use, data=depth200_13)
shapiro.test(residuals(test.lm33))
#p=0.004395
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(C_cont_in_interval_g_m2)~land_use, data=depth200_13)
shapiro.test(residuals(test.lm33))
#p=0.001698
#still not normal

#non-parametric
kruskal.test(C_cont_in_interval_g_m2~land_use, data=depth200_13)
#p=0.7728




#summed content 0-200 cm for Table 1; this includes estimate of C storage in between sampled depths
#only use samples from the profile sites because only these samples include all depths
profiles2013<-mainBS2013[which(mainBS2013$profiles=="yes"),]

Ctotal<-tapply(profiles2013$C_cont_of_profile_g_m2, profiles2013$point, sum, na.rm=TRUE)
Ctotal

land_use_Ctotal<-factor(c("forest", "forest", "forest", "forest", "forest", "forest", "forest", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy", "soy"))
land_use_Ctotal

CtotalLU<-data.frame(Ctotal, land_use_Ctotal)


head(CtotalLU)
means<-with (CtotalLU, tapply(Ctotal, land_use_Ctotal, mean, na.rm=TRUE))
sds<-with(CtotalLU, tapply(Ctotal, land_use_Ctotal, sd, na.rm=TRUE))
ns<-with(CtotalLU, tapply(Ctotal, land_use_Ctotal, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
# forest: 22827.64, soy: 19387.99
round(ses, digits=2)
# forest: 6196.43, soy: 2446.90

#test for normality
test.lm33<-lm(Ctotal~land_use_Ctotal, data=CtotalLU)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.0056
#not normal; need non-parametric alternative

#try log transformation
test.lm33<-lm(log(Ctotal)~land_use_Ctotal, data=CtotalLU)
shapiro.test(residuals(test.lm33))
#p=0.01785
#still not normal

#try square root transformation
test.lm33<-lm(sqrt(Ctotal)~land_use_Ctotal, data=CtotalLU)
shapiro.test(residuals(test.lm33))
#p=0.00896
#still not normal

#non-parametric
kruskal.test(Ctotal~land_use_Ctotal, data=CtotalLU)
#p=0.3865


#######################################
#End of info from Table 1








######################################
#C storage

######
#regression of C content vs. % clay by depth

#C content vs % clay for 0-10 cm
C14.lm2<-lm(depth10_13$C_cont_in_interval_g_m2~depth10_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm2)

#test normality of residuals
shapiro.test(residuals(C14.lm2))
#p=0.24

summary(C14.lm2)
#intercept:p=0.00455, %clay:p=0.6899; multiple r2=0.00737; adj r2=-0.0377




#C content vs % clay for 10-20 cm
C14.lm2<-lm(depth20_13$C_cont_in_interval_g_m2~depth20_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm2)
#slightly non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm2))
#p=0.8388

summary(C14.lm2)
#intercept:p=0.103, %clay:p=0.609; multiple r2=0.1532; adj r2=-0.2702



#C content vs % clay for 40-50 cm
C14.lm2<-lm(depth50_13$C_cont_in_interval_g_m2~depth50_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm2)
#slightly non-normal; only 4 points

#test normality of residuals
shapiro.test(residuals(C14.lm2))
#p=0.29

summary(C14.lm2)
#intercept:p=0.0285, %clay:p=0.0697; multiple r2=0.8654; adj r2=-0.7982



######

##########
#ANCOVA of C content with % clay as covariate and land use

#for 0-10 cm
test.lm33 = lm(C_cont_in_interval_g_m2~X.clay, data=depth10_13) 
test.stdres33 = rstandard(test.lm33)

qqnorm(test.stdres33, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres33)
#pretty good

test.lm34 = lm(C_cont_in_interval_g_m2~land_use, data=depth10_13) 
test.stdres34 = rstandard(test.lm34)

qqnorm(test.stdres34, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres34)
#good

#with interaction
anc7<-aov(depth10_13$C_cont_in_interval~depth10_13$X.clay*depth10_13$land_use)
summary(anc7)
#clay p=0.451, land use p=0.033, interaction p=0.125

#without interaction
anc8<-aov(depth10_13$C_cont_in_interval~depth10_13$X.clay+depth10_13$land_use)
summary(anc8)
#clay p=0.620, land use p=0.051


#for 10-20 cm
test.lm2 = lm(C_cont_in_interval_g_m2~X.clay, data=depth20_13) 
test.stdres2 = rstandard(test.lm2)

qqnorm(test.stdres2, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres2)

#with interaction
anc7<-aov(depth20_13$C_cont_in_interval~depth20_13$X.clay*depth20_13$land_use)
summary(anc7)
#n=4; too few samples to say much

#without interaction
anc8<-aov(depth20_13$C_cont_in_interval~depth20_13$X.clay+depth20_13$land_use)
summary(anc8)
#clay p=0.45, land use p=0.237
#n=4; too few samples to say much


#for 40-50 cm depth
test.lm3 = lm(C_cont_in_interval_g_m2~X.clay, data=depth50_13) 
test.stdres3 = rstandard(test.lm3)

qqnorm(test.stdres3, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres3)

#with interaction
anc7<-aov(depth50_13$C_cont_in_interval~depth50_13$X.clay*depth50_13$land_use)
summary(anc7)
#n too small

#without interaction
anc8<-aov(depth50_13$C_cont_in_interval~depth50_13$X.clay+depth50_13$land_use)
summary(anc8)
#clay p=0.24, land use p=0.94
#n=4; too few samples to say much



#across all depths
test.lm6 = lm(C_cont_in_interval_g_m2~X.clay, data=mainBS2013) 
test.stdres6 = rstandard(test.lm6)

qqnorm(test.stdres6, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres6)
#pretty good


test.lm7 = lm(C_cont_in_interval_g_m2~land_use, data=mainBS2013) 
test.stdres7 = rstandard(test.lm7)

qqnorm(test.stdres7, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres7)
#could be better. take these next results with a grain of salt

#with interaction
anc17<-aov(mainBS2013$C_cont_in_interval~mainBS2013$X.clay*mainBS2013$land_use)
summary(anc17)
#clay p=0.0621, land use p=0.6914, interaction p=0.5162

#without interaction
anc18<-aov(mainBS2013$C_cont_in_interval~mainBS2013$X.clay+mainBS2013$land_use)
summary(anc18)
#clay p=0.06, land use p=0.689

#non-parametric alternative
sm.ancova(mainBS2013$X.clay,mainBS2013$C_cont_in_interval,mainBS2013$land_use, model="equal")
#p=0.481


##########

##################
#multiple regression: includes both % clay and # years in soybean cultivation

#across all depths 
lm4<-lm(mainBS2013$C_cont_in_interval~mainBS2013$X.clay+mainBS2013$yrssoyBSnn)
plot(lm4)
summary(lm4)
#clay p=0.122, yrs soy p=0.474, r2=0.1047, adj r2=0.056

#test normality of residuals
shapiro.test(residuals(lm4))
#p=0.39



#0-10 cm
lm8<-lm(depth10_13$C_cont_in_interval~depth10_13$X.clay+depth10_13$yrssoyBSnn)
plot(lm8)
summary(lm8)
#clay p=0.6123, yrs soy p=0.0396, r2=0.1925, adj r2=0.1156

#test normality of residuals
shapiro.test(residuals(lm8))
#p=0.95



#10-20 cm
lm12<-lm(depth20_13$C_cont_in_interval~depth20_13$X.clay+depth20_13$yrssoyBSnn)
plot(lm12)
summary(lm12)
#clay p=0.466, yrs soy p=0.389, r2=0.721, adj r2=0.1643

#test normality of residuals
shapiro.test(residuals(lm12))
#p=0.4631

##################

#end C storage
######################################





######################################
#percent C


########
#regression of % C with years in soybean cultivation


C14.lm<-lm(mainBS2013$perc_C~mainBS2013$yrssoyBSnn)
par(mfrow=c(2,2))
plot(C14.lm)
#looks good

#test normality of residuals
shapiro.test(residuals(C14.lm))
#p=1.378e-10

#quantile regression
qr10<-rq(mainBS2013$perc_C~mainBS2013$yrssoyBSnn, tau=0.5)
summ10<-summary(qr10, se="ker")
summ10
#intercept p=0.00003, yrssoyBSprofnn p=0.78022

scatterplot(mainBS2013$perc_C~mainBS2013$yrssoyBSnn, reg.line=FALSE)
#looks ok
plot(mainBS2013$perc_C~mainBS2013$yrssoyBSnn)


scatterplot(mainBS2013$perc_C~mainBS2013$X.clay, reg.line=FALSE)
#not great


#across all soil depths with % clay and % C
C14.lm4<-lm(mainBS2013$perc_C~mainBS2013$X.clay)
par(mfrow=c(2,2))
plot(C14.lm4)
#looks good

#test normality of residuals
shapiro.test(residuals(C14.lm4))
#p=0.0016

#quantile regression
qr4<-rq(mainBS2013$perc_C~mainBS2013$X.clay, tau=0.5)
summ4<-summary(qr4,se="ker")
summ4
#intercept: 1.58, mainBS2013$X.clay: -0.01
#intercept p=0.0018, mainBS2013$X.clay p=0.0969

sm.regression(mainBS2013$X.clay, mainBS2013$perc_C, model="linear")
#linear model is significant, p=0.007


#0-10 cm 
scatterplot(depth10_13$perc_C~depth10_13$X.clay, reg.line=FALSE)
#not great


C14.lm4<-lm(depth10_13$perc_C~depth10_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm4)
#slightly non-normal

summary(C14.lm4)
#intercept:p=0.0116, %clay:p=0.8581; multiple r2=0.00148; adj r2=-0.04


#10-20 cm
scatterplot(depth20_13$perc_C~depth20_13$X.clay, reg.line=FALSE)
#not great


C14.lm4<-lm(depth20_13$perc_C~depth20_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm4)
#slightly non-normal; n=9

#test normality of residuals
shapiro.test(residuals(C14.lm4))
#p=0.005305

#quantile regression
qr40<-rq(depth20_13$perc_C~depth20_13$X.clay, tau=0.5)
summ40<-summary(qr40, se="ker")
summ40
#intercept:p=0.3972, %clay:p=0.39374


#40-50 cm
scatterplot(depth50_13$perc_C~depth50_13$X.clay, reg.line=FALSE)
#not great; 

C14.lm<-lm(depth50_13$perc_C~depth50_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)

#test normality of residuals
shapiro.test(residuals(C14.lm))
#p=0.9176

summary(C14.lm)
#intercept:p=0.0139, %clay:p=0.4045; multiple r2=0.07, adj r2=-0.02

########

#############
#ANCOVA of % C with % clay as covariate


#0-10 cm

test.lm33 = lm(perc_C~X.clay, data=depth10_13) 
test.stdres33 = rstandard(test.lm33)

qqnorm(test.stdres33, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres33)
#pretty good

test.lm34 = lm(perc_C~land_use, data=depth10_13) 
test.stdres34 = rstandard(test.lm34)

qqnorm(test.stdres34, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres34)
#pretty good

#with interaction
anc2<-aov(depth10_13$perc_C~depth10_13$X.clay*depth10_13$land_use)
summary(anc2)
#clay p=0.81779, land use p=0.00149, interaction p=0.09287

#without interaction
anc1<-aov(depth10_13$perc_C~depth10_13$X.clay+depth10_13$land_use)
summary(anc1)
#clay p=0.8261; land use p=0.0021; 
#model shows that land use has a significant effect on %C which can be interpreted as a significant difference in 'intercepts' between the regression lines of soy and forest.

#10-20 cm

test.lm33 = lm(perc_C~X.clay, data=depth20_13) 
test.stdres33 = rstandard(test.lm33)

qqnorm(test.stdres33, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres33)
#pretty good, fewer samples

test.lm34 = lm(perc_C~land_use, data=depth20_13) 
test.stdres34 = rstandard(test.lm34)

qqnorm(test.stdres34, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres34)
#pretty good


#with interaction
anc2<-aov(depth20_13$perc_C~depth20_13$X.clay*depth20_13$land_use)
summary(anc2)
#clay p=0.0493, land use p=0.625, interaction p=0.7933

#without interaction
anc2<-aov(depth20_13$perc_C~depth20_13$X.clay+depth20_13$land_use)
summary(anc2)
#clay p=0.0356, land use p=0.6019, interaction p=0.7933



#across all depths
test.lm60 = lm(perc_C~X.clay, data=mainBS2013) 
test.stdres60 = rstandard(test.lm60)

qqnorm(test.stdres60, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres60)
#looks good


test.lm61 = lm(perc_C~land_use, data=mainBS2013) 
test.stdres61 = rstandard(test.lm61)

qqnorm(test.stdres61, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres61)
#not very good


#with interaction
anc11<-aov(mainBS2013$perc_C~mainBS2013$X.clay*mainBS2013$land_use)
summary(anc11)
#clay p=0.0201, land use p=0.0140, interaction p=0.6293

#without interaction
anc12<-aov(mainBS2013$perc_C~mainBS2013$X.clay+mainBS2013$land_use)
summary(anc12)
#clay p=0.0194; land use p=0.0134; 


#test normality of residuals
shapiro.test(residuals(anc11))
#p=0.02
shapiro.test(residuals(anc12))
#p=0.04

#non-parametric alternative
sm.ancova(mainBS2013$X.clay,mainBS2013$perc_C,mainBS2013$land_use,model="equal")
#p=0.035

#############


#################
#multiple regression of %C with years in soybean cultivation and %clay


#across all depths
lm2<-lm(perc_C~X.clay+yrssoyBSnn, data=mainBS2013)
plot(lm2)
summary(lm2)
#clay p=0.4493, yrs soy p=0.0171, r2=0.1526, adj r2=0.1265

#test normality of residuals
shapiro.test(residuals(lm2))
#p=0.06; not great, but ok


#0-10 cm
lm6<-lm(perc_C~X.clay+yrssoyBSnn, data=depth10_13)
plot(lm6)
summary(lm6)
#clay p=0.228, yrs soy p=0.002, r2=0.3645, adj r2=0.304

shapiro.test(residuals(lm6))
#p=0.83


#10-20 cm
lm10<-lm(perc_C~X.clay+yrssoyBSnn, data=depth20_13)
plot(lm10)
summary(lm10)
#clay p=0.0916, yrs soy p=0.8246, r2=0.4381, adj r2=0.2977

shapiro.test(residuals(lm10))
#p=0.004918; assumptions are violated- take this with a grain of salt

#################

#end percent C
#######################################





#######################################
#14C


########
#ANOVA of 14C for samples collected in 2013 

 
#across all depths
means<-with (mainBS2013, tapply(X.14C, year_conv, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(X.14C, year_conv, sd, na.rm=TRUE))
ns<-with(mainBS2013, tapply(X.14C, year_conv, length))
ses<-sds/sqrt(ns)
#forest values higher (less negative) than all soy; gradient with age in soy

round(means, digits=2)
#2003: -34.64, 2007: 69.23, 2007: -18.82, 2008: -69.73, 2020: -85.25
#note forest here is represented by the year 2020 (i.e., has not been converted to soy yet)

means<-with (mainBS2013, tapply(X.14C, land_use, mean, na.rm=TRUE))
round(means, digits=1)
#forest: -85.2, soy: -26.1


#by depth

#0-10 cm
means<-with (depth10_13, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 104.3, soy:63.2
round(ses, digits=1)
# forest: 6.0, soy: 3.4

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth10_13)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.3289

C10.aov<-aov(X.14C~land_use, data=depth10_13)
anova(C10.aov)
#p=3.937e-06




#10-20 cm
means<-with (depth20_13, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth20_13, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth20_13, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(0), max(80)), xpd=FALSE, ylab="14C of bulk soil, 10-20 cm, 2013", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")
#forest values higher than soy

round(means, digits=1)
#forest: 18.1, soy:8.5
round(ses, digits=1)
#forest: 17.6, soy: 18.2



#test for normality
test.lm33<-lm(X.14C~land_use, data=depth20_13)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.609

C20.aov<-aov(X.14C~land_use, data=depth20_13)
anova(C20.aov)
#p=0.8634




#40-50 cm
means<-with (depth50_13, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth50_13, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth50_13, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-150), max(0)), xpd=FALSE, ylab="14C of bulk soil, 40-50 cm, 2013", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")
#forest values lower than soy (more negative)

round(means, digits=1)
#forest: -112.4, soy: -105.7
round(ses, digits=1)
#forest: 6.5, soy: 9.5

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth50_13)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.9822

C50.aov<-aov(X.14C~land_use, data=depth50_13)
anova(C50.aov)
#p=0.8074



#90-100 cm
means<-with (depth100_13, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth100_13, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth100_13, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-300), max(0)), xpd=FALSE, ylab="14C of bulk soil, 90-100 cm, 2013", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")
#forest values higher than soy (less negative)

round(means, digits=1)
#forest: -216.9, soy: -250.6
round(ses, digits=1)
#forest: 11.4, 10.9

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth100_13)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.637

C100.aov<-aov(X.14C~land_use, data=depth100_13)
anova(C100.aov)
#p=0.3417



#190-200 cm
means<-with (depth200_13, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth200_13, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-450), max(0)), xpd=FALSE, ylab="14C of bulk soil, 190-200 cm, 2013", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")
#forest values higher than soy (less negative)

round(means, digits=1)
#forest: -361.4, soy: -382.2
round(ses, digits=1)
#forest: 24.0, soy: 9.5

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth200_13)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.4448

C200.aov<-aov(X.14C~land_use, data=depth200_13)
anova(C200.aov)
#p=0.6264




########


########
#ANOVA of 14C for samples collected in 2009

#across all depths
means2<-with (mainBS2009, tapply(X.14C, year_conv, mean, na.rm=TRUE))
sds2<-with(mainBS2009, tapply(X.14C, year_conv, sd, na.rm=TRUE))
ns2<-with(mainBS2009, tapply(X.14C, year_conv, length))
ses2<-sds2/sqrt(ns2)
b2<-barplot(means2, ylim=c(min(-300), max(0)), xpd=FALSE, ylab="14C of bulk soil collected in 2009", xlab="land use")
arrows(b2, means2+ses2, b2, means2-ses2, angle=90, code=3)
box(bty="l")
#forest values higher (less negative) than all soy

round(means2, digits=2)
#2003: -102.96, 2008: -186.88, 2020: -91.61
#...why is 2008 so low (-)? 

means2<-with (mainBS2009, tapply(X.14C, land_use, mean, na.rm=TRUE))
round(means2, digits=1)
#forest: -91.6, soy: -144.9


#0-10 cm
means<-with (depth10_09, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth10_09, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth10_09, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-200), max(200)), xpd=FALSE, ylab="14C, 2009 soils", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")

round(means, digits=1)
#forest:110.6, soy:27.2
round(ses, digits=1)
#forest: 8.0, soy: 49.5

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth10_09)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.97

C10.aov<-aov(X.14C~land_use, data=depth10_09)
anova(C10.aov)
#p=0.06284



#10-20 cm
means<-with (depth20_09, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth20_09, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth20_09, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-200), max(200)), xpd=FALSE, ylab="14C, 2009 soils", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")

round(means, digits=1)
#forest: 50.8, soy: 21.9
round(ses, digits=1)
#forest: 21.2, soy: 63.4

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth20_09)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.49

C20.aov<-aov(X.14C~land_use, data=depth20_09)
anova(C20.aov)
#p=0.5954



#for 40-50 cm
means<-with (depth50_09, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth50_09, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth50_09, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-200), max(200)), xpd=FALSE, ylab="14C, 2009 soils", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")

round(means, digits=1)
#forest: -69.8, soy: -143.1
round(ses, digits=1)
#forest: 9.6, soy: 44.2

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth50_09)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.967

C50.aov<-aov(X.14C~land_use, data=depth50_09)
anova(C50.aov)
#p=0.07507




#90-100 cm
means<-with (depth100_09, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth100_09, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth100_09, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-200), max(200)), xpd=FALSE, ylab="14C, 2009 soils", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")

round(means, digits=1)
#forest: -191.2, soy: -267.9
round(ses, digits=1)
#forest: 31.1, soy: 11.8 

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth100_09)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.602

C100.aov<-aov(X.14C~land_use, data=depth100_09)
anova(C100.aov)
#p=0.1796



#190-200 cm
means<-with (depth200_09, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(depth200_09, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(depth200_09, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)
b<-barplot(means, ylim=c(min(-500), max(100)), xpd=FALSE, ylab="14C, 2009 soils", xlab="land use")
arrows(b, means+ses, b, means-ses, angle=90, code=3)
box(bty="l")

round(means, digits=1)
#forest: -358.5, soy: -362.8
round(ses, digits=1)
#forest: 29.0, 40.9

#test for normality
test.lm33<-lm(X.14C~land_use, data=depth200_09)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.5939

C200.aov<-aov(X.14C~land_use, data=depth200_09)
anova(C200.aov)
#p=0.9354


########


########
#ANOVA of 14C in high or low clay soils

#across all depths
means<-with (profiles2013, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -142.0, low: -101.5




#0-10 cm
prof2013_10<-profiles2013[which(profiles2013$depth=="10"),]
means<-with (prof2013_10, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: 82.0, low: 103.1

#test for normality
test.lm33<-lm(X.14C~clay_cat, data=prof2013_10)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.2219

C.aov<-aov(X.14C~clay_cat, data=prof2013_10)
anova(C.aov)
#p=0.2037




#10-20 cm
prof2013_20<-profiles2013[which(profiles2013$depth=="20"),]
means<-with (prof2013_20, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -2.7, low: 29.3

#test for normality
test.lm33<-lm(X.14C~clay_cat, data=prof2013_20)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.5302

C2.aov<-aov(X.14C~clay_cat, data=prof2013_20)
anova(C2.aov)
#p=0.5617



#40-50 cm
prof2013_50<-profiles2013[which(profiles2013$depth=="50"),]
means<-with (prof2013_50, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -128, low: -90

#test for normality
test.lm33<-lm(X.14C~clay_cat, data=prof2013_50)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.6084

C3.aov<-aov(X.14C~clay_cat, data=prof2013_50)
anova(C3.aov)
#p=0.1318



#90-100 cm
prof2013_100<-profiles2013[which(profiles2013$depth=="100"),]
means<-with (prof2013_100, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -265.1, low: -202.4

#test for normality
test.lm33<-lm(X.14C~clay_cat, data=prof2013_100)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.2466

C4.aov<-aov(X.14C~clay_cat, data=prof2013_100)
anova(C4.aov)
#p=0.04209



#190-200 cm
prof2013_200<-profiles2013[which(profiles2013$depth=="200"),]
means<-with (prof2013_200, tapply(X.14C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -396.3, low: -347.4

#test for normality
test.lm33<-lm(X.14C~clay_cat, data=prof2013_200)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.5081

C5.aov<-aov(X.14C~clay_cat, data=prof2013_200)
anova(C5.aov)
#p=0.2246

########


############
#regression for 14C

#% clay vs. 14 from samples collected in 2009
#across all depths
C14.lm<-lm(mainBS2009$X.14C~mainBS2009$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)
#slightly non-normal, not bad

#test for normality
shapiro.test(residuals(C14.lm))
#p=0.2398

summary(C14.lm)
#clay p=0.07, R2 adj=0.1368


###can't do the same for 2013; no clay data for 2013



#14C vs. years in soy cultivation for soils collected in 2013
#across all depths
C14.lm<-lm(mainBS2013$X.14C~yrssoyBSnn)
par(mfrow=c(2,2))
plot(C14.lm)
#slightly non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm))
#p=1.77e-05

#quantile regression
qr5<-rq(mainBS2013$X.14C~yrssoyBSnn, tau=0.5)
summ5<-summary(qr5,se="ker")
summ5
#intercept: -33, yrssoyBSnn: 9.88
#intercept p=0.62, yrssoyBSnn p=0.33


#14C vs. years in %clay for soils collected in 2013
#across all depths
C14.lm<-lm(mainBS2013$X.14C~mainBS2013$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)
#very non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm))
#p=2.98e-06

#quantile regression
qr6<-rq(mainBS2013$X.14C~mainBS2013$X.clay, tau=0.5)
summ6<-summary(qr6,se="ker")
summ6
#intercept p=0.69, main2013BS$X.clay p=0.92



#0-10 cm
C14.lm<-lm(depth10_13$X.14C~depth10_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)
#looks good

shapiro.test(residuals(C14.lm))
#p=0.602

summary(C14.lm)
#intercept:p=5.63e-07, %clay:p=0.0232; multiple r2=0.2129, adj r2=0.1771



#10-20 cm
C14.lm<-lm(depth20_13$X.14C~depth20_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)
#slightly non-normal; only 4 points

shapiro.test(residuals(C14.lm))
#p=0.8465

summary(C14.lm)
#intercept:p=0.368, %clay:p=0.729; multiple r2=0.073, adj r2=-0.39



#40-50 cm
C14.lm<-lm(depth50_13$X.14C~depth50_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm)
#slightly non-normal; only 4 points

shapiro.test(residuals(C14.lm))
#p=0.9604

summary(C14.lm)
#intercept:p=0.0799, %clay:p=0.057; multiple r2=0.8893, adj r2=0.834

############



#################
#ANCOVA for 14C

#0-10 cm depth
anc3<-aov(depth10_13$X.14C~depth10_13$X.clay*depth10_13$land_use)
summary(anc3)
#clay p=0.004; land use p=0.0004, interaction p=0.794

anc4<-aov(depth10_13$X.14C~depth10_13$X.clay+depth10_13$land_use)
summary(anc4)
#clay p=0.00349, land use p=0.000274
#use this one


test.lm33 = lm(X.14C~land_use, data=depth10_13) 
test.stdres33 = rstandard(test.lm33)

qqnorm(test.stdres33, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres33)
#not bad

test.lm33 = lm(X.14C~X.clay, data=depth10_13) 
test.stdres33 = rstandard(test.lm33)

qqnorm(test.stdres33, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres33)
#a bit better

shapiro.test(residuals(anc3))
#p=0.189
shapiro.test(residuals(anc4))
#p=0.2179




#40-50 cm
anc2<-aov(depth50_13$X.14C~depth50_13$X.clay*depth50_13$land_use)
summary(anc2)
#n too small

anc2<-aov(depth50_13$X.14C~depth50_13$X.clay+depth50_13$land_use)
summary(anc2)
#clay p=0.13, land use p=0.399




#across all depths
test.lm62 = lm(X.14C~X.clay, data=mainBS2013) 
test.stdres62 = rstandard(test.lm62)

qqnorm(test.stdres62, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres62)
#could be better

test.lm63 = lm(X.14C~land_use, data=mainBS2013) 
test.stdres63 = rstandard(test.lm63)

qqnorm(test.stdres63, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres63)
#could be better

#with interaction
anc13<-aov(mainBS2013$X.14C~mainBS2013$X.clay*mainBS2013$land_use)
summary(anc13)
#clay p=0.337; land use p=0.147, interaction p=0.379

#without interaction
anc14<-aov(mainBS2013$X.14C~mainBS2013$X.clay+mainBS2013$land_use)
summary(anc14)
#clay p=0.336, land use p=0.146

shapiro.test(residuals(anc13))
#p=8.906e-05

shapiro.test(residuals(anc14))
#p=0.0001373

#non-parametric alternative
sm.ancova(mainBS2013$X.clay,mainBS2013$X.14C,mainBS2013$land_use, model="equal")
#p=0.0801

#################




#########################
#multiple regression of 14C with years in soy cultivation and % clay


#across all depths
lm1<-lm(X.14C~X.clay+yrssoyBSnn, data=mainBS2013)
plot(lm1)

summary(lm1)
#clay p=0.168, yrs soy p=0.163, r2=0.07, adj r2=0.02

#test normality of residuals
shapiro.test(residuals(lm1))
#p=0.0001425

#quantile regression
qr7<-rq(X.14C~X.clay+yrssoyBSnn, data=mainBS2013, tau=0.5)
summ7<-summary(qr7,se="ker")
summ7
#intercept p=0.78, X.clay p=0.72, yrssoyBSnn p=0.56



#0-10 cm
lm5<-lm(X.14C~X.clay+yrssoyBSnn, data=depth10_13)
plot(lm5)
summary(lm5)
#clay p=0.1732, yrs soy p=0.0167, r2=0.4047, adj r2=0.348

shapiro.test(residuals(lm5))
#p=0.009

#quantile regression
qr1<-rq(depth10_13$X.14C~depth10_13$X.clay+depth10_13$yrssoyBSnn,tau=0.5)
summ1<-summary(qr1, se="ker")
summ1
#clay p=0.66
#yrs soy =0.14

sm.regression(depth10_13$X.14C,depth10_13$X.clay, model="linear")
#p=0.408
sm.regression(depth10_13$X.14C,depth10_13$yrssoyBSnn, model="linear")
#p=0.026



#10-20 cm
lm9<-lm(X.14C~X.clay+yrssoyBSnn, data=depth20_13)
plot(lm9)
summary(lm9)
#clay p=0.73, yrs soy p=0.69, r2=0.27, adj r2=-1.18

shapiro.test(residuals(lm9))
#p=0.4631


#########################

#end 14C
######################################



######################################
#13C


########
#ANOVA of 13C by land use for samples collected in 2013


#0-10 cm
means<-with (depth10_13, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -28.51, soy: -23.56
round(ses, digits=2)
#forest: 0.17, soy: 0.21

test.lm33 = lm(d13C~land_use, data=depth10_13)

#test for normality
shapiro.test(residuals(test.lm33))
#p=0.63

C.aov<-aov(d13C~land_use, data=depth10_13)
anova(C.aov)
#p=2.833e-13



#10-20 cm
means<-with (depth20_13, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth20_13, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth20_13, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -26.67, soy: -24.39
round(ses, digits=2)
#forest: 0.31, soy: 0.16

test.lm33 = lm(d13C~land_use, data=depth20_13)

#test for normality
shapiro.test(residuals(test.lm33))
#p=0.88

C.aov<-aov(d13C~land_use, data=depth20_13)
anova(C.aov)
#p=3.945e-07




#40-50 cm
means<-with (depth50_13, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth50_13, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth50_13, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -24.96, soy: -23.77
round(ses, digits=2)
#forest: 0.27, soy: 0.21


test.lm33 = lm(d13C~land_use, data=depth50_13) 
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.42

C.aov<-aov(d13C~land_use, data=depth50_13)
anova(C.aov)
#p=0.01066




#90-100 cm
means<-with (depth100_13, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth100_13, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth100_13, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -24.09, soy: -23.39
round(ses, digits=2)
#forest: 0.24, 0.26

test.lm33 = lm(d13C~land_use, data=depth100_13) 
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.07234; not great, but ok

C.aov<-aov(d13C~land_use, data=depth100_13)
anova(C.aov)
#p=0.2038




#190-200 cm
means<-with (depth200_13, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth200_13, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -23.73, soy: -22.77
round(ses, digits=2)
#forest: 0.33, soy: 0.28

test.lm33 = lm(d13C~land_use, data=depth200_13) 
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.07353; not great, but ok

C.aov<-aov(d13C~land_use, data=depth200_13)
anova(C.aov)
#p=0.1161



#across all depths
means<-with (mainBS2013, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(mainBS2013, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -25.59, soy: -23.57
round(ses, digits=2)
#forest: 0.33, soy: 0.11

test.lm33 = lm(d13C~land_use, data=mainBS2013) 
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.1123

C.aov<-aov(d13C~land_use, data=mainBS2013)
anova(C.aov)
#p=5.885e-12

########


########
#ANOVAs for 13C with land use for samples collected in 2009

#0-10 cm
means<-with (depth10_09, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth10_09, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth10_09, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -27.55, soy: -23.10
round(ses, digits=2)
#forest: 0.43, soy: 1.70

test.lm33 = lm(d13C~land_use, data=depth10_09)
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.9697

C.aov<-aov(d13C~land_use, data=depth10_09)
anova(C.aov)
#p=0.02196



#10-20 cm
means<-with (depth20_09, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth20_09, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth20_09, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -26.5, soy: -23.9
round(ses, digits=2)
#forest: 0.53, soy:0.50


test.lm33 = lm(d13C~land_use, data=depth20_09)
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.28

C.aov<-aov(d13C~land_use, data=depth20_09)
anova(C.aov)
#p=0.03868



#40-50 cm
means<-with (depth50_09, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth50_09, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth50_09, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -25.15, soy: -23.95
round(ses, digits=2)
#forest: 0.32. soy: 0.35


test.lm33 = lm(d13C~land_use, data=depth50_09)
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.0178

kruskal.test(d13C~land_use, data=depth50_09)
#p=0.1588




#90-100 cm
means<-with (depth100_09, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth100_09, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth100_09, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -24.3, soy: -23.3
round(ses, digits=2)
#forest: 0.38, soy:0.40

test.lm33 = lm(d13C~land_use, data=depth100_09)
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.6749

C.aov<-aov(d13C~land_use, data=depth100_09)
anova(C.aov)
#p=0.1795




#190-200 cm
means<-with (depth200_09, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(depth200_09, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(depth200_09, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=2)
#forest: -23.15, soy: -22.50
round(ses, digits=2)
#forest: 0.49, 0.80

test.lm33 = lm(d13C~land_use, data=depth200_09)
#test for normality
shapiro.test(residuals(test.lm33))
#p=0.1303

C.aov<-aov(d13C~land_use, data=prof2009_200)
anova(C.aov)
#p=0.5011


########


########
#comparison of means of 13C by clay category
#soils collected in 2013

#0-10 cm
means<-with (depth10_13, tapply(d13C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -26.1, low: -25.5

#10-20 cm
means<-with (depth20_13, tapply(d13C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -25.6, low: -25.0

#40-50 cm
means<-with (depth50_13, tapply(d13C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -24.2, low: -23.7

#90-100 cm
means<-with (depth100_13, tapply(d13C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -23.5, low: -23.0

#190-200 cm
means<-with (depth200_13, tapply(d13C, clay_cat, mean, na.rm=TRUE))
round(means, digits=1)
#high: -22.5, low: -23.0

########



##############
#regression of 13C with years in cultivation


#across all depths
C14.lm3<-lm(mainBS2013$d13C~yrssoyBSnn)
par(mfrow=c(2,2))
plot(C14.lm3)
#looks good

#test normality of residuals
shapiro.test(residuals(C14.lm3))
#p=0.38

summary(C14.lm3)
#intercept:p<2e-16, yrssoyBSnn:p=1.28e-07; reject the null that the slope equals zero, so years in soy has a sig. effect on the BS 13C




means<-with (main2013BS, tapply(d13C, year_conv, mean, na.rm=TRUE))
sds<-with(main2013BS, tapply(d13C, year_conv, sd, na.rm=TRUE))
ns<-with(main2013BS, tapply(d13C, year_conv, length))
ses<-sds/sqrt(ns)
round(means, digits=1)
#2003: -23.5, 2004: -23.7, 2007: -24.1, 2008: -22.9, forest: -25.6
round(ses, digits=1)
#2003, 2004, 2007, 2008: 0.2, forest: 0.3



#sub soy only and run ANOVA for differences in years converted
soyzz<-mainBS2013[which(mainBS2013$land_use=="soy"),]
#test for normality
test.lm33<-lm(d13C~year_conv, data=soyzz)
par(mfrow=c(2,2))
plot(test.lm33)
#looks good

shapiro.test(residuals(test.lm33))
#p=0.002563

kruskal.test(d13C~year_conv,data=soyzz)
#p=0.001804





#0-10 cm
means<-with (depth10_13, tapply(d13C, year_conv, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(d13C, year_conv, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(d13C, year_conv, length))
ses<-sds/sqrt(ns)
round(means, digits=1)
#2003: -23.3, 2004: -24.2, 2007: -23.7, 2008: -23.0, forest: -28.5
round(ses, digits=1)
#2003: 0.3, 2004, 2007, 2008: 0.4, forest: 0.2


C14.lm9<-lm(depth10_13$d13C~depth10_13$yrssoyBSnn)
par(mfrow=c(2,2))
plot(C14.lm9)
#slightly non-normal and weird things with residuals...very small n for 13C...ok for 14C

#test normality of residuals
shapiro.test(residuals(C14.lm9))
#p=0.0495

#quantile regression
qr1<-rq(depth10_13$d13C~depth10_13$yrssoyBSnn,tau=0.5)
summ1<-summary(qr1, se="ker")
summ1
#intercept: -28.1, yrssoyBS10nn: 0.55
#intercept p=0.0000, yrssoyBS10nn p=0.00127






##############


##############
#regression of 13C vs. %clay
#across all depths
C14.lm3<-lm(mainBS2013$d13C~mainBS2013$X.clay)
par(mfrow=c(2,2))
plot(C14.lm3)
#slightly non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm3))
#p=9.348e-05

#quantile regression
qr2<-rq(mainBS2013$d13C~mainBS2013$X.clay, tau=0.5)
summ2<-summary(qr2,se="ker")
summ2
#intercept: -25.22, main2013BS$X.clay: 0.03
#intercept p=0.0000, main2013BS$X.clay p=0.09676

sm.regression(mainBS2013$X.clay,mainBS2013$d13C, model="linear")
#p=0.082




#0-10 cm
C14.lm3<-lm(depth10_13$d13C~depth10_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm3)
#slightly non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm3))
#p=0.006

#quantile regression
qr12<-rq(depth10_13$d13C~depth10_13$X.clay, tau=0.5)
summ12<-summary(qr12, se="ker")
summ12
#intercept p=0.000, depth10_13$X.clay p=0.93



#10-20 cm
C14.lm3<-lm(depth20_13$d13C~depth20_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm3)
#slightly non-normal, few outliers

#test normality of residuals
shapiro.test(residuals(C14.lm3))
#p=0.14

summary(C14.lm3)
#intercept:p=7.31, %clay:p=0.141; multiple r2=0.2245; adj r2=-0.1383



#40-50 cm
C14.lm3<-lm(depth50_13$d13C~depth50_13$X.clay)
par(mfrow=c(2,2))
plot(C14.lm3)
#slightly non-normal

#test normality of residuals
shapiro.test(residuals(C14.lm3))
#p=0.4949

summary(C14.lm3)
#intercept:p=6.118e-11, %clay:p=0.0454; multiple r2=0.3744; adj r2=0.3049

##############




#####################
#ANCOVA of 13C with % clay and land use

#0-10 cm
test.lm30 = lm(d13C~X.clay, data=depth10_13) 
test.stdres30 = rstandard(test.lm30)

qqnorm(test.stdres30, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres30)
#not bad

test.lm31 = lm(d13C~land_use, data=depth10_13) 
test.stdres31 = rstandard(test.lm31)

qqnorm(test.stdres31, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres31)
#not bad


#with interaction
anc5<-aov(depth10_13$d13C~depth10_13$X.clay*depth10_13$land_use)
summary(anc5)
#clay p=0.385, land use p=2.23e-08, interaction p=0.485

anc6<-aov(depth10_13$d13C~depth10_13$X.clay+depth10_13$land_use)
summary(anc6)
#clay p=0.379, land use p=1.21e-08

shapiro.test(residuals(anc5))
#p=0.3343
shapiro.test(residuals(anc6))
#p=0.4192




#10-20 cm
test.lm = lm(d13C~X.clay, data=depth20_13) 
test.stdres = rstandard(test.lm)

qqnorm(test.stdres, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres)
#not bad...few that are off of the line

test.lm2 = lm(d13C~land_use, data=depth20_13) 
test.stdres2 = rstandard(test.lm2)

qqnorm(test.stdres2, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres2)
#looks pretty good

#with interaction
anc2<-aov(depth20_13$d13C~depth20_13$X.clay*depth20_13$land_use)
summary(anc2)
#clay p=0.00605, land use p=0.00028, interaction p=0.634

#without interaction
anc23<-aov(depth20_13$d13C~depth20_13$X.clay+depth20_13$land_use)
summary(anc23)
#clay p=0.0035, land use p=0.0001

shapiro.test(residuals(anc2))
#p=0.8393
shapiro.test(residuals(anc23))
#p=0.7895




#40-50 cm
#with interaction
anc2<-aov(depth50_13$d13C~depth50_13$X.clay*depth50_13$land_use)
summary(anc2)
#clay p=0.0372, land use p=0.0897, interaction p=0.7242

#without interaction
anc23<-aov(depth50_13$d13C~depth50_13$X.clay+depth50_13$land_use)
summary(anc23)
#clay p=0.026, land use p=0.0706


shapiro.test(residuals(anc2))
#p=0.129
shapiro.test(residuals(anc23))
#p=0.0839



#across all depths
test.lm4 = lm(d13C~X.clay, data=mainBS2013) 
test.stdres4 = rstandard(test.lm4)

qqnorm(test.stdres4, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres4)
#not bad

test.lm5 = lm(d13C~land_use, data=mainBS2013) 
test.stdres5 = rstandard(test.lm5)

qqnorm(test.stdres5, 
       ylab="Standardized Residuals", 
       xlab="Normal Scores") 
qqline(test.stdres5)
#looks quite good


anc15<-aov(mainBS2013$d13C~mainBS2013$X.clay*mainBS2013$land_use)
summary(anc15)
#clay p=2.15e-05, land use p=9.16e-07, interaction p=0.547

anc16<-aov(mainBS2013$d13C~mainBS2013$X.clay+mainBS2013$land_use)
summary(anc16)
#clay p=1.94e-05, land use p=7.99e-07

shapiro.test(residuals(anc15))
#p=0.2214
shapiro.test(residuals(anc16))
#p=0.5071

#####################



############################
#multiple regression of 13C with years of cultivation and % clay

#across all depths
lm3<-lm(d13C~X.clay+yrssoyBSnn, data=mainBS2013)
plot(lm3)
summary(lm3)
#clay p=0.14, yrs soy p=9.38e-05, r2=0.355, adj r2=0.335

#test normality of residuals
shapiro.test(residuals(lm3))
#p=0.38



#0-10 cm
lm7<-lm(d13C~X.clay+yrssoyBSnn, data=depth10_13)
plot(lm7)
summary(lm7)
#clay p=0.17, yrs soy p=0.0001, r2=0.5149, adj r2=0.4687

#test normality of residuals
shapiro.test(residuals(lm7))
#p=0.47



#10-20 cm
lm11<-lm(d13C~X.clay+yrssoyBSnn, data=depth20_13)
plot(lm11)
summary(lm11)
#clay p=0.7846, yrs soy p=0.0011, r2=0.8073, adj r2=0.7591

#test normality of residuals
shapiro.test(residuals(lm11))
#p=0.96

############################

#end 13C
########################################



################################################
# % C3 or C4 mixing model

#across all depths

#%C4 in 2009
means<-with (mainBS2009, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2009, tapply(X.C4_depth_correct, land_use, sd))
ns<-with(mainBS2009, tapply(X.C4_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: 0.0, soy 12.4
round (ses, digits=1)
#forest: 1.1, soy: 3.9




#%C3 in 2009
means<-with (mainBS2009, tapply(X.C3_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2009, tapply(X.C3_depth_correct, land_use, sd))
ns<-with(mainBS2009, tapply(X.C3_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: 100.0, soy 87.6
round (ses, digits=1)
#forest: 1.1, soy: 3.9



#%C4 in 2013
means<-with (mainBS2013, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(X.C4_depth_correct, land_use, sd))
ns<-with(mainBS2013, tapply(X.C4_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -1.6, soy 11.0
round (ses, digits=1)
#forest: 0.7, soy: 1.0




#%C3 in 2013
means<-with (mainBS2013, tapply(X.C3_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(mainBS2013, tapply(X.C3_depth_correct, land_use, sd))
ns<-with(mainBS2013, tapply(X.C3_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: 101.6, soy 89.0
round (ses, digits=1)
#forest: 0.7, soy: 1.0



#0-10 cm for soils collected in 2009
means<-with (depth10_09, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
round(means, digits=1)
#forest: 2.8, soy 30.6
sds<-with(depth10_09, tapply(X.C4_depth_correct, land_use, sd))
ns<-with(depth10_09, tapply(X.C4_depth_correct, land_use, length))
ses<-sds/sqrt(ns)
round (ses, digits=1)
#forest:2.7, soy:10.6



#190-200 cm for soils collected in 2009
means<-with (depth200_09, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
round(means, digits=1)
#forest: 0.2, soy 4.3



#0-10 cm for soils collected in 2013
means<-with (depth10_13, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(X.C4_depth_correct, land_use, sd))
ns<-with(depth10_13, tapply(X.C4_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -3.2, soy 27.8
round (ses, digits=1)
#forest:1.1, soy: 1.3




#190-200 cm for soils collected in 2013
means<-with (depth200_13, tapply(X.C4_depth_correct, land_use, mean, na.rm=TRUE))
sds<-with(depth200_13, tapply(X.C4_depth_correct, land_use, sd))
ns<-with(depth200_13, tapply(X.C4_depth_correct, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -3.4, soy 2.6
round (ses, digits=1)
#forest: 2,0, soy: 1.8


#end % C3 or C4 mixing model
################################################




#################################################
#correlation of clay with years in soy cultivation
yrssoyBS<-2013-mainBS2013$year_conv
yrssoyBSn<-gsub("-7", "0", yrssoyBS)
yrssoyBSnn<-as.numeric(yrssoyBSn)

X.claynn<-as.numeric(mainBS2013$X.clay)
X.claynn

#parametric correlation
cor(X.claynn, yrssoyBSnn, method="spearman",use="pairwise.complete.obs")
#0.622

plot (X.claynn~yrssoyBSnn)

lm.zzz<-lm(X.claynn~yrssoyBSnn)

#test normality of residuals
shapiro.test(residuals(lm.zzz))
#p=0.0994; ok, but not great

summary(lm.zzz)
#multiple R2=0.2722, adj r2=0.2612, p=5.05e-06





#use soy sites only; no forest
#same as above but using year converted rather than years in soy
soyzz<-mainBS2013[which(mainBS2013$land_use=="soy"),]

cor(soyzz$X.clay,soyzz$year_conv,method="pearson", use="complete.obs")
#-0.698231

cor.test(soyzz$X.clay,soyzz$year_conv,method="pearson", use="complete.obs")
#p=2.203e-09


#end correlation of clay with years in soy cultivation
#################################################