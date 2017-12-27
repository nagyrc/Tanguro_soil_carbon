#this code was written by Dr. Chelsea Nagy (University of Colorado- Boulder) for the publication "Soil carbon dynamics in soybean cropland and forests in Mato Grosso, Brazil".

#analysis on CO2 samples only

library(ggplot2)
library(doBy)
library(sm)
library(quantreg)
#library(car)


#############################################
CO2<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/final_code_and_data/data/CO2_conce_data.csv")

#subset data by depth sampled
depth0_10<-CO2[which(CO2$depth=="0-10"),]
depth40_50<-CO2[which(CO2$depth=="40-50"),]
depth190_200<-CO2[which(CO2$depth=="190-200"),]
depth10_20<-CO2[which(CO2$depth=="10-20"),]
depth90_100<-CO2[which(CO2$depth=="90-100"),]

#to make Table 2
################################
means<-with (depth0_10, tapply(C_evolv_mgC_gsoil_day, landuse, mean, na.rm=TRUE))
sds<-with(depth0_10, tapply(C_evolv_mgC_gsoil_day, landuse, sd, na.rm=TRUE))
ns<-with(depth0_10, tapply(C_evolv_mgC_gsoil_day, landuse, length))
ses<-sds/sqrt(ns)

round(means, digits=3)
#forest: 0.035, soy: 0.012
round(ses, digits=3)
#forest: 0.004, soy: 0.001

#test for normality
test.lm33<-lm(C_evolv_mgC_gsoil_day~landuse, data=depth0_10)
shapiro.test(residuals(test.lm33))
#p=4.563e-08

#try transformations
test.lm33<-lm(log(C_evolv_mgC_gsoil_day)~landuse, data=depth0_10)
shapiro.test(residuals(test.lm33))
#p=0.002038

test.lm33<-lm(sqrt(C_evolv_mgC_gsoil_day)~landuse, data=depth0_10)
shapiro.test(residuals(test.lm33))
#p=0.002783

#non-parametric alternative
kruskal.test(C_evolv_mgC_gsoil_day~landuse, data=depth0_10)
#p=5.217e-09



#40-50 cm
means<-with (depth40_50, tapply(C_evolv_mgC_gsoil_day, landuse, mean, na.rm=TRUE))
sds<-with(depth40_50, tapply(C_evolv_mgC_gsoil_day, landuse, sd, na.rm=TRUE))
ns<-with(depth40_50, tapply(C_evolv_mgC_gsoil_day, landuse, length))
ses<-sds/sqrt(ns)

round(means, digits=4)
#forest: 0.0014, soy: 0.0016
round(ses, digits=5)

#test normality
test.lm33<-lm(C_evolv_mgC_gsoil_day~landuse, data=depth40_50)
shapiro.test(residuals(test.lm33))
#p=1.89e-11

#try transformations
test.lm33<-lm(log(C_evolv_mgC_gsoil_day)~landuse, data=depth40_50)
shapiro.test(residuals(test.lm33))
#p=0.013

test.lm33<-lm(sqrt(C_evolv_mgC_gsoil_day)~landuse, data=depth40_50)
shapiro.test(residuals(test.lm33))
#p=3.206e-07

#non-parametric alternative
kruskal.test(C_evolv_mgC_gsoil_day~landuse, data=depth40_50)
#p=0.6043



#90-100 cm
means<-with (depth90_100, tapply(C_evolv_mgC_gsoil_day, landuse, mean, na.rm=TRUE))
sds<-with(depth90_100, tapply(C_evolv_mgC_gsoil_day, landuse, sd, na.rm=TRUE))
ns<-with(depth90_100, tapply(C_evolv_mgC_gsoil_day, landuse, length))
ses<-sds/sqrt(ns)

round(means, digits=5)
#forest: 0.00044, soy: 0.00055
round(ses, digits=5)
#forest: 0.00009, soy: 0.00011

#test normality
test.lm33<-lm(C_evolv_mgC_gsoil_day~landuse, data=depth90_100)
shapiro.test(residuals(test.lm33))
#p=4.599e-07

#try transformations
test.lm33<-lm(log(C_evolv_mgC_gsoil_day)~landuse, data=depth90_100)
shapiro.test(residuals(test.lm33))
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : NA/NaN/Inf in 'y'

test.lm33<-lm(sqrt(C_evolv_mgC_gsoil_day)~landuse, data=depth90_100)
shapiro.test(residuals(test.lm33))
#p=0.07

#non-parametric alternative
kruskal.test(C_evolv_mgC_gsoil_day~landuse, data=depth90_100)
#p=0.3676



#190-200 cm
means<-with (depth190_200, tapply(C_evolv_mgC_gsoil_day, landuse, mean, na.rm=TRUE))
sds<-with(depth190_200, tapply(C_evolv_mgC_gsoil_day, landuse, sd, na.rm=TRUE))
ns<-with(depth190_200, tapply(C_evolv_mgC_gsoil_day, landuse, length))
ses<-sds/sqrt(ns)

round(means, digits=5)
#forest: 0.00052, soy: 0.00065
round(ses, digits=5)
#forest: 0.00010, soy: 0.00011

#test normality
test.lm33<-lm(C_evolv_mgC_gsoil_day~landuse, data=depth190_200)
shapiro.test(residuals(test.lm33))
#p=1.761e-12

#try transformations
test.lm33<-lm(log(C_evolv_mgC_gsoil_day)~landuse, data=depth190_200)
shapiro.test(residuals(test.lm33))
#Error in lm.fit(x, y, offset = offset, singular.ok = singular.ok, ...) : NA/NaN/Inf in 'y'

test.lm33<-lm(sqrt(C_evolv_mgC_gsoil_day)~landuse, data=depth190_200)
shapiro.test(residuals(test.lm33))
#p=0.0001573

#non-parametric alternative
kruskal.test(C_evolv_mgC_gsoil_day~landuse, data=depth190_200)
#p=0.6268


###############################
#End of Table 2



###############################
#to calculate time to respire all C in sample (values not reported in manuscript table)

time<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/final_code_and_data/data/time_to_respire_C.csv")

head(time)
str(time)
summary(time$depth)

time10<-time[which(time$depth==10),]
time50<-time[which(time$depth==50),]
time100<-time[which(time$depth==100),]
time200<-time[which(time$depth==200),]


#0-10 cm
means<-with (time10, tapply(timetoresp_yr, land_use, mean, na.rm=TRUE))
sds<-with(time10, tapply(timetoresp_yr, land_use, sd, na.rm=TRUE))
ns<-with(time10, tapply(timetoresp_yr, land_use, length))
ses<-sds/sqrt(ns)

means
#forest=1.902, soy=3.513
ses
#forest=0.237, soy=1.150

#40-50 cm
means<-with (time50, tapply(timetoresp_yr, land_use, mean, na.rm=TRUE))
sds<-with(time50, tapply(timetoresp_yr, land_use, sd, na.rm=TRUE))
ns<-with(time50, tapply(timetoresp_yr, land_use, length))
ses<-sds/sqrt(ns)

means
#forest=23.735, soy=16.761
ses
#forest=7.255, soy=4.709

#90-100 cm
means<-with (time100, tapply(timetoresp_yr, land_use, mean, na.rm=TRUE))
sds<-with(time100, tapply(timetoresp_yr, land_use, sd, na.rm=TRUE))
ns<-with(time100, tapply(timetoresp_yr, land_use, length))
ses<-sds/sqrt(ns)

means
#forest=41.377, soy=24.7845
ses
#forest=21.171, soy=9.305

#190-200 cm
means<-with (time200, tapply(timetoresp_yr, land_use, mean, na.rm=TRUE))
sds<-with(time200, tapply(timetoresp_yr, land_use, sd, na.rm=TRUE))
ns<-with(time200, tapply(timetoresp_yr, land_use, length))
ses<-sds/sqrt(ns)

means
#forest=30.667, soy=25.195
ses
#forest=19.086, soy=15.096



#############################################





###################################
#start Table 3: CO2 and 14C


C14<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/final_code_and_data/data/Tanguro_14C_data_for_analysis_CO2.csv")
str(C14)
head(C14)

#subset to only samples that were not acidified
main<-C14[which(C14$acidified=="no"),]

#subset by year samples were collected
main2009<-main[which(main$year_samp=="2009"),]
main2013<-main[which(main$year_samp=="2013"),]

#subset to only CO2 samples, not bulk soil
main2013CO2<-main2013[which(main2013$samle=="CO2"),]

#subset by depth
CO2_depth10<-main2013CO2[which(main2013CO2$depth=="10"),]
CO2_depth20<-main2013CO2[which(main2013CO2$depth=="20"),]
CO2_depth50<-main2013CO2[which(main2013CO2$depth=="50"),]
CO2_depth100<-main2013CO2[which(main2013CO2$depth=="100"),]
CO2_depth200<-main2013CO2[which(main2013CO2$depth=="200"),]

#create variable for number of years since soybean conversion
yrssoyCO2<-2013-main2013CO2$year_conv
yrssoyCO2n<-gsub("-7", "0", yrssoyCO2)
#make numeric
yrssoyCO2nn<-as.numeric(yrssoyCO2n)
yrssoyCO2nn





####CO2 and 14C by depth

#0-10 cm
means<-with (CO2_depth10, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(CO2_depth10, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(CO2_depth10, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: 73.8, soy: -28.5
ses
#forest: 4.9, soy: 17.9

#test for normality 
test.lm33<-lm(X.14C~land_use, data=CO2_depth10)
shapiro.test(residuals(test.lm33))
#p=0.04

#try transformations
test.lm33<-lm(log(X.14C+100)~land_use, data=CO2_depth10)
shapiro.test(residuals(test.lm33))
#p=0.0016

test.lm33<-lm(sqrt(X.14C+100)~land_use, data=CO2_depth10)
shapiro.test(residuals(test.lm33))
#p=0.007

#use non-parametric alternative
kruskal.test(X.14C~land_use, data=CO2_depth10)
#0.014



#40-50 cm
means<-with (CO2_depth50, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(CO2_depth50, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(CO2_depth50, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -55.0, soy: -54.3
ses
#forest: 12.9, soy: 14.1

#test for normality 
test.lm33<-lm(X.14C~land_use, data=CO2_depth50)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.2079

#ANOVA
C.aov<-aov(X.14C~land_use, data=CO2_depth50)
anova(C.aov)
#p=0.975



#90-100 cm
means<-with (CO2_depth100, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds<-with(CO2_depth100, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns<-with(CO2_depth100, tapply(X.14C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -16.4, soy: -37.1
ses
#forest: 8.7, 16.3

#test for normality 
test.lm33<-lm(X.14C~land_use, data=CO2_depth100)
par(mfrow=c(2,2))
plot(test.lm33)

shapiro.test(residuals(test.lm33))
#p=0.635

#ANOVA
C.aov<-aov(X.14C~land_use, data=CO2_depth100)
anova(C.aov)
#p=0.3772



###################################
#End of Table 3



##############################
#CO2 and 14C, all depths by land use

means2<-with (main2013CO2, tapply(X.14C, land_use, mean, na.rm=TRUE))
sds2<-with(main2013CO2, tapply(X.14C, land_use, sd, na.rm=TRUE))
ns2<-with(main2013CO2, tapply(X.14C, land_use, length))
ses2<-sds2/sqrt(ns2)

round(means2, digits=1)
#forest: 4.3, soy: -39.5

#test normality
test.lm33<-lm(X.14C~land_use, data=main2013CO2)
shapiro.test(residuals(test.lm33))
#p=0.5156

#ANOVA
C.aov<-aov(X.14C~land_use, data=main2013CO2)
anova(C.aov)
#p=0.1796





################################
#13C vs. 14C of CO2, all depths
subby<-subset(main2013CO2,select=c("Other.ID","d13C","X.14C","year_conv"))

summaryBy(d13C~year_conv, data=subby,FUN=c(mean),na.rm=TRUE)

subbzz<-subby[order(subby[,4]),]
subbzz
#2003: 2 sites, 4 obs
#2007: 1 site, 3 obs
#2008: 2 sites, 4 obs
#2020: 4 sites, 10 obs


#test for normality of residuals
lm2<-lm(main2013CO2$d13C~main2013CO2$X.14C)
shapiro.test(residuals(lm2))
#p=0.51

summary(lm2)
#multiple r2=0.57, r2=0.53, p=0.00457
#so 13C is sig. related to 14C of CO2



################################
#CO2 and 13C, all depths by year converted to soybeans
means2<-with (main2013CO2, tapply(d13C, year_conv, mean, na.rm=TRUE))
sds2<-with(main2013CO2, tapply(d13C, year_conv, sd, na.rm=TRUE))
ns2<-with(main2013CO2, tapply(d13C, year_conv, length))
ses2<-sds2/sqrt(ns2)

round(means2, digits=1)
#2003: -18.5, 2007: -20.9, 2008: -17.9, forest:-23.9
round(ses2, digits=1)
#2003: 0.3, 2007: NA, 2008: 1.0, forest:1.3


################################
#CO2 and 13C, all depths by land use
means2<-with (main2013CO2, tapply(d13C, land_use, mean, na.rm=TRUE))
sds2<-with(main2013CO2, tapply(d13C, land_use, sd, na.rm=TRUE))
ns2<-with(main2013CO2, tapply(d13C, land_use, length))
ses2<-sds2/sqrt(ns2)

round(means2, digits=1)
#forest: -23.9, soy: -18.8



################################
#CO2 and 13C by land use by depth

#0-10 cm
means<-with (CO2_depth10, tapply(d13C, land_use, mean, na.rm=TRUE))
sds<-with(CO2_depth10, tapply(d13C, land_use, sd, na.rm=TRUE))
ns<-with(CO2_depth10, tapply(d13C, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
#forest: -27.3, soy: -18.8
ses
#forest: 0.2136, soy: 0.7264

#test for normality 
lm555<-lm(d13C~land_use,data=CO2_depth10)
shapiro.test(residuals(lm555))
#p=0.64

C.aov<-aov(d13C~land_use, data=CO2_depth10)
anova(C.aov)
#p=2.029e-05



#10-20 cm
means<-with (CO2_depth20, tapply(d13C, land_use, mean, na.rm=TRUE))
round(means, digits=1)
#forest: NA soy: NA

#40-50 cm
means34<-with (CO2_depth50, tapply(d13C, land_use, mean, na.rm=TRUE))
round(means34, digits=1)
#forest: -19.4, soy: NaN

#90-100 cm
means35<-with (CO2_depth100, tapply(d13C, land_use, mean, na.rm=TRUE))
round(means35, digits=1)
#forest: NaN soy: NaN

#190-200 cm
means<-with (CO2_depth200, tapply(d13C, land_use, mean, na.rm=TRUE))
round(means, digits=1)
#forest: NA soy: NA
#########################################


################################
#14C of CO2 vs. years in soybean cultivation, all depths

#test normality of residuals
C14.lm7<-lm(main2013CO2$X.14C~yrssoyCO2nn)
shapiro.test(residuals(C14.lm7))
#p=0.42

summary(C14.lm7)
#intercept:p=0.9960, yrssoyCO2nn:p=0.0965; cannot reject the null that the slope equals zero



################################
#13C of CO2 vs. years in soybean cultivation, all depths
C14.lm8<-lm(main2013CO2$d13C~yrssoyCO2nn)
shapiro.test(residuals(C14.lm8))
#p=0.0515


#quantile regression
qr8<-rq(main2013CO2$d13C~yrssoyCO2nn, tau=0.5)
summ8<-summary(qr8,se="ker")
summ8
#intercept p=0.0004, yrssoyCO2nn p=0.48





