#this code was written by Dr. Chelsea Nagy (University of Colorado- Boulder) for the publication "Soil carbon dynamics in soybean cropland and forests in Mato Grosso, Brazil".

library(ggplot2)
library(doBy)
library(sm)
library(quantreg)
library(car)

C14<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/Tanguro_14C_data_for_analysis_BS.csv")
str(C14)
head(C14)

#remove some unnecessary variables (columns)
C14slim<-C14[ , -c(2,3,4,5,6,7,8,9,12,17,20,23,25,26,27,28)]
head(C14slim)

#subset samples that were not acidified
main<-C14slim[which(C14$acidified=="no"),]

#subset bulk soil
mainBS<-main[which(main$samle=="BS"),]

#subset samples collected in 2013
mainBS2013<-mainBS[which(mainBS$year_samp=="2013"),]

#subset samples collected from 0-10 cm
depth10_13<-mainBS2013[which(mainBS2013$depth=="10"),]

#calculate mean bulk density by land use at 10 cm
means10<-with (depth10_13, tapply(BD, land_use, mean, na.rm=TRUE))
round(means10, digits=2)
#forest=1.18, soy=1.49

#calculate ratio of forestBD/soyBD
rat10<-means10[1]/means10[2]
rat10
#0.7971

#correction factor
corr10<-0.7971

#create adjusted C content based on correction factor
depth10_13$C_cont_mass_adj <- ifelse(depth10_13$land_use=="forest", depth10_13$C_cont_in_interval_g_m2, depth10_13$C_cont_in_interval_g_m2*corr10)

#calculate mean adjusted C content at 0-10 cm
means<-with (depth10_13, tapply(C_cont_mass_adj, land_use, mean, na.rm=TRUE))
sds<-with(depth10_13, tapply(C_cont_mass_adj, land_use, sd, na.rm=TRUE))
ns<-with(depth10_13, tapply(C_cont_mass_adj, land_use, length))
ses<-sds/sqrt(ns)
round(means, digits=2)
# forest: 2826.26, soy: 1386.52
round(ses, digits=2)
# forest: 170.88, soy: 152.03

#test for normality
test.lm33<-lm(C_cont_mass_adj~land_use, data=depth10_13)
par(mfrow=c(2,2))
plot(test.lm33)
#looks pretty good

shapiro.test(residuals(test.lm33))
#p=0.4107

#ANOVA
C10.aov<-aov(C_cont_mass_adj~land_use, data=depth10_13)
anova(C10.aov)
#p=7.401e-05




#include update to 50 cm based on equivalent mass (adjusts for soil compaction) in 0-10 cm
#bring in data from profile sites 
C14w<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/weighted.csv")

#subset by depth
depth10<-C14w[which(C14w$depth=="10"),]
depth20<-C14w[which(C14w$depth=="20"),]
depth50<-C14w[which(C14w$depth=="50"),]

#calculate means at each depth
means10<-with (depth10, tapply(BD, land_use, mean, na.rm=TRUE))
round(means10, digits=2)
#forest=1.20, soy=1.46

#ratio of forestBD/soyBD
rat10<-means10[1]/means10[2]
rat10
#0.81949

#correction factor
corr10<-0.81949

head(C14w)
#calculate new adjusted values for soy at 0-10 cm
C14w$C10new<- ifelse(C14w$depth==10&C14w$land_use=="soy", C14w$cumC*corr10,0)
#calculate the difference between new adjusted values for soy and old values for soy at 0-10 cm
C14w$C10diff<- ifelse(C14w$depth==10&C14w$land_use=="soy", C14w$cumC-C14w$C10new,0)
head(C14w)
tail(C14w)

sub50<-C14w[which(C14w$depth==50),]
head(sub50)

sub10<-C14w[which(C14w$depth=="10"),]
head(sub10)

#sub in new values with the difference between new and old values for soy C content
sub50$C10diff<-sub10$C10diff[match(sub50$point, sub10$point)]
head(sub50)

#new adjusted C content to 50 cm
sub50$cumC3<-sub50$cumC2-sub50$C10diff

#calculate mean C content to 50 cm by land use
means<-with (sub50, tapply(cumC3, land_use, mean, na.rm=TRUE))
sds<-with(sub50, tapply(cumC3, land_use, sd, na.rm=TRUE))
ns<-with(sub50, tapply(cumC3, land_use, length))
ses<-sds/sqrt(ns)
round(means, digits=2)
# forest: 7900.09, soy: 6570.33
round(ses, digits=2)
# forest: 839.14, soy: 1136.80

#test for normality
test.lm33<-lm(cumC3~land_use, data=sub50)
par(mfrow=c(2,2))
plot(test.lm33)
#slightly non-normal

shapiro.test(residuals(test.lm33))
#p=0.1039
#ok

#ANOVA
C99.aov<-aov(cumC3~land_use, data=sub50)
anova(C99.aov)
#p=0.383