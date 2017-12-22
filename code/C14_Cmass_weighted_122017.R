#this code was written by Dr. Chelsea Nagy (University of Colorado- Boulder) for the publication "Soil carbon dynamics in soybean cropland and forests in Mato Grosso, Brazil".

#weighted data to simulate mixing due to plowing

library(ggplot2)
library(doBy)
library(sm)
library(quantreg)

C14w<- read.csv("C:/Users/rnagy/Dropbox/Tanguro_soil_C_chapter/final_code_and_data/weighted.csv")
str(C14w)
head(C14w)

#subset by depth
depth10<-C14w[which(C14w$depth=="10"),]
depth20<-C14w[which(C14w$depth=="20"),]
depth50<-C14w[which(C14w$depth=="50"),]



#calculate means for 0-10 cm
means<-with (depth10, tapply(X14Cnew, land_use, mean, na.rm=TRUE))
sds<-with(depth10, tapply(X14Cnew, land_use, sd, na.rm=TRUE))
ns<-with(depth10, tapply(X14Cnew, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 107.8, soy:77.4
round (ses, digits=2)
# forest: 4.75, soy: 10.80

#test for normality
test.lm33 = lm(X14Cnew~land_use, data=depth10)
shapiro.test(residuals(test.lm33))
#p=0.73

#ANOVA
C10.aov<-aov(X14Cnew~land_use, data=depth10)
anova(C10.aov)
#p=0.04219






#calculate means for 0-20 cumulative

#14C for 0-20 cm cumulative
means<-with (depth20, tapply(X14Cnew, land_use, mean, na.rm=TRUE))
sds<-with(depth20, tapply(X14Cnew, land_use, sd, na.rm=TRUE))
ns<-with(depth20, tapply(X14Cnew, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 78.9, soy:45.4
round (ses, digits=2)
# forest: 9.68, soy: 21.22

#test for normality
test.lm33 = lm(X14Cnew~land_use, data=depth20)
shapiro.test(residuals(test.lm33))
#p=0.96

#ANOVA
C10.aov<-aov(X14Cnew~land_use, data=depth20)
anova(C10.aov)
#p=0.2011



#13C for 0-20 cm cumulative
means<-with (depth20, tapply(X13Cnew, land_use, mean, na.rm=TRUE))
sds<-with(depth20, tapply(X13Cnew, land_use, sd, na.rm=TRUE))
ns<-with(depth20, tapply(X13Cnew, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: -27.8, soy: -23.6
round (ses, digits=2)
# forest: 0.38, soy: 0.54

#test for normality
test.lm33 = lm(X13Cnew~land_use, data=depth20)
shapiro.test(residuals(test.lm33))
#p=0.1372

#ANOVA
C10.aov<-aov(X13Cnew~land_use, data=depth20)
anova(C10.aov)
#p=0.00077







#0-50 cm cumulative

#14C for 0-50 cm cumulative
means<-with (depth50, tapply(X14Cnew, land_use, mean, na.rm=TRUE))
sds<-with(depth50, tapply(X14Cnew, land_use, sd, na.rm=TRUE))
ns<-with(depth50, tapply(X14Cnew, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 37.5, soy:3.7
round (ses, digits=2)
# forest: 13.36, soy: 23.58

#test for normality
test.lm33 = lm(X14Cnew~land_use, data=depth50)
shapiro.test(residuals(test.lm33))
#p=0.675

#ANOVA
C10.aov<-aov(X14Cnew~land_use, data=depth50)
anova(C10.aov)
#p=0.2594



#13C for 0-50 cm cumulative
means<-with (depth50, tapply(X13Cnew, land_use, mean, na.rm=TRUE))
sds<-with(depth50, tapply(X13Cnew, land_use, sd, na.rm=TRUE))
ns<-with(depth50, tapply(X13Cnew, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: -27.1, soy: -23.5
round (ses, digits=2)
# forest: 0.40, soy: 0.47

#test for normality
test.lm33 = lm(X13Cnew~land_use, data=depth50)
shapiro.test(residuals(test.lm33))
#p=0.01537

#try non-parametric anova here
kruskal.test(X13Cnew ~ land_use, data = depth50)
#p=0.02




#C content (cumC2)
#cumC2 includes C in intervals not sampled (e.g., 20-40 cm)
means<-with (depth50, tapply(cumC2, land_use, mean, na.rm=TRUE))
sds<-with(depth50, tapply(cumC2, land_use, sd, na.rm=TRUE))
ns<-with(depth50, tapply(cumC2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 7900.1, soy: 6849.8
round (ses, digits=1)
# forest: 839.1, soy: 1197.4

#test for normality
test.lm33 = lm(cumC2~land_use, data=depth50)
shapiro.test(residuals(test.lm33))
#p=0.2267

#ANOVA
C10.aov<-aov(cumC2~land_use, data=depth50)
anova(C10.aov)
#p=0.4995





##########################################################
#14CO2 rather than bulk soil from 0-50 cm
means<-with (depth50, tapply(X14Cnew_CO2, land_use, mean, na.rm=TRUE))
sds<-with(depth50, tapply(X14Cnew_CO2, land_use, sd, na.rm=TRUE))
ns<-with(depth50, tapply(X14Cnew_CO2, land_use, length))
ses<-sds/sqrt(ns)

round(means, digits=1)
# forest: 25.5, soy: -23.8
round (ses, digits=2)
# forest: 7.64, soy: 10.92

#test for normality
test.lm33 = lm(X14Cnew_CO2~land_use, data=depth50)
shapiro.test(residuals(test.lm33))
#p=0.6518

#ANOVA
C10.aov<-aov(X14Cnew_CO2~land_use, data=depth50)
anova(C10.aov)
#p=0.01006


