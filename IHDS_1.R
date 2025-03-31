library(haven) #loading stata dataset
library(dplyr) #loading dplyr for filtering out Madhya Pradesh and other criterias
d <- read_dta("C:\\Users\\Dilip\\Desktop\\IHDS_data.dta")
df <- d %>% filter(STATEID != 23)
df$exper <- df$age - df$eduyrs - 5
df <- df %>% filter(age >= 15 & age <= 65 & exper > 0 & !(indus_grp == 9 | indus_grp == 999) & dwg == 1)
df$sqeduyrs <- df$eduyrs^2           #squaring eduyrs
df$sqexper <- df$exper^2             #squaring exper

#2 requirements
df$mwrkwg <- df$wrkwg/12             #converting annual earnings to monthly earnings and taking log
df$sqexper100 <- df$sqexper/100      #creating transformation by dividing sq. of exper by 100

#3 requirements
df$sqage100 <- (df$age^2)/100

#4 data cleaning
df$logwrkhr <- log(df$wrkhr)
any(is.infinite(df$logwrkhr))        #infinite values are present in logwrkhr
df <- df[is.finite(df$logwrkhr), ]   #removing infinite values

#dataset cleaned as per requirements giving final dataset of 47574 obs. of 68 variables

#1
model1 <- lm(log(df$wrkwg) ~ df$exper + df$sqexper + df$eduyrs + df$sqeduyrs, data = df)
summary(model1)
anova(model1)

#2
model2 <- lm(log(df$mwrkwg) ~ df$exper + df$sqexper100 + df$eduyrs + df$sqeduyrs, data = df)
summary(model2)
anova(model2)

#3
model3 <- lm(log(df$mwrkwg) ~ df$age + df$sqage100 + df$eduyrs + df$sqeduyrs, data = df)
summary(model3)
anova(model3)

#4
model4 <- lm(log(df$mwrkwg) ~ df$age + df$sqage100 + df$eduyrs + df$sqeduyrs + df$logwrkhr, data = df)
summary(model4)
anova(model4)

df$mwrkwgwrkhr <- df$mwrkwg/df$wrkhr
model4_1 <- lm(log(df$mwrkwgwrkhr) ~ df$age + df$sqage100 + df$eduyrs + df$sqeduyrs, data = df)
summary(model4_1)

#calculating Fcal for restricted model[4:(ii)]:
fcalRSS1 <- (60650 - 33794)/(1)/(33794/47568)
fcalRsq1 <- (0.5689 - 0.2264)/(1)/((1-0.5689)/(47568))

#calculating Fcal for for restricted model[4:(iii)]
fcalRSS2 <- (34299 - 33794)/(1)/(33794/47568)

#5
df$sqage <- (df$age^2)
model5 <- lm(log(df$wrkwg) ~ df$eduyrs + df$age + df$sqage, data = df)
summary(model5)

#6
model6 <- lm(log(df$wrkwg) ~ df$eduyrs + df$exper + df$eduyrs*df$exper, data = df)
summary(model6)

anova(model3 , model4)
anova(model4_1,model4)

qf(0.99, 1, 47568) #ftab
qt(0.95, 47568)    #ttab
#------------------------------------------------------------------------------------------------------