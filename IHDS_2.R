library(haven)      #loading stata dataset
library(dplyr)      #package for filtering out Madhya Pradesh and other criterias
library(fastDummies)#for creating dummies
d <- read_dta("C:\\Users\\Dilip\\Desktop\\IHDS_data.dta")
df <- d %>% filter(STATEID != 23)
df$exper <- df$age - df$eduyrs - 5
df <- df %>% filter(age >= 15 & age <= 65 & exper > 0 & !(indus_grp == 9|indus_grp == 999) & dwg == 1)
sum(df$wrkhr == 0)               #there are 31 obs with value zero
df <- df[df$wrkhr != 0, ]        #removing zero values
#cleaned dataset returning df with 47574 obs. of 62 variables
df$hrlywage <- df$wrkwg/df$wrkhr
df$lnhrwage <- log(df$hrlywage)
df$sqage <- df$age^2
df$sqeduyrs <- df$eduyrs^2

#converting to factor variables & creating dummies
#ED3
df$ED3 <- factor(df$ED3, labels = c("none", "Little", "Fluent"))
df <- dummy_cols(df, select_columns = "ED3")

#educd
df$educd <- factor(df$educd, labels = c("Not Literate", "Primary", "Middle", "Secondary", 
                                        "HiSecondary", "PostHiSecndry"))
df <- dummy_cols(df, select_columns = "educd")

#gender
df$gender <- factor(df$gender, levels = c(1, 2), labels = c("males", "females"))

#1
model1_1 <- lm(log(df$hrlywage) ~ df$`educd_Not Literate` + df$educd_Primary + df$educd_Middle + 
                 df$educd_Secondary + df$educd_HiSecondary + df$educd_PostHiSecndry, data = df)
model1_1 <- lm(log(df$hrlywage) ~ df$educd, data = df)
model1_2 <- lm(log(df$hrlywage) ~ df$eduyrs + df$sqeduyrs)
summary(model1_1)
summary(model1_2)

#2
model2_1 <- lm(log(df$hrlywage) ~ df$ED3_Little + df$ED3_Fluent, data = df)
model2_2 <- lm(log(df$hrlywage) ~ df$ED3_none + df$ED3_Little + df$ED3_Fluent, data = df)
model2_3 <- lm(log(df$hrlywage) ~ -1 + df$ED3_none + df$ED3_Little + df$ED3_Fluent, data = df)
summary(model2_1)
summary(model2_2)
summary(model2_3)

#3
model3_1 <- lm(log(df$hrlywage) ~ df$ED3)
model3_2 <- lm(log(df$hrlywage) ~ df$ED3 + df$eduyrs + df$sqeduyrs)
model3_3 <- lm(log(df$hrlywage) ~ df$ED3 + df$eduyrs + df$sqeduyrs + df$age +df$sqage)
model3_4 <- lm(log(df$hrlywage) ~ df$ED3 + df$eduyrs + df$sqeduyrs + df$age +df$sqage + df$gender)
model3_5 <- lm(log(df$hrlywage) ~ df$ED3 + df$eduyrs + df$sqeduyrs + df$age +df$sqage + df$gender +
               df$ED3:df$gender  + df$eduyrs:df$gender + df$sqeduyrs:df$gender + df$age:df$gender + df$sqage:df$gender)
summary(model3_1)
summary(model3_2)
summary(model3_3)
summary(model3_4)
summary(model3_5)
#------------------------------------------------------------------------------------------------------------------------