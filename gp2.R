rm(list=ls())

library("corrgram")



setwd("F:\\office\\2023 fall\\stat 628\\module 2")
df <- read.csv("BodyFat.csv")
head(df)
summary(df)

#Brozek formula: BF=(4.57/ρ − 4.142) × 100[14]

# df[182,]$BODYFAT = -3.611687

#Siri formula is: BF=(4.95/ρ − 4.50) × 100[15]

# df[182,]$BODYFAT = -2.079881

# negative bodyfat so simply delete / or use mice ? as a NA

hist(df$WEIGHT)

center <- round((4.57/df$DENSITY)-4.142,3)*100
sum((df$BODYFAT<center+1) & (df$BODYFAT>center-1))





corrplot::corrplot(cor(df))
# redundant dataset density and bodyfat, adiposity and weigh&height

df_ <- data.frame(df$BODYFAT,df$AGE,df$ADIPOSITY,df$NECK,df$THIGH,df$KNEE,df$ANKLE,
                  df$BICEPS,df$FOREARM,df$WRIST)
colnames(df_) <- c("bodyfat","age","adiposity","neck","thigh","knee","ankle",
                   "forearm","biceps","wrist") 
mod1 <- lm(bodyfat~.,df_)

summary(mod1)

df[which.min(df$BODYFAT),]

library(randomForest)
mod2 <- randomForest(bodyfat~.,data=df_)
mod2
