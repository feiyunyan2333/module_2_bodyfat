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

center1 <- round((4.57/df$DENSITY)-4.142,3)*100
sum((df$BODYFAT<center1+1) & (df$BODYFAT>center1-1))
density_out_ind <- which(((df$BODYFAT<center1+1) & (df$BODYFAT>center1-1))==F)
density_out_ind


center2 <- round((df$WEIGHT/df$HEIGHT^2)*703,3)
sum((df$ADIPOSITY<center2+1) & (df$ADIPOSITY>center2-1))
bmi_out_ind <- which(((df$ADIPOSITY<center2+1) & (df$ADIPOSITY>center2-1))==F)
bmi_out_ind



corrplot::corrplot(cor(df))
# redundant dataset density and bodyfat, adiposity and weigh&height

df_ <- data.frame(df$BODYFAT,df$AGE,df$ADIPOSITY,df$NECK,df$THIGH,df$KNEE,df$ANKLE,
                  df$BICEPS,df$FOREARM,df$WRIST,df$HIP)
colnames(df_) <- c("bodyfat","age","adiposity","neck","thigh","knee","ankle",
                   "forearm","biceps","wrist","hip") 


df_$waist_hip_ratio <- df$ABDOMEN/df$HIP

set.seed(26261)
train_ind <- sample(1:nrow(df_),size = round(0.7*nrow(df_)))
df_train <- df_[train_ind,]
df_test <- df_[-train_ind,]
# df[which.min(df$BODYFAT),]

mod1 <- lm(bodyfat~.,df_train)

summary(mod1)
mod1_fit <- predict(mod1,newdata = df_train )
sum((mod1_fit- df_train$bodyfat)^2)/nrow(df_train)
cor(mod1_fit,df_train$bodyfat)^2


mod1_pred <- predict(mod1,newdata = df_test )
sum((mod1_pred- df_test$bodyfat)^2)/nrow(df_test)
cor(mod1_pred,df_test$bodyfat)^2



library(randomForest)
mod2 <- randomForest(bodyfat~.,data=df_train)
#barplot(x=1:9,mod2$importance)

library(rpart)
library(rpart.plot)
mod3 <- rpart(bodyfat~.,df_train,method = "anova")
summary(mod3)
prp(mod3)

mod3_fit <- predict(mod3,newdata = df_train )
sum((mod3_fit- df_train$bodyfat)^2)/nrow(df_train)
cor(mod3_fit,df_train$bodyfat)^2


mod3_pred <- predict(mod3,newdata = df_test )
sum((mod3_pred- df_test$bodyfat)^2)/nrow(df_test)
cor(mod3_pred,df_test$bodyfat)^2

best <- mod3$cptable[which.min(mod3$cptable[,"xerror"]),"CP"]

#produce a pruned tree based on the best cp value
pruned_tree <- prune(mod3, cp=best)

#plot the pruned tree


prp(pruned_tree,
    faclen=0, #use full names for factor labels
    extra=1, #display number of obs. for each terminal node
    roundint=F, #don't round to integers in output
    digits=5) #display 5 decimal places in output
#ggplot2::ggplot()+



# Set the number of bootstrap iterations
num_iterations <- 100

# Initialize a vector to store R-squared values
rsquared_values <- numeric(num_iterations)

# Loop through each iteration
for (i in 1:num_iterations) {
  # Create a bootstrap sample by sampling with replacement from the training data
  
  train_ind <- sample(1:nrow(df_),size = round(0.7*nrow(df_)))
  df_train <- df_[train_ind,]
  df_test <- df_[-train_ind,]
  
  bootstrap_indices <- sample(nrow(df_train), replace = TRUE)
  bootstrap_sample <- df_train[bootstrap_indices, ]
  
  # Fit a linear regression model to the bootstrap sample
  lm_model_bootstrap <- lm(bodyfat ~ adiposity + waist_hip_ratio, data = bootstrap_sample)
  
  # Predict on the testing data
  predictions_bootstrap <- predict(lm_model_bootstrap, newdata = df_test)
  
  # Calculate R-squared for the current bootstrap iteration
  rsquared_values[i] <- 1 - sum((df_test$bodyfat - predictions_bootstrap)^2) / sum((df_test$bodyfat - mean(df_test$bodyfat))^2)
}

# Calculate the mean R-squared over all iterations
mean_rsquared <- mean(rsquared_values)

# Print the mean R-squared
print(paste("Mean R-squared:", mean_rsquared))












