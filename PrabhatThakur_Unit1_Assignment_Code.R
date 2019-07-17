# Unit 1 Assignment > MONEYBALL OLS REGRESSION PROJECT
# MSDS 411 Section 56
# Prabhat Thakur 04/20/2019

######## Download appropriate packages and install######
library(reshape2)
library(psych)
library(ggplot2)
library(RColorBrewer)
library(mice)
library(car)
library(MASS)
library(dplyr)
library(caret)
library(party)
library(moments)
require(gridExtra)

#Designated proper working environment#
setwd("C:/Users/THAKPR8/Documents/MSDS/411_Sec56/Unit1/HW1/")

# Load training and testing data
train <- read.csv("moneyball.csv",header=T)
test <- read.csv("moneyball_test.csv")


############## Part 1: Data Exploration ##########################################################################
str(train)
summary(train)
describe(train %>%  select(-INDEX))
str(test)
summary(test)

# High level summary of the target
summary(train$TARGET_WINS)

par(mfrow=c(1,2))
hist(train$TARGET_WINS, col = "#A71930", xlab = "TARGET_WINS", main = "Histogram of Wins")
boxplot(train$TARGET_WINS, col = "#A71930", main = "Boxplot of Wins")
par(mfrow = c(1,1))

################# Batting ####################
# Hits and Doubles
par(mfrow=c(2,2))
hist(train$TEAM_BATTING_H, col = "#A71930", xlab = "Team_Batting_H", main = "Histogram of Hits")
hist(train$TEAM_BATTING_2B, col = "#09ADAD", xlab = "Doubles", main = "Histogram of Doubles")
boxplot(train$TEAM_BATTING_H, col = "#A71930", main = "Boxplot of Hits")
boxplot(train$TEAM_BATTING_2B, col = "#09ADAD", main = "Boxplot of Doubles")
par(mfrow=c(1,1))

# Triples and Home Runs
par(mfrow=c(2,2))
hist(train$TEAM_BATTING_3B, col = "#A71930", xlab = "Triples", main = "Histogram of Triples")
hist(train$TEAM_BATTING_HR, col = "#DBCEAC", xlab = "Home Runs", main = "Histogram of Home Runs")
boxplot(train$TEAM_BATTING_3B, col = "#A71930", main = "Boxplot of Triples")
boxplot(train$TEAM_BATTING_HR, col = "#DBCEAC", main = "Boxplot of Home Runs")
par(mfrow=c(1,1))

# Walks, Strikeouts, HBP
par(mfrow=c(2,3))
hist(train$TEAM_BATTING_BB, col = "#A71930", xlab = "Walks", main = "Histogram of Walks")
hist(train$TEAM_BATTING_SO, col = "#09ADAD", xlab = "Strikeouts", main = "Histogram of Strikeouts")
hist(train$TEAM_BATTING_HBP, col = "#DBCEAC", xlab = "Hit By Pitches", main = "Histogram of HBP")
boxplot(train$TEAM_BATTING_BB, col = "#A71930", main = "Boxplot of Walks")
boxplot(train$TEAM_BATTING_SO, col = "#09ADAD", main = "Boxplot of Strikeouts")
boxplot(train$TEAM_BATTING_HBP, col = "#DBCEAC", main = "Boxplot of HBP")
par(mfrow=c(1,1))

# Stolen Bases and Caught Stealing
par(mfrow=c(2,2))
hist(train$TEAM_BASERUN_SB, col = "#A71930", xlab = "Stolen Bases", main = "Histogram of Steals")
hist(train$TEAM_BASERUN_CS, col = "#DBCEAC", xlab = "Caught Stealing", main = "Histogram of CS")
boxplot(train$TEAM_BASERUN_SB, col = "#A71930", main = "Boxplot of Steals")
boxplot(train$TEAM_BASERUN_CS, col = "#DBCEAC", main = "Boxplot of CS")
par(mfrow=c(1,1))

################ Pitching ############
# Hits and Home Runs
par(mfrow=c(2,2))
hist(train$TEAM_PITCHING_H, col = "#A71930", xlab = "Hits Against", main = "Histogram of Hits Against")
hist(train$TEAM_PITCHING_HR, col = "#09ADAD", xlab = "Home Runs Against", main = "Histograms of HR Against")
boxplot(train$TEAM_PITCHING_H, col = "#A71930", main = "Boxplot of Hits Against")
boxplot(train$TEAM_PITCHING_HR, col = "#09ADAD", main = "Boxplot of HR Against")
par(mfrow=c(1,1))

# Walks and Strikeouts
par(mfrow=c(2,2))
hist(train$TEAM_PITCHING_BB, col = "#A71930", xlab = "Walks Allowed", main = "Histogram of Walks Allowed")
hist(train$TEAM_PITCHING_SO, col = "#DBCEAC", xlab = "Strikeouts", main = "Histograms of Strikeouts")
boxplot(train$TEAM_PITCHING_BB, col = "#A71930", main = "Boxplot of Walks Allowed")
boxplot(train$TEAM_PITCHING_SO, col = "#DBCEAC", main = "Boxplot of Strikeouts")
par(mfrow=c(1,1))

############## Fielding ###########
# Double Plays and Errors 
par(mfrow=c(2,2))
hist(train$TEAM_FIELDING_DP, col = "#A71930", xlab = "Double Plays", main = "Histogram of Double Plays")
hist(train$TEAM_FIELDING_E, col = "#09ADAD", xlab = "Errors Committed", main = "Histogram of Errors Committed")
boxplot(train$TEAM_FIELDING_DP, col = "#A71930", main = "Boxplot of Double Plays")
boxplot(train$TEAM_FIELDING_E, col = "#09ADAD", main = "Boxplot of Errors Committed")
par(mfrow=c(1,1))

# Setting colors for plots
pal <- brewer.pal(4, "Set1")

# Merge training and testing for analyzing predictors
df1 <- train %>%   select(-INDEX, -TARGET_WINS) %>%  mutate(Data = "Training")
df2 <- test %>%   select(-INDEX) %>%  mutate(Data = "Testing")
df <- rbind(df1, df2) %>% melt(id.vars = "Data")

# High level summary of all predictors
summary(rbind(df1, df2))

# Box plots
ggplot(df, aes(x = variable, y = value, color = Data)) +
  xlab("Variable") +
  ylab("Count") +
  ggtitle("Box Plots for Predictor Variables") +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_blank())

# Histograms
ggplot(df, aes(value, fill = Data)) +
  xlab("Variable Value") +
  ylab("Count") +
  ggtitle("Histograms for Predictor Variables") +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Training predictor correlation 
corr <- cor(train[complete.cases(train[, 2:ncol(train)]), 2:ncol(train)])
corr_melt <- melt(round(corr, 2))

ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.75) +
  scale_fill_gradient2(low = pal[2], high = pal[1], mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(.75, "line")) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation - Training Data")

# Visualize univariate relationships with the target
train_melt <- train[, 2:ncol(train)] %>%   melt(., id.vars = "TARGET_WINS")

ggplot(train_melt, aes(x = value, y = TARGET_WINS)) +
  geom_point(fill = pal[4], color = "black", shape = 21, size = 1) +
  geom_smooth(method = lm) +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Univariate Relationship with Target") +
  xlab("Predictor Variable Value") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Calculate skewness for each variable
train[, 2:ncol(train)] %>%
  summarise_all(skewness, na.rm = TRUE)

# Visualize missing values in the data
MissingCount <- train %>%  select(-INDEX) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>% filter(value > 0) %>% rename("Missing_Value" = value)

MissingFraction <- train %>% select(-INDEX) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>%  mutate(value = value / nrow(train)) %>% filter(value > 0) %>% rename("Missing_Fraction" = value)

p1 <- ggplot(MissingCount, aes(x = variable, weight = Missing_Value)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values") +
  ggtitle("Missing Values - Training") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

p2 <- ggplot(MissingFraction, aes(x = variable, weight = Missing_Fraction)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values Fraction") +
  ggtitle("Missing Values Fraction - Training") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

MissingCountT <- test %>%  select(-INDEX) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>% filter(value > 0) %>% rename("Missing_Value" = value)

MissingFractionT <- test %>% select(-INDEX) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>%  mutate(value = value / nrow(test)) %>% filter(value > 0) %>% rename("Missing_Fraction" = value)

p3 <- ggplot(MissingCountT, aes(x = variable, weight = Missing_Value)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values") +
  ggtitle("Missing Values - Test DataSet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

p4 <- ggplot(MissingFractionT, aes(x = variable, weight = Missing_Fraction)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values Fraction") +
  ggtitle("Missing Values Fraction - Test DataSet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))

grid.arrange(p1, p2,p3,p4, nrow= 2,ncol=2)

# Check for normality in target
ggplot(train, aes(sample = TARGET_WINS)) +
  stat_qq(color = pal[2]) + stat_qq_line() +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Target Normal QQ-Plot") +
  theme(legend.position = "none")

# Part 2: Data Preparation

# Drop pitching columns that appear incorrect
# Cap TARGET_WINS to 116 and 20. 
# Create identifier columns for missing data
# Create variable for singles to reduce correlation

train_prep <- train %>%
  select(-TEAM_PITCHING_H,
         -TEAM_PITCHING_HR,
         -TEAM_PITCHING_BB,
         -TEAM_PITCHING_SO) %>%
  mutate(TARGET_WINS = ifelse (TARGET_WINS >=116, 116,TARGET_WINS), 
         TARGET_WINS = ifelse (TARGET_WINS <=20, 20,TARGET_WINS), 
         TEAM_BATTING_SO_MISS = ifelse(is.na(TEAM_BATTING_SO), 1, 0),
         TEAM_BASERUN_SB_MISS = ifelse(is.na(TEAM_BASERUN_SB), 1, 0),
         TEAM_BASERUN_CS_MISS = ifelse(is.na(TEAM_BASERUN_CS), 1, 0),
         TEAM_BATTING_HBP_MISS = ifelse(is.na(TEAM_BATTING_HBP), 1, 0),
         TEAM_FIELDING_DP_MISS = ifelse(is.na(TEAM_FIELDING_DP), 1, 0),
         TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)

# Apply same transformations to testing since it is also used in imputation
test_prep <- test %>%
  select(-TEAM_PITCHING_H,
         -TEAM_PITCHING_HR,
         -TEAM_PITCHING_BB,
         -TEAM_PITCHING_SO) %>%
  mutate(TEAM_BATTING_SO_MISS = ifelse(is.na(TEAM_BATTING_SO), 1, 0),
         TEAM_BASERUN_SB_MISS = ifelse(is.na(TEAM_BASERUN_SB), 1, 0),
         TEAM_BASERUN_CS_MISS = ifelse(is.na(TEAM_BASERUN_CS), 1, 0),
         TEAM_BATTING_HBP_MISS = ifelse(is.na(TEAM_BATTING_HBP), 1, 0),
         TEAM_FIELDING_DP_MISS = ifelse(is.na(TEAM_FIELDING_DP), 1, 0),
         TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)

# Impute missing data

# Prepare data
train_impute_input <- train_prep %>%
  select(-TEAM_BATTING_SO_MISS, -TEAM_BASERUN_SB_MISS, -TEAM_BASERUN_CS_MISS,
         -TEAM_BATTING_HBP_MISS,-TEAM_FIELDING_DP_MISS, -TARGET_WINS, -INDEX) %>%
  mutate(Data = "train")

test_impute_input <- test_prep %>%
  select(-TEAM_BATTING_SO_MISS, -TEAM_BASERUN_SB_MISS, -TEAM_BASERUN_CS_MISS,
         -TEAM_BATTING_HBP_MISS,-TEAM_FIELDING_DP_MISS, -INDEX) %>%
  mutate(Data = "test")

# Combine training and testing for identical imputation
# The Mice package lacks a way to build an imputation and then apply it
#  to new data. The work-around here is to redo the imputation with all data.
impute_input <- rbind(train_impute_input, test_impute_input)

# Impute missing using decision trees
mice_df <- impute_input[1:(ncol(impute_input) - 1)]
impute_missing <- mice(mice_df,
                       m = 1, method = "cart", seed = 9798, maxit = 5)

# Save imputed data set
impute_output <- complete(impute_missing, 1)

# Add label of train vs. test
impute_output$Data <- impute_input$Data

# Visualize imputation
densityplot(impute_missing)

describe(impute_output %>%  select(-Data))

# Cap outliers to 1% and 99% values and transform variables
# Add back in the imputed data
# Remove the label for training vs. testing
# Cap and transform variables
# Remove untransformed versions

train_final <- train_prep %>%
  select(INDEX,
         TARGET_WINS,
         TEAM_BATTING_SO_MISS,
         TEAM_BASERUN_SB_MISS,
         TEAM_BASERUN_CS_MISS,
         TEAM_BATTING_HBP_MISS,
         TEAM_FIELDING_DP_MISS) %>%
  cbind(., impute_output[impute_output$Data == "train", ]) %>%
  select(-Data) %>%
  mutate(TEAM_BATTING_1B_CLOG = if(TEAM_BATTING_1B >
                                         quantile(train_prep$TEAM_BATTING_1B,
                                                  probs = c(0.99),
                                                  na.rm = TRUE)){
                                       log(quantile(train_prep$TEAM_BATTING_1B, probs = c(0.99),na.rm = TRUE) + 1)
                                    } else if (TEAM_BATTING_1B < 
                                                                quantile(train_prep$TEAM_BATTING_1B, probs = c(0.01),na.rm = TRUE)){
                                      log(quantile(train_prep$TEAM_BATTING_1B, probs = c(0.01),na.rm = TRUE) + 1)
                                    } else {log(TEAM_BATTING_1B + 1)},
                                   
         
         TEAM_BATTING_2B_C = if(TEAM_BATTING_2B >
                                quantile(train_prep$TEAM_BATTING_2B,
                                         probs = c(0.99),
                                         na.rm = TRUE)){
           quantile(train_prep$TEAM_BATTING_2B, probs = c(0.99),na.rm = TRUE) 
         } else if (TEAM_BATTING_2B < 
                    quantile(train_prep$TEAM_BATTING_2B, probs = c(0.01),na.rm = TRUE)){
           quantile(train_prep$TEAM_BATTING_2B, probs = c(0.01),na.rm = TRUE)
         } else {TEAM_BATTING_2B},
           
         TEAM_BATTING_3B_CLOG = if(TEAM_BATTING_3B >
                                   quantile(train_prep$TEAM_BATTING_3B,
                                            probs = c(0.99),
                                            na.rm = TRUE)){
           log(quantile(train_prep$TEAM_BATTING_3B, probs = c(0.99),na.rm = TRUE) + 1)
         } else if (TEAM_BATTING_3B < 
                    quantile(train_prep$TEAM_BATTING_3B, probs = c(0.05),na.rm = TRUE)){
           log(quantile(train_prep$TEAM_BATTING_3B, probs = c(0.05),na.rm = TRUE) + 1)
         } else {log(TEAM_BATTING_3B + 1)},
           

         TEAM_BASERUN_SB_CLOG = if(TEAM_BASERUN_SB >
                                   quantile(train_prep$TEAM_BASERUN_SB,
                                            probs = c(0.99),
                                            na.rm = TRUE)){
           log(quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.99),na.rm = TRUE) + 1)
         } else if (TEAM_BASERUN_SB < 
                    quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.05),na.rm = TRUE)){
           log(quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.05),na.rm = TRUE) + 1)
         } else {log(TEAM_BASERUN_SB + 1)},
           
           
         #TEAM_BASERUN_SB_S = TEAM_BASERUN_SB /   (TEAM_BASERUN_SB + TEAM_BASERUN_CS),

         TEAM_FIELDING_E_CLOG = if(TEAM_FIELDING_E >
                                   quantile(train_prep$TEAM_FIELDING_E,
                                            probs = c(0.99),
                                            na.rm = TRUE)){
           log(quantile(train_prep$TEAM_FIELDING_E, probs = c(0.99),na.rm = TRUE) + 1)
         } else if (TEAM_FIELDING_E < 
                    quantile(train_prep$TEAM_FIELDING_E, probs = c(0.01),na.rm = TRUE)){
           log(quantile(train_prep$TEAM_FIELDING_E, probs = c(0.01),na.rm = TRUE) + 1)
         } else {log(TEAM_FIELDING_E + 1)},
         

         TEAM_FIELDING_DP_C = if(TEAM_FIELDING_DP >
                                 quantile(train_prep$TEAM_FIELDING_DP,
                                          probs = c(0.99),
                                          na.rm = TRUE)){
           quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.99),na.rm = TRUE) 
         } else if (TEAM_FIELDING_DP < 
                    quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.01),na.rm = TRUE)){
           quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.01),na.rm = TRUE)
         } else {TEAM_FIELDING_DP},
         
         TEAM_BATTING_SO_C = if(TEAM_BATTING_SO >
                                quantile(train_prep$TEAM_BATTING_SO,
                                         probs = c(0.99),
                                         na.rm = TRUE)){
           quantile(train_prep$TEAM_BATTING_SO, probs = c(0.99),na.rm = TRUE) 
         } else if (TEAM_BATTING_SO < 
                    quantile(train_prep$TEAM_BATTING_SO, probs = c(0.01),na.rm = TRUE)){
           quantile(train_prep$TEAM_BATTING_SO, probs = c(0.01),na.rm = TRUE)
         } else {TEAM_BATTING_SO}) %>%
  
   select(-TEAM_BATTING_H, -TEAM_BATTING_1B, -TEAM_BATTING_2B, -TEAM_BATTING_3B,
         -TEAM_BASERUN_SB, -TEAM_FIELDING_E, -TEAM_FIELDING_DP, -TEAM_BATTING_SO )

describe(train_final %>%  select(-INDEX))

train_final <- train_final[-c(1211,1342), ]

# Visualize transformation results
train_final_melt <- train_final[, 2:ncol(train_final)] %>%
  melt(., id.vars = "TARGET_WINS") %>%
  filter(!variable %in% c("TEAM_BATTING_SO_MISS",
                          "TEAM_BASERUN_SB_MISS",
                          "TEAM_BASERUN_CS_MISS",
                          "TEAM_BATTING_HBP_MISS",
                          "TEAM_FIELDING_DP_MISS"))

# Histograms for modified variables 
ggplot(train_final_melt, aes(value)) +
  xlab("Variable Value") +
  ylab("Count") +
  ggtitle("Histograms for Transformed Predictor Variables") +
  geom_histogram(bins = 30, color = "black", fill = pal[2]) +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Training predictor correlation 
corr <- cor(train_final[, 2:ncol(train_final)])
corr_melt <- melt(round(corr, 2))

ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  scale_fill_gradient2(low = pal[2], high = pal[1], mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(.75, "line")) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation - Training Data with Transformations")

# Relationship with target for modified variables
ggplot(train_final_melt, aes(x = value, y = TARGET_WINS)) +
  geom_point(fill = pal[1], color = "black", shape = 21, size = 1) +
  geom_smooth(method = lm) +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Univariate Relationship with Target for Transformed Variables") +
  xlab("Predictor Variable Value") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Part 3: Build Models

# Split training data to observe how models fit on holdout data 
set.seed(6174)
trainIndex <- createDataPartition(train_final$TARGET_WINS, p = 0.7,
                                  list = FALSE,
                                  times = 1)

train_prelim <- train_final[ trainIndex, ]
train_validation <- train_final[-trainIndex, ]

describe(train_final)

# Validation mse function
val_mse <- function(model, log = FALSE) {
  if (log == TRUE) {
    predictions <- exp(predict(model, train_validation))
  } else {
    predictions <- predict(model, train_validation)
  }
  mse <- data.frame(predictions, train_validation$TARGET_WINS)
  names(mse) <- c("predictions", "TARGET_WINS")
  mse <- mse %>%
    mutate(sq_error = (TARGET_WINS - predictions) ^ 2) %>%
    summarise(mse_model1 = mean(sq_error))
  return(mse[1, 1])
}

# Model 1 - Base model using batting variables.
model1 <- lm(TARGET_WINS ~ TEAM_BATTING_1B_CLOG + TEAM_BATTING_2B_C + TEAM_BATTING_3B_CLOG 
             + TEAM_BATTING_HR +  TEAM_BATTING_BB +  TEAM_BATTING_HBP , data = train_prelim)
summary(model1)
anova(model1)

# Model 2 - Complete model
model2 <- lm(TARGET_WINS ~ TEAM_BATTING_HR +
               TEAM_BATTING_BB + TEAM_BASERUN_CS + TEAM_BATTING_HBP + 
               TEAM_BATTING_1B_CLOG + TEAM_BATTING_2B_C +
               TEAM_BATTING_3B_CLOG + TEAM_BASERUN_SB_CLOG +
               TEAM_FIELDING_E_CLOG + TEAM_FIELDING_DP_C + TEAM_BATTING_SO_C +
               TEAM_BATTING_SO_MISS + TEAM_BASERUN_SB_MISS + TEAM_BASERUN_CS_MISS +
               TEAM_BATTING_HBP_MISS + TEAM_FIELDING_DP_MISS, data = train_prelim)

summary(model2)
vif(model2)

# Model 3 - Stepwise Regression
model3 <- stepAIC(model2, direction = "both", trace = 0)

summary(model3)
vif(model3)

# Model comparison
AIC(model1)
AIC(model2)
AIC(model3)
mean(model1$residuals ^ 2)
mean(model2$residuals ^ 2)
mean(model3$residuals ^ 2)
val_mse(model1)
val_mse(model2)
val_mse(model3)

# Bonus Model 4 - Model 3 Using GLM instead
# Results are the same as lm because default is gaussian family with
#  identity link function.
model4_spec <- glm(TARGET_WINS ~ . -INDEX,
                  data = train_prelim)

model4 <- stepAIC(model4_spec, direction = "both")

summary(model4)
Anova(model4)
AIC(model4)
mean(model4$residuals ^ 2)
val_mse(model4)

# Bonus use variables deemed most important by random forest model
set.seed(2141)
train_forest <- cforest(TARGET_WINS ~ . - INDEX,
                        data = train_prelim,
                        control = cforest_unbiased(mtry = 3, ntree = 100))
forest_imp <- varimp(train_forest, conditional = FALSE)
sort(forest_imp, decreasing = TRUE)

# Select the top predictors
vars <- paste("TARGET_WINS ~ ",
              paste(names(head(sort(forest_imp, decreasing = TRUE), 8)),
                    collapse = " + "))

model5 <- lm(vars, data = train_prelim)

summary(model5)
vif(model5)
AIC(model5)
mean(model5$residuals ^ 2)
val_mse(model5)

# Model 6 - Additional interaction terms on top of stepwise
model6 <- lm(TARGET_WINS ~ TEAM_BATTING_HR +
               TEAM_BATTING_BB + TEAM_BASERUN_CS + TEAM_BATTING_HBP + 
               TEAM_BATTING_1B_CLOG + TEAM_BATTING_2B_C +
               TEAM_BATTING_3B_CLOG + TEAM_BASERUN_SB_CLOG +
               TEAM_FIELDING_E_CLOG + TEAM_FIELDING_DP_C + 
               TEAM_BATTING_SO_MISS + TEAM_BASERUN_SB_MISS + TEAM_BASERUN_CS_MISS +
               TEAM_BATTING_HBP_MISS + TEAM_FIELDING_DP_MISS +
               TEAM_BATTING_HR:TEAM_BASERUN_SB_CLOG +
               TEAM_BATTING_SO_C:TEAM_FIELDING_E_CLOG,
             data = train_prelim)

summary(model6)
vif(model6)
AIC(model6)
mean(model6$residuals ^ 2)
val_mse(model6)

# Relationship with target for interactions
train_inter <- train_final %>%
  mutate(HR_SB = TEAM_BATTING_HR * TEAM_BASERUN_SB_CLOG)

ggplot(train_inter, aes(x = HR_SB, y = TARGET_WINS)) +
  geom_point(fill = pal[1], color = "black", shape = 21, size = 1) +
  geom_smooth(method = lm) +
  ggtitle("Univariate Relationship with Target") +
  xlab("TEAM_BATTING_HR * TEAM_BASERUN_SB_CLOG") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))



# Fit final model (model 6) to the entire training data
# Mix of performance and parsimony considerations
model_final <- lm(TARGET_WINS ~ TEAM_BATTING_HR +
                    TEAM_BATTING_BB + TEAM_BASERUN_CS + TEAM_BATTING_HBP + 
                    TEAM_BATTING_1B_CLOG + TEAM_BATTING_2B_C +
                    TEAM_BATTING_3B_CLOG + TEAM_BASERUN_SB_CLOG +
                    TEAM_FIELDING_E_CLOG + TEAM_FIELDING_DP_C + 
                    TEAM_BATTING_SO_MISS + TEAM_BASERUN_SB_MISS + TEAM_BASERUN_CS_MISS +
                    TEAM_BATTING_HBP_MISS + TEAM_FIELDING_DP_MISS +
                    TEAM_BATTING_HR:TEAM_BASERUN_SB_CLOG +
                    TEAM_BATTING_SO_C:TEAM_FIELDING_E_CLOG,
                  data = train_final)

summary(model_final)
AIC(model_final)
mean(model_final$residuals ^ 2)
val_mse(model_final)
# Print coefficients to use in scoring
model_final$coefficients

# Compare coefficients between model6 and model_final as a check
model_coef <- data.frame(model_final$coefficients)
model_coef$model6 <- model6$coefficients

# Diagnoistic Plots
par(mfrow = c(2, 2))
plot(model_final)

# Part 4: Stand Alone Scoring Program

library(mice)
library(dplyr)

# Load training and testing data
train <- read.csv("moneyball.csv")
test <- read.csv("moneyball_test.csv")

# Missing Value Imputation
# The imputation uses both training and testing data so both must be processed
train_prep <- train %>%
  select(-TEAM_PITCHING_H,
         -TEAM_PITCHING_HR,
         -TEAM_PITCHING_BB,
         -TEAM_PITCHING_SO) %>%
  mutate(TARGET_WINS = ifelse (TARGET_WINS >=116, 116,TARGET_WINS), 
         TARGET_WINS = ifelse (TARGET_WINS <=20, 20,TARGET_WINS), 
         TEAM_BATTING_SO_MISS = ifelse(is.na(TEAM_BATTING_SO), 1, 0),
         TEAM_BASERUN_SB_MISS = ifelse(is.na(TEAM_BASERUN_SB), 1, 0),
         TEAM_BASERUN_CS_MISS = ifelse(is.na(TEAM_BASERUN_CS), 1, 0),
         TEAM_BATTING_HBP_MISS = ifelse(is.na(TEAM_BATTING_HBP), 1, 0),
         TEAM_FIELDING_DP_MISS = ifelse(is.na(TEAM_FIELDING_DP), 1, 0),
         TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)

# Apply same transformations to testing since it is also used in imputation
test_prep <- test %>%
  select(-TEAM_PITCHING_H,
         -TEAM_PITCHING_HR,
         -TEAM_PITCHING_BB,
         -TEAM_PITCHING_SO) %>%
  mutate(TEAM_BATTING_SO_MISS = ifelse(is.na(TEAM_BATTING_SO), 1, 0),
         TEAM_BASERUN_SB_MISS = ifelse(is.na(TEAM_BASERUN_SB), 1, 0),
         TEAM_BASERUN_CS_MISS = ifelse(is.na(TEAM_BASERUN_CS), 1, 0),
         TEAM_BATTING_HBP_MISS = ifelse(is.na(TEAM_BATTING_HBP), 1, 0),
         TEAM_FIELDING_DP_MISS = ifelse(is.na(TEAM_FIELDING_DP), 1, 0),
         TEAM_BATTING_1B = TEAM_BATTING_H - TEAM_BATTING_2B - TEAM_BATTING_3B - TEAM_BATTING_HR)

# Impute missing data

# Prepare data
train_impute_input <- train_prep %>%
  select(-TEAM_BATTING_SO_MISS, -TEAM_BASERUN_SB_MISS, -TEAM_BASERUN_CS_MISS,
         -TEAM_BATTING_HBP_MISS,-TEAM_FIELDING_DP_MISS, -TARGET_WINS, -INDEX) %>%
  mutate(Data = "train")

test_impute_input <- test_prep %>%
  select(-TEAM_BATTING_SO_MISS, -TEAM_BASERUN_SB_MISS, -TEAM_BASERUN_CS_MISS,
         -TEAM_BATTING_HBP_MISS,-TEAM_FIELDING_DP_MISS, -INDEX) %>%
  mutate(Data = "test")

# Combine training and testing for identical imputation
impute_input <- rbind(train_impute_input, test_impute_input)

# Impute missing using decision trees
mice_df <- impute_input[1:(ncol(impute_input) - 1)]
impute_missing <- mice(mice_df,
                       m = 1, method = "cart", seed = 9798, maxit = 5)

# Save imputed data set
impute_output <- complete(impute_missing, 1)

# Add label of train vs. test
impute_output$Data <- impute_input$Data

# Feature Creation
test_final <- test_prep %>%
  select(INDEX,
         TEAM_BATTING_SO_MISS,
         TEAM_BASERUN_SB_MISS,
         TEAM_BASERUN_CS_MISS,
         TEAM_BATTING_HBP_MISS,
         TEAM_FIELDING_DP_MISS) %>%
  cbind(., impute_output[impute_output$Data == "test", ]) %>%
  select(-Data) %>%
  mutate(TEAM_BATTING_1B_CLOG = if(TEAM_BATTING_1B >
                                   quantile(train_prep$TEAM_BATTING_1B,
                                            probs = c(0.99),
                                            na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BATTING_1B, probs = c(0.99),na.rm = TRUE) + 1)
  } else if (TEAM_BATTING_1B < 
             quantile(train_prep$TEAM_BATTING_1B, probs = c(0.01),na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BATTING_1B, probs = c(0.01),na.rm = TRUE) + 1)
  } else {log(TEAM_BATTING_1B + 1)},
  
  
  TEAM_BATTING_2B_C = if(TEAM_BATTING_2B >
                         quantile(train_prep$TEAM_BATTING_2B,
                                  probs = c(0.99),
                                  na.rm = TRUE)){
    quantile(train_prep$TEAM_BATTING_2B, probs = c(0.99),na.rm = TRUE) 
  } else if (TEAM_BATTING_2B < 
             quantile(train_prep$TEAM_BATTING_2B, probs = c(0.01),na.rm = TRUE)){
    quantile(train_prep$TEAM_BATTING_2B, probs = c(0.01),na.rm = TRUE)
  } else {TEAM_BATTING_2B},
  
  TEAM_BATTING_3B_CLOG = if(TEAM_BATTING_3B >
                            quantile(train_prep$TEAM_BATTING_3B,
                                     probs = c(0.99),
                                     na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BATTING_3B, probs = c(0.99),na.rm = TRUE) + 1)
  } else if (TEAM_BATTING_3B < 
             quantile(train_prep$TEAM_BATTING_3B, probs = c(0.05),na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BATTING_3B, probs = c(0.05),na.rm = TRUE) + 1)
  } else {log(TEAM_BATTING_3B + 1)},
  
  
  TEAM_BASERUN_SB_CLOG = if(TEAM_BASERUN_SB >
                            quantile(train_prep$TEAM_BASERUN_SB,
                                     probs = c(0.99),
                                     na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.99),na.rm = TRUE) + 1)
  } else if (TEAM_BASERUN_SB < 
             quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.05),na.rm = TRUE)){
    log(quantile(train_prep$TEAM_BASERUN_SB, probs = c(0.05),na.rm = TRUE) + 1)
  } else {log(TEAM_BASERUN_SB + 1)},
  
  
  TEAM_FIELDING_E_CLOG = if(TEAM_FIELDING_E >
                            quantile(train_prep$TEAM_FIELDING_E,
                                     probs = c(0.99),
                                     na.rm = TRUE)){
    log(quantile(train_prep$TEAM_FIELDING_E, probs = c(0.99),na.rm = TRUE) + 1)
  } else if (TEAM_FIELDING_E < 
             quantile(train_prep$TEAM_FIELDING_E, probs = c(0.01),na.rm = TRUE)){
    log(quantile(train_prep$TEAM_FIELDING_E, probs = c(0.01),na.rm = TRUE) + 1)
  } else {log(TEAM_FIELDING_E + 1)},
  
  
  TEAM_FIELDING_DP_C = if(TEAM_FIELDING_DP >
                          quantile(train_prep$TEAM_FIELDING_DP,
                                   probs = c(0.99),
                                   na.rm = TRUE)){
    quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.99),na.rm = TRUE) 
  } else if (TEAM_FIELDING_DP < 
             quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.01),na.rm = TRUE)){
    quantile(train_prep$TEAM_FIELDING_DP, probs = c(0.01),na.rm = TRUE)
  } else {TEAM_FIELDING_DP},
  
  TEAM_BATTING_SO_C = if(TEAM_BATTING_SO >
                         quantile(train_prep$TEAM_BATTING_SO,
                                  probs = c(0.99),
                                  na.rm = TRUE)){
    quantile(train_prep$TEAM_BATTING_SO, probs = c(0.99),na.rm = TRUE) 
  } else if (TEAM_BATTING_SO < 
             quantile(train_prep$TEAM_BATTING_SO, probs = c(0.01),na.rm = TRUE)){
    quantile(train_prep$TEAM_BATTING_SO, probs = c(0.01),na.rm = TRUE)
  } else {TEAM_BATTING_SO}) %>%
  
  select(-TEAM_BATTING_H, -TEAM_BATTING_1B, -TEAM_BATTING_2B, -TEAM_BATTING_3B,
         -TEAM_BASERUN_SB, -TEAM_FIELDING_E, -TEAM_FIELDING_DP, -TEAM_BATTING_SO )

# Score (assignment asks for hard-coded coefficients and save output
# Some very low scores need flooring as they are outliers

scores <- test_final %>%
  mutate(P_TARGET_WINS = round(-276.0711 +   
                                 0.2491296 * TEAM_BATTING_HR  + 
                                 0.02751336 * TEAM_BATTING_BB + 
                                 0.04213978 * TEAM_BASERUN_CS + 
                                 0.02432542 * TEAM_BATTING_HBP +
                                 57.04187 * TEAM_BATTING_1B_CLOG + 
                                 0.01864378 * TEAM_BATTING_2B_C  + 
                                 8.182519 * TEAM_BATTING_3B_CLOG + 
                                 5.449530 * TEAM_BASERUN_SB_CLOG - 
                                 21.19661 * TEAM_FIELDING_E_CLOG - 
                                 0.1330793 * TEAM_FIELDING_DP_C +  
                                 6.287021 * TEAM_BATTING_SO_MISS + 
                                 26.46384 * TEAM_BASERUN_SB_MISS + 
                                 2.572081 * TEAM_BASERUN_CS_MISS + 
                                 5.292837 * TEAM_BATTING_HBP_MISS +
                                 4.294441 * TEAM_FIELDING_DP_MISS -
                                 0.03003720 * TEAM_BATTING_HR * TEAM_BASERUN_SB_CLOG -    
                                 0.002560585 * TEAM_FIELDING_E_CLOG * TEAM_BATTING_SO_C ,
                               2),
         P_TARGET_WINS = ifelse(P_TARGET_WINS < 20, 20, P_TARGET_WINS)) %>%
  select(INDEX, P_TARGET_WINS)

# Save output
write.csv(scores, "PrabhatThakur_Unit1_Assignment_test_scores.csv", row.names = FALSE)