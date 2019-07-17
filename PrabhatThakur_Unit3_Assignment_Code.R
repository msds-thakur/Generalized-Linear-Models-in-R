# Unit 3 Assignment > Wine Sales Problem
# MSDS 411 Section 56
# Prabhat Thakur 05/30/2019

#call libraries
library(RColorBrewer) # nolint
library(car)
library(reshape2)
library(ggplot2)  # For graphical tools
library(MASS)  # For some advanced statistics
library(pscl) # For "counting" models (e.g., Poisson and Negative Binomial)
library(dplyr)
library(caret)
library(rpart)
library(moments)
library(corrplot)
library(party)
library(ROCR)
require(gridExtra)
library(psych)
library(mice)


#Designated proper working environment#
setwd("C:/Users/THAKPR8/Documents/MSDS/411_Sec56/Unit3/Assignment/")

# Load data
training <- read.csv("Wine_Training.csv")
testing <- read.csv("Wine_Test.csv")

# Setting colors for plots
pal <- brewer.pal(4, "Set1")


##########################################################
# Part 1: Data Exploration
#########################################################

str(training)
summary(training)
describe(training %>%  select(-INDEX))
str(testing)
summary(testing)

# High level summary of the targets
table(training$TARGET)

#examine the target variable
ggplot(data=training, aes(training$TARGET)) + 
  geom_histogram(binwidth =1, 
                 col="BLUE", 
                 aes(fill=..count..))+
  scale_fill_gradient("Count", low = "blue", high = "red")

ggplot(training, aes(TARGET)) +
  geom_bar(fill = pal[3], color = "black") +
  scale_x_continuous(breaks = seq(0, 8, by = 1)) +
  ggtitle("Training Target Distribution") +
  ylab("Number of Wines") +
  xlab("Cases of Wines Sold") +
  geom_text(stat='count', aes(label=..count..),vjust = -0.1 ,size=4)

# Compare variance to mean for target values
mean(training$TARGET)
var(training$TARGET)

# Compare variance to mean for target values greater than zero
target_analysis <- training %>%
  filter(TARGET > 0) %>%
  mutate(TARGET_M1 = TARGET -1) %>%
  summarise(M = mean(TARGET_M1), V = var(TARGET_M1))

print(target_analysis)

# Visualize missing values in the data
MissingCount <- training %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
  melt(id.vars = NULL) %>% filter(value > 0) %>% rename("Missing_Value" = value)

MissingFraction <- training %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
  melt(id.vars = NULL) %>%  mutate(value = round(value / nrow(training),4)) %>% filter(value > 0) %>% rename("Missing_Fraction" = value)

p1 <- ggplot(MissingCount, aes(x = variable, weight = Missing_Value)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values") +
  ggtitle("Missing Values - Training") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  geom_text(stat='count', aes(label=..count..),vjust = -0.1 ,size=3) 
  
p2 <- ggplot(MissingFraction, aes(x = variable, weight = Missing_Fraction)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values Fraction") +
  ggtitle("Missing Values Fraction - Training") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_text(stat='count', aes(label=..count..),vjust = -0.1 ,size=3) 

MissingCountT <- testing %>% select(-TARGET) %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
  melt(id.vars = NULL) %>%  filter(value > 0) %>% rename("Missing_Value" = value)

MissingFractionT <- testing %>% select(-TARGET) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
  melt(id.vars = NULL) %>%  mutate(value = round(value / nrow(testing),4)) %>% filter(value > 0) %>% rename("Missing_Fraction" = value)

p3 <- ggplot(MissingCountT, aes(x = variable, weight = Missing_Value)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values") +
  ggtitle("Missing Values - Test DataSet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_text(stat='count', aes(label=..count..),vjust = -0.1 ,size=3)

p4 <- ggplot(MissingFractionT, aes(x = variable, weight = Missing_Fraction)) +
  geom_bar(color = 'orange') +
  xlab("Variable (Those with Missing Values)") +
  ylab("Missing Values Fraction") +
  ggtitle("Missing Values Fraction - Test DataSet") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8))+
  geom_text(stat='count', aes(label=..count..),vjust = -0.1 ,size=3)

grid.arrange(p1, p2,p3,p4, nrow= 2,ncol=2)


# Training predictor correlation for continuous variables
corr_input <- training %>%  select(-INDEX) %>%   na.omit()

corr <- cor(corr_input,use = "pairwise.complete.obs")
corrplot(corr, method = "square")

corr_melt <- melt(round(corr, 2))

ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.75) +
  scale_fill_gradient2(low = pal[3], high = pal[4], mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(.75, "line")) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation - Training Data")

# Merge training and testing for analyzing predictors
df1 <- training %>%   select(-TARGET, -INDEX) %>%  mutate(Data = "Training")
df2 <- testing %>%   select(-TARGET, -INDEX) %>%  mutate(Data = "Testing")
df <- rbind(df1, df2) %>% melt(id.vars = "Data") %>% na.omit()

# High level summary of all predictors
summary(rbind(df1, df2))

# Box plots for Predictor Variables 
ggplot(df, aes(x = variable, y = value, color = Data)) +
  xlab("Variable") +
  ylab("Count") +
  ggtitle("Box Plots for Predictor Variables") +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_blank())

# Histograms for Predictor Variables
ggplot(df, aes(value, fill = Data)) +
  xlab("Variable Value") +
  ylab("Count") +
  ggtitle("Histograms for Predictor Variables") +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Univariate relationships with the target
# Different approach having y on x-axis
train_melt <- training %>%  select(-INDEX) %>%
  melt(., id.vars = "TARGET") %>%
  na.omit() %>%
  group_by(variable, TARGET) %>%
  summarise(value = mean(value))

ggplot(train_melt, aes(x = as.factor(TARGET), y = value, group = 1)) +
  geom_line(color='red') +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Univariate Relationship with Continuous Target") +
  xlab("Target") +
  ylab("Mean Predictor Variable Value") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

##########################################################
# Part 2: Data Preparation
#########################################################
# Function to impute a variable with the mean
impute_mean <- function(v, base) {
  ifelse(is.na(v), mean(base, na.rm = TRUE), v)
}

# Function to create missing indicator
miss_ind <- function(v) {
  ifelse(is.na(v), 1, 0)
}

# Function to cap/flr a variable
cap_flr <- function(v, base, mi = 0.025, ma = 0.975) {
  ifelse(v > quantile(base, na.rm = TRUE, ma), quantile(base, na.rm = TRUE, ma),
         ifelse(v < quantile(base, na.rm = TRUE, mi), 
                quantile(base, na.rm = TRUE, mi), v))
}
  
# Impute missing with the training mean
# Impute Missing STARS to 0
# Create missing indicator variables
# Cap and floor most variables at 2.5% and 97.5% of training data

table(training$STARS)
table(training$LabelAppeal)

train_process1 <- training %>%
  select(-INDEX) %>%
  mutate(FixedAcidity_cf = cap_flr(FixedAcidity, training$FixedAcidity),
         VolatileAcidity_cf = cap_flr(VolatileAcidity, training$VolatileAcidity),
         CitricAcid_cf = cap_flr(CitricAcid, training$CitricAcid),
         ResidualSugar_imp = impute_mean(ResidualSugar, training$ResidualSugar),
         ResidualSugar_mind = miss_ind(ResidualSugar),
         ResidualSugar_cf = cap_flr(ResidualSugar_imp, training$ResidualSugar),
         
         Chlorides_imp = impute_mean(Chlorides, training$Chlorides),
         Chlorides_mind = miss_ind(Chlorides),
         Chlorides_cf = cap_flr(Chlorides_imp, training$Chlorides),
         
         FreeSulfurDioxide_imp = impute_mean(FreeSulfurDioxide, training$FreeSulfurDioxide),
         FreeSulfurDioxide_mind = miss_ind(FreeSulfurDioxide),
         FreeSulfurDioxide_cf = cap_flr(FreeSulfurDioxide_imp, training$FreeSulfurDioxide),
         
         TotalSulfurDioxide_imp = impute_mean(TotalSulfurDioxide,training$TotalSulfurDioxide),
         TotalSulfurDioxide_mind = miss_ind(TotalSulfurDioxide),
         TotalSulfurDioxide_cf = cap_flr(TotalSulfurDioxide_imp, training$TotalSulfurDioxide),
         
         Density_cf = cap_flr(Density, training$Density),
         
         pH_imp = impute_mean(pH, training$pH),
         pH_mind = miss_ind(pH),
         pH_cf = cap_flr(pH_imp, training$pH),
         
         Sulphates_imp = impute_mean(Sulphates, training$Sulphates),
         Sulphates_mind = miss_ind(Sulphates),
         Sulphates_cf = cap_flr(Sulphates_imp, training$Sulphates),
         
         Alcohol_imp = impute_mean(Alcohol, training$Alcohol),
         Alcohol_mind = miss_ind(Alcohol),
         Alcohol_cf = cap_flr(Alcohol_imp, training$Alcohol),
         
         Star_imp = impute_mean(STARS, training$STARS),
         Star_mind = miss_ind(STARS)) %>%
  
  select(-FixedAcidity,-VolatileAcidity,-CitricAcid,-ResidualSugar,-ResidualSugar_imp,
         -Chlorides,-Chlorides_imp,-FreeSulfurDioxide,-FreeSulfurDioxide_imp,
         -TotalSulfurDioxide,-TotalSulfurDioxide_imp,-Density,-pH,-pH_imp,
         -Sulphates,-Sulphates_imp,-Alcohol,-Alcohol_imp,-STARS)

summary(train_process1)

vars_categorical1 <- c("ResidualSugar_mind", "Chlorides_mind",
                       "FreeSulfurDioxide_mind","TotalSulfurDioxide_mind",
                       "pH_mind", "Sulphates_mind","Alcohol_mind", "Star_mind")

vars_continuous <- c("FixedAcidity_cf","VolatileAcidity_cf","CitricAcid_cf","ResidualSugar_cf", "Chlorides_cf",
                     "FreeSulfurDioxide_cf","TotalSulfurDioxide_cf","Density_cf",
                     "pH_cf", "Sulphates_cf","Alcohol_cf","LabelAppeal","AcidIndex", "Star_imp")

# Univariate relationship with target: Categorical group 1
train_melt <- train_process1 %>%
  select(vars_categorical1, TARGET) %>%
  melt(., id.vars = "TARGET") %>%
  group_by(variable, value) %>%
  summarise(n = n(),
            m = mean(TARGET),
            v = var(TARGET)) %>%
  ungroup() %>%
  mutate(m_plus = m + qnorm(0.95) * sqrt(v / n),
         m_minus = m - qnorm(0.95) * sqrt(v / n))

ggplot(data = train_melt, aes(value, m)) +
  geom_line() +
  geom_line(y = train_melt$m_plus, color = pal[1]) +
  geom_line(y = train_melt$m_minus, color = pal[1]) +
  facet_wrap(~variable, scales = "free") +
  scale_x_continuous(breaks = seq(0, 13, by = 1)) +
  xlab("Variable Value") +
  ylab("Average Cases Sold") +
  ggtitle("Training Univariate Cases Sold with 90% CI") +
  theme(strip.text.x = element_text(size = 6),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Univariate relationships with the target
# Different approach having y on x-axis
train_melt <- train_process1 %>%
  select(TARGET, vars_continuous) %>%
  melt(., id.vars = "TARGET") %>%
  na.omit() %>%
  group_by(variable, TARGET) %>%
  summarise(value = mean(value))

ggplot(train_melt, aes(x = as.factor(TARGET), y = value, group = 1)) +
  geom_line(color='red') +
  facet_wrap(~variable, scales = "free") +
  ggtitle("Univariate Relationship with Continuous Target") +
  xlab("Target") +
  ylab("Mean Predictor Variable Value") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))


# Part 3 Build Models

# Split training data to observe how models fit on holdout data 
set.seed(999)
trainIndex <- createDataPartition(train_process1$TARGET, p = 0.7, list = FALSE)

train_trn <- train_process1[ trainIndex, ]
train_val <- train_process1[-trainIndex, ]

# Validation mse function
val_mse <- function(model, log = FALSE) {
  if (log == TRUE) {
    predictions <- exp(predict(model, train_val, type = "response"))
  } else {
    predictions <- predict(model, train_val, type = "response")
  }
  mse <- data.frame(predictions, train_val$TARGET)
  names(mse) <- c("predictions", "TARGET")
  mse <- mse %>%
    mutate(sq_error = (TARGET - predictions) ^ 2) %>%
    summarise(mse_model1 = mean(sq_error))
  return(mse[1, 1])
}

# Training mse function
train_mse <- function(model, log = FALSE) {
  if (log == TRUE) {
    predictions <- exp(predict(model, train_trn, type = "response"))
  } else {
    predictions <- predict(model, train_trn, type = "response")
  }
  mse <- data.frame(predictions, train_trn$TARGET)
  names(mse) <- c("predictions", "TARGET")
  mse <- mse %>%
    mutate(sq_error = (TARGET - predictions) ^ 2) %>%
    summarise(mse_model1 = mean(sq_error))
  return(mse[1, 1])
}

# Function for general result printing
model_diag <- function(model) {
  print(summary(model))
  print("VIFs:")
  print(vif(model))
  print(paste0("AIC: ", AIC(model)))
  print(paste0("Training MSE: ", train_mse(model)))
  print(paste0("Validation MSE: ", val_mse(model)))
}

# Model 1
# Multiple Linear Regression with only Label, STARS, and AcidIndex as baseline
model1 <- lm(TARGET ~ LabelAppeal + Star_imp + AcidIndex,
             data = train_trn)

model_diag(model1)

# Model 2
# Multiple Linear Regression with backwards selection from the baseline
model2_spec <- lm(TARGET ~ ., data = train_trn)
model2 <- stepAIC(model2_spec, direction = "backward", trace = 0)

model_diag(model2)

# Model 3
# Poisson regression with backwards selection
model3_spec <- glm(TARGET ~ ., data = train_trn, 
                   family = poisson(link = "log"))
model3 <- stepAIC(model3_spec, direction = "backward", trace = 0)

model_diag(model3)

# Model 4
# Zero-inflated Poisson regression starting with Model 3 terms
# Manual selection due to fitting issues
# Started with Model 3 terms for both zinf and base parts of models then
# removed terms until it was possible to get convergence

model4 <- zeroinfl(TARGET ~ LabelAppeal + AcidIndex + VolatileAcidity_cf + 
                     Chlorides_cf + FreeSulfurDioxide_cf + TotalSulfurDioxide_cf + 
                     Alcohol_cf + Star_imp + Star_mind | 
                     LabelAppeal + Alcohol_cf+ Star_imp , 
                   data = train_trn, 
                   dist = "poisson", link = "log")

model_diag(model4)

# Model 5
# Negative Binomial regression with same terms as Model 3 due to fit issues
model5 <- glm.nb(model3$formula, data = train_trn)
model_diag(model5)

model5_spec <- glm.nb(TARGET ~ ., data = train_trn)
model5a <- stepAIC(model5_spec, direction = "backward", trace = 0)

model_diag(model5a)


# Model 6
# Zero-inflated Negative Binomial regression using Model 4 terms due to fit
# issues
model6 <- zeroinfl(TARGET ~ LabelAppeal + AcidIndex + VolatileAcidity_cf + 
                     Chlorides_cf + FreeSulfurDioxide_cf + TotalSulfurDioxide_cf + 
                     Alcohol_cf + Star_imp + Star_mind | 
                     LabelAppeal + Alcohol_cf+ Star_imp ,
                   data = train_trn, 
                   dist = "negbin", link = "log")
model_diag(model6)


# Bonus Models 7 hurdle approach first part logistic regression and second part Poisson regression 
# Logistic Regression with backwards selection predicting whether any is sold
model7_Logistic_spec <- glm(ifelse(TARGET > 0, 1, 0) ~ . -TARGET,
                   data = train_trn, family = binomial(link = "logit"))
model7_Logistic <- stepAIC(model7_Logistic_spec, direction = "backward", trace = 0)

summary(model7_Logistic)
vif(model7_Logistic)

# Poisson regression conditional on being sold
# Subtracts 1 as zero should be handled exclusively by logistic regression
model7_Poisson_spec <- glm((TARGET - 1) ~ . -TARGET,
                   data = train_trn[train_trn$TARGET > 0, ], 
                   family = poisson(link = "log"))
model7_Poisson <- stepAIC(model7_Poisson_spec, direction = "backward", trace = 0)

summary(model7_Poisson)
vif(model7_Poisson)

# Training MSE
pred7_Logistic <- predict(model7_Logistic, train_trn, type = "response")
pred7_Poisson <- predict(model7_Poisson, train_trn, type = "response") + 1
mse_train <- data.frame(pred7_Logistic, pred7_Poisson, train_trn$TARGET)

names(mse_train) <- c("pred7_Logistic", "pred7_Poisson", "TARGET")
mse_train <- mse_train %>%
  mutate(pred = pred7_Logistic * pred7_Poisson,
         sq_error = (TARGET - pred) ^ 2) %>%
  summarize(mse_model = mean(sq_error))
print(mse_train[1, 1])

# Validation MSE
pred7_Logistic <- predict(model7_Logistic, train_val, type = "response")
pred7_Poisson <- predict(model7_Poisson, train_val, type = "response") + 1
mse_train <- data.frame(pred7_Logistic, pred7_Poisson, train_val$TARGET)

names(mse_train) <- c("pred7_Logistic", "pred7_Poisson", "TARGET")
mse_train <- mse_train %>%
  mutate(pred = pred7_Logistic * pred7_Poisson,
         sq_error = (TARGET - pred) ^ 2) %>%
  summarize(mse_model = mean(sq_error))
print(mse_train[1, 1])


# Bonus Model 8 Decision tree with default options
# Fit model using decision tree

model8 <- rpart(TARGET ~ ., data = train_trn)
summary(model8)

# Training MSE
predictions <- predict(model8, train_trn)
mse <- data.frame(predictions, train_trn$TARGET)
names(mse) <- c("predictions", "TARGET")
mse <- mse %>%
  mutate(sq_error = (TARGET - predictions) ^ 2) %>%
  summarise(mse_model1 = mean(sq_error))
print(mse[1, 1])

# Validation MSE
predictions <- predict(model8, train_val)
mse <- data.frame(predictions, train_val$TARGET)
names(mse) <- c("predictions", "TARGET")
mse <- mse %>%
  mutate(sq_error = (TARGET - predictions) ^ 2) %>%
  summarise(mse_model1 = mean(sq_error))
print(mse[1, 1])

# Bonus Model 9 ######################
# Use decision tree for variable selection and missing value imputation
training <- read.csv("Wine_Training.csv")
testing <- read.csv("Wine_Test.csv")

train_process2 <- training %>%
    mutate(ResidualSugar_mind = miss_ind(ResidualSugar),
         Chlorides_mind = miss_ind(Chlorides),
         FreeSulfurDioxide_mind = miss_ind(FreeSulfurDioxide),
         TotalSulfurDioxide_mind = miss_ind(TotalSulfurDioxide),
         pH_mind = miss_ind(pH),
         Sulphates_mind = miss_ind(Sulphates),
         Alcohol_mind = miss_ind(Alcohol),
         Star_mind = miss_ind(STARS))

test_process2 <- testing %>%
   mutate(ResidualSugar_mind = miss_ind(ResidualSugar),
         Chlorides_mind = miss_ind(Chlorides),
         FreeSulfurDioxide_mind = miss_ind(FreeSulfurDioxide),
         TotalSulfurDioxide_mind = miss_ind(TotalSulfurDioxide),
         pH_mind = miss_ind(pH),
         Sulphates_mind = miss_ind(Sulphates),
         Alcohol_mind = miss_ind(Alcohol),
         Star_mind = miss_ind(STARS))

# Prepare data
train_impute_input <- train_process2 %>%
  select(-INDEX, -TARGET,-ResidualSugar_mind,
         -Chlorides_mind,
         -FreeSulfurDioxide_mind,
         -TotalSulfurDioxide_mind,
         -pH_mind,
         -Sulphates_mind,
         -Alcohol_mind,
         -Star_mind) %>%
  mutate(Data = "train")

test_impute_input <- test_process2 %>%
  select(-INDEX, -TARGET,-ResidualSugar_mind,
         -Chlorides_mind,
         -FreeSulfurDioxide_mind,
         -TotalSulfurDioxide_mind,
         -pH_mind,
         -Sulphates_mind,
         -Alcohol_mind,
         -Star_mind) %>%
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


#######
train_final <- train_process2 %>%
  select(TARGET,
         ResidualSugar_mind,
         Chlorides_mind,
         FreeSulfurDioxide_mind,
         TotalSulfurDioxide_mind,
         pH_mind,
         Sulphates_mind,
         Alcohol_mind,
         Star_mind) %>%
  cbind(., impute_output[impute_output$Data == "train", ]) %>%
  select(-Data) %>%
  mutate(FixedAcidity_cf = cap_flr(FixedAcidity, train_process2$FixedAcidity),
         VolatileAcidity_cf = cap_flr(VolatileAcidity, train_process2$VolatileAcidity),
         CitricAcid_cf = cap_flr(CitricAcid, train_process2$CitricAcid),
         ResidualSugar_cf = cap_flr(ResidualSugar, train_process2$ResidualSugar),
         Chlorides_cf = cap_flr(Chlorides, train_process2$Chlorides),
         FreeSulfurDioxide_cf = cap_flr(FreeSulfurDioxide, train_process2$FreeSulfurDioxide),
         TotalSulfurDioxide_cf = cap_flr(TotalSulfurDioxide, train_process2$TotalSulfurDioxide),
         Density_cf = cap_flr(Density, train_process2$Density),
         pH_cf = cap_flr(pH, train_process2$pH),
         Sulphates_cf = cap_flr(Sulphates, train_process2$Sulphates),
         Alcohol_cf = cap_flr(Alcohol, train_process2$Alcohol)) %>%
  
  select(-FixedAcidity,-VolatileAcidity,-CitricAcid,-ResidualSugar,
         -Chlorides,-FreeSulfurDioxide,
         -TotalSulfurDioxide,-Density,-pH,
         -Sulphates,-Alcohol)


trainIndex <- createDataPartition(train_final$TARGET, p = 0.7, list = FALSE)

train_trn <- train_final[ trainIndex, ]
train_val <- train_final[-trainIndex, ]


# Using use variables deemed most important by random forest model
train_forest <- cforest(TARGET ~ .,
                        data = train_trn,
                        control = cforest_unbiased(mtry = 3, ntree = 100))
forest_imp <- varimp(train_forest, conditional = FALSE)
sort(forest_imp, decreasing = TRUE)

# Select the top predictors
vars <- paste("TARGET ~ ",
              paste(names(head(sort(forest_imp, decreasing = TRUE), 12)),
                    collapse = " + "))


model9_Logistic_spec <- glm(ifelse(TARGET > 0, 1, 0) ~ Star_mind + STARS + LabelAppeal + AcidIndex + VolatileAcidity_cf + 
                              Chlorides_cf + Alcohol_cf + Density_cf + TotalSulfurDioxide_cf + FreeSulfurDioxide_cf + 
                              FixedAcidity_cf + ResidualSugar_cf,
                            data = train_trn, family = binomial(link = "logit"))
model9_Logistic <- stepAIC(model9_Logistic_spec, direction = "backward", trace = 0)

summary(model9_Logistic)
vif(model9_Logistic)

# Poisson regression conditional on being sold
# Subtracts 1 as zero should be handled exclusively by logistic regression
model9_Poisson_spec <- glm((TARGET - 1) ~ Star_mind + STARS + LabelAppeal + AcidIndex + VolatileAcidity_cf + 
                             Chlorides_cf + Alcohol_cf + Density_cf + TotalSulfurDioxide_cf + FreeSulfurDioxide_cf + 
                             FixedAcidity_cf + ResidualSugar_cf,
                           data = train_trn[train_trn$TARGET > 0, ], 
                           family = poisson(link = "log"))
model9_Poisson <- stepAIC(model9_Poisson_spec, direction = "backward", trace = 0)

summary(model9_Poisson)
vif(model9_Poisson)

# Training MSE
pred9_Logistic <- predict(model9_Logistic, train_trn, type = "response")
pred9_Poisson <- predict(model9_Poisson, train_trn, type = "response") + 1
mse_train <- data.frame(pred9_Logistic, pred9_Poisson, train_trn$TARGET)

names(mse_train) <- c("pred9_Logistic", "pred9_Poisson", "TARGET")
mse_train <- mse_train %>%
  mutate(pred = pred9_Logistic * pred9_Poisson,
         sq_error = (TARGET - pred) ^ 2) %>%
  summarize(mse_model = mean(sq_error))
print(mse_train[1, 1])

# Validation MSE
pred9_Logistic <- predict(model9_Logistic, train_val, type = "response")
pred9_Poisson <- predict(model9_Poisson, train_val, type = "response") + 1
mse_train <- data.frame(pred9_Logistic, pred9_Poisson, train_val$TARGET)

names(mse_train) <- c("pred9_Logistic", "pred9_Poisson", "TARGET")
mse_train <- mse_train %>%
  mutate(pred = pred9_Logistic * pred9_Poisson,
         sq_error = (TARGET - pred) ^ 2) %>%
  summarize(mse_model = mean(sq_error))
print(mse_train[1, 1])



# Print coefficients for stand alone program
options(scipen=999)
print(model7_Logistic$coefficients)
print(model7_Poisson$coefficients)

print(model9_Logistic$coefficients)
print(model9_Poisson$coefficients)


#########################################################################
# Part 4: Stand Alone Scoring Program

# Stand alone program
library(dplyr)

# Load data
training <- read.csv("Wine_Training.csv")
testing <- read.csv("Wine_Test.csv")

# Function to impute a variable with the mean
impute_mean <- function(v, base) {
  ifelse(is.na(v), mean(base, na.rm = TRUE), v)
}

# Function to create missing indicator
miss_ind <- function(v) {
  ifelse(is.na(v), 1, 0)
}

# Function to cap/flr a variable
cap_flr <- function(v, base, mi = 0.025, ma = 0.975) {
  ifelse(v > quantile(base, na.rm = TRUE, ma), quantile(base, na.rm = TRUE, ma),
         ifelse(v < quantile(base, na.rm = TRUE, mi), 
                quantile(base, na.rm = TRUE, mi), v))
}

# Impute missing with the training mean
# Create missing indicator variables
# Cap and floor most variables at 2.5% and 97.5% of training data

test_process1 <- testing %>%
  mutate(FixedAcidity_cf = cap_flr(FixedAcidity, training$FixedAcidity),
         VolatileAcidity_cf = cap_flr(VolatileAcidity, training$VolatileAcidity),
         CitricAcid_cf = cap_flr(CitricAcid, training$CitricAcid),
         ResidualSugar_imp = impute_mean(ResidualSugar, training$ResidualSugar),
         ResidualSugar_mind = miss_ind(ResidualSugar),
         ResidualSugar_cf = cap_flr(ResidualSugar_imp, training$ResidualSugar),
         
         Chlorides_imp = impute_mean(Chlorides, training$Chlorides),
         Chlorides_mind = miss_ind(Chlorides),
         Chlorides_cf = cap_flr(Chlorides_imp, training$Chlorides),
         
         FreeSulfurDioxide_imp = impute_mean(FreeSulfurDioxide, training$FreeSulfurDioxide),
         FreeSulfurDioxide_mind = miss_ind(FreeSulfurDioxide),
         FreeSulfurDioxide_cf = cap_flr(FreeSulfurDioxide_imp, training$FreeSulfurDioxide),
         
         TotalSulfurDioxide_imp = impute_mean(TotalSulfurDioxide,training$TotalSulfurDioxide),
         TotalSulfurDioxide_mind = miss_ind(TotalSulfurDioxide),
         TotalSulfurDioxide_cf = cap_flr(TotalSulfurDioxide_imp, training$TotalSulfurDioxide),
         
         Density_cf = cap_flr(Density, training$Density),
         
         pH_imp = impute_mean(pH, training$pH),
         pH_mind = miss_ind(pH),
         pH_cf = cap_flr(pH_imp, training$pH),
         
         Sulphates_imp = impute_mean(Sulphates, training$Sulphates),
         Sulphates_mind = miss_ind(Sulphates),
         Sulphates_cf = cap_flr(Sulphates_imp, training$Sulphates),
         
         Alcohol_imp = impute_mean(Alcohol, training$Alcohol),
         Alcohol_mind = miss_ind(Alcohol),
         Alcohol_cf = cap_flr(Alcohol_imp, training$Alcohol),
         
         Star_imp = impute_mean(STARS, training$STARS),
         Star_mind = miss_ind(STARS)) %>%
  
  select(-FixedAcidity,-VolatileAcidity,-CitricAcid,-ResidualSugar,-ResidualSugar_imp,
         -Chlorides,-Chlorides_imp,-FreeSulfurDioxide,-FreeSulfurDioxide_imp,
         -TotalSulfurDioxide,-TotalSulfurDioxide_imp,-Density,-pH,-pH_imp,
         -Sulphates,-Sulphates_imp,-Alcohol,-Alcohol_imp,-STARS)

# Score logistic/poisson hurdle model
scores <- test_process1 %>%
  mutate(SCORE_ZERO1 = 2.7677782229 +
           LabelAppeal * -0.4608923867 +
           AcidIndex	*	-0.3828787047	+
           VolatileAcidity_cf	*	-0.1794974473	+
           Chlorides_cf	*	-0.2816060860	+
           FreeSulfurDioxide_cf	*	0.0006699291	+
           TotalSulfurDioxide_mind	*	0.2196631550	+
           TotalSulfurDioxide_cf	*	 0.0008866712	+
           pH_cf	*	-0.1947236759	+
           Sulphates_cf	*	-0.0987064933	+
           Alcohol_cf	*	-0.0269410701	+
           Star_imp	*	2.5116450460	+
           Star_mind	*	-4.4438135083	,
         SCORE_ZERO = exp(SCORE_ZERO1) / (1 + exp(SCORE_ZERO1)),
         SCORE_NONZERO = exp(0.83367212809 +
                               LabelAppeal * 0.29246908139 +
                               AcidIndex	*	-0.02100540105	+
                               VolatileAcidity_cf	*	-0.01538854250	+
                               TotalSulfurDioxide_cf	*	 -0.00005241572	+
                               Alcohol_cf	*	0.00987772346	+
                               Star_imp	*	0.12190218156	+
                               Star_mind	*	-0.20961823709	) + 1,
         P_TARGET = SCORE_ZERO * SCORE_NONZERO) %>%
  select(INDEX, P_TARGET)

# Save output
write.csv(scores, "PrabhatThakur_Unit3_Assignment_test_scores.csv", row.names = FALSE)

summary(scores$P_TARGET)

## Score based on decison tree imputaion and variable selection
test_final <- test_process2 %>%
  select(TARGET,INDEX,
         ResidualSugar_mind,
         Chlorides_mind,
         FreeSulfurDioxide_mind,
         TotalSulfurDioxide_mind,
         pH_mind,
         Sulphates_mind,
         Alcohol_mind,
         Star_mind) %>%
  cbind(., impute_output[impute_output$Data == "test", ]) %>%
  select(-Data) %>%
  mutate(FixedAcidity_cf = cap_flr(FixedAcidity, train_process2$FixedAcidity),
         VolatileAcidity_cf = cap_flr(VolatileAcidity, train_process2$VolatileAcidity),
         CitricAcid_cf = cap_flr(CitricAcid, train_process2$CitricAcid),
         ResidualSugar_cf = cap_flr(ResidualSugar, train_process2$ResidualSugar),
         Chlorides_cf = cap_flr(Chlorides, train_process2$Chlorides),
         FreeSulfurDioxide_cf = cap_flr(FreeSulfurDioxide, train_process2$FreeSulfurDioxide),
         TotalSulfurDioxide_cf = cap_flr(TotalSulfurDioxide, train_process2$TotalSulfurDioxide),
         Density_cf = cap_flr(Density, train_process2$Density),
         pH_cf = cap_flr(pH, train_process2$pH),
         Sulphates_cf = cap_flr(Sulphates, train_process2$Sulphates),
         Alcohol_cf = cap_flr(Alcohol, train_process2$Alcohol)) %>%
  
  select(-FixedAcidity,-VolatileAcidity,-CitricAcid,-ResidualSugar,
         -Chlorides,-FreeSulfurDioxide,
         -TotalSulfurDioxide,-Density,-pH,
         -Sulphates,-Alcohol)

# Score logistic/poisson hurdle model
scores <- test_final %>%
  mutate(SCORE_ZERO1 = 4.5215310764 +
           LabelAppeal * -0.4561086287 +
           AcidIndex	*	-0.3703783217	+
           VolatileAcidity_cf	*	-0.2211563783	+
           Chlorides_cf	*	-0.2270893593	+
           FreeSulfurDioxide_cf	*	0.0007036394	+
           TotalSulfurDioxide_cf	*	 0.0007786879	+
           Alcohol_cf	*	-0.0247528634	+
           STARS	*	0.6763689310	+
           Star_mind	*	-3.1458145681	,
         SCORE_ZERO = exp(SCORE_ZERO1) / (1 + exp(SCORE_ZERO1)),
         SCORE_NONZERO = exp(1.47657087 +
                               LabelAppeal * 0.29224896 +
                               AcidIndex	*	-0.02484326	+
                               Density_cf	*	-0.62003541	+
                               Alcohol_cf	*	0.00936499	+
                               STARS	*	0.11948414	+
                               Star_mind	*	-0.19798521	) + 1,
         P_TARGET = SCORE_ZERO * SCORE_NONZERO) %>%
  select(INDEX, P_TARGET)

# Save output
write.csv(scores, "PrabhatThakur_Unit3_Assignment_test_scores1.csv", row.names = FALSE)

summary(scores$P_TARGET)
summary(training$TARGET)
