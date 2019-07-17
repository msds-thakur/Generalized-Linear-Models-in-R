# Unit 2 Assignment > Insurance
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
library(corrplot)
library(rpart)
library(rpart.plot)
library(ROCR)


#Designated proper working environment#
setwd("C:/Users/THAKPR8/Documents/MSDS/411_Sec56/Unit2/Assignment2/")

# Load training and testing data
train <- read.csv("logit_insurance.csv")
test <- read.csv("logit_insurance_test.csv")

# Setting colors for plots
pal <- brewer.pal(4, "Set1")

############## Part 1: Data Exploration ##########################################################################
str(train)
summary(train)
describe(train %>%  select(-INDEX))
str(test)
summary(test)

# High level summary of the targets
table(train$TARGET_FLAG)

summary(train$TARGET_AMT)

# Visualize the distribution of the continuous model target
dataLine <- train %>%
  filter(TARGET_AMT > 0) %>%
  summarise(y75 = quantile(TARGET_AMT, 0.75),
            y25 = quantile(TARGET_AMT, 0.25),
            x75 = qnorm(0.75), x25 = qnorm(0.25)) %>%
  mutate(slope = (y75 - y25) / (x75 - x25), intercept = y75 - slope * x75)

# Check for normality in target
ggplot(train[train$TARGET_AMT > 0, ], aes(sample = TARGET_AMT)) +
  stat_qq(color = pal[2]) +
  geom_abline(data = dataLine, aes(slope = slope, intercept = intercept)) +
  xlab("Theoretical Quantiles") +
  ylab("Sample Quantiles") +
  ggtitle("Target Normal QQ-Plot")


# Check for normality in target
#ggplot(train, aes(sample = TARGET_AMT)) +
#  stat_qq(color = pal[2]) + stat_qq_line() +
#  xlab("Theoretical Quantiles") +
#  ylab("Sample Quantiles") +
#  ggtitle("Target Normal QQ-Plot") +
#  theme(legend.position = "none")

# Some initial formating and indicator creation since we have a mix of numerical and categorical
# Train Data
train_1 <- train %>%
  mutate(PARENT1_IND = ifelse(PARENT1 == "Yes", 1, 0),
         MARRIED_IND = ifelse(MSTATUS == "z_No", 0, 1),
         MALE_IND = ifelse(SEX == "z_F", 0, 1),
         COMM_USE_IND = ifelse(CAR_USE == "Commercial", 1, 0),
         RED_CAR_IND = ifelse(RED_CAR == "yes", 1, 0),
         REVOKED_IND = ifelse(REVOKED == "Yes", 1, 0),
         RURAL_IND = ifelse(URBANICITY == "z_Highly Rural/ Rural", 1, 0),
         JOB = as.factor(ifelse(JOB == "", "Other", as.character(JOB))),
         OLDCLAIM_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", OLDCLAIM))),
         BLUEBOOK_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", BLUEBOOK))),
         INCOME_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", INCOME))),
         HOME_VAL_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", HOME_VAL)))) %>%
  dplyr::select(-OLDCLAIM, -BLUEBOOK, -HOME_VAL, -INCOME, -PARENT1, -MSTATUS,
                -SEX, -CAR_USE, -RED_CAR, -REVOKED, -URBANICITY, -INDEX)
# Test Data
test_1 <- test %>%
  mutate(PARENT1_IND = ifelse(PARENT1 == "Yes", 1, 0),
         MARRIED_IND = ifelse(MSTATUS == "z_No", 0, 1),
         MALE_IND = ifelse(SEX == "z_F", 0, 1),
         COMM_USE_IND = ifelse(CAR_USE == "Commercial", 1, 0),
         RED_CAR_IND = ifelse(RED_CAR == "yes", 1, 0),
         REVOKED_IND = ifelse(REVOKED == "Yes", 1, 0),
         RURAL_IND = ifelse(URBANICITY == "z_Highly Rural/ Rural", 1, 0),
         JOB = as.factor(ifelse(JOB == "", "Other", as.character(JOB))),
         OLDCLAIM_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", OLDCLAIM))),
         BLUEBOOK_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", BLUEBOOK))),
         INCOME_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", INCOME))),
         HOME_VAL_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", HOME_VAL)))) %>%
  dplyr::select(-OLDCLAIM, -BLUEBOOK, -HOME_VAL, -INCOME, -PARENT1, -MSTATUS,
                -SEX, -CAR_USE, -RED_CAR, -REVOKED, -URBANICITY, -INDEX)


#################### Part 1: Data Exploration ##############################################
# Analyzing continuous variables

par(mfrow=c(2,2))
hist(train_1$AGE, col = "red", xlab = "Age", main = "AGE Hist")
train_10<- subset(train_1, TARGET_FLAG == 1 )
hist(log(train_10$TARGET_AMT), col = "green", xlab = "Log TARGET_AMT", main = "Log TARGET_AMT Hist")
boxplot(train_1$AGE, col = "red", main = "AGE BoxPlot")
boxplot(log(train_10$TARGET_AMT), col = "green", main = "LOG TARGET_AMT Boxplot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(train_1$TRAVTIME), col = "green", xlab = "SQRT TRAVTIME", main = "SQRT TRAVTIME Hist")
hist(train_1$YOJ, col = "blue", xlab = "YOJ", main = "YOJ Hist")
boxplot(sqrt(train_1$TRAVTIME), col = "green", main = "SQRT TRAVTIME BoxPlot")
boxplot(train_1$YOJ, col = "blue", main = "YOJ BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(sqrt(train_1$BLUEBOOK_NUM), col = "green", xlab = "SQRT BLUEBOOK", main = "SQRT BLUEBOOK Hist")
hist((train_1$TIF), col = "blue", xlab = "TIF", main = "TIF Hist")
boxplot(sqrt(train_1$BLUEBOOK_NUM), col = "green", main = "SQRT BLUEBOOK BoxPlot")
boxplot(train_1$TIF, col = "blue", main = "TIF BoxPlot")
par(mfrow=c(1,1))

par(mfrow=c(2,2))
hist(train_1$MVR_PTS, col = "red", xlab = "MVR_PTS", main = "MVR_PTS Hist")
hist(train_1$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE Hist")
boxplot(train_1$MVR_PTS, col = "red", main = "MVR_PTS BoxPlot")
boxplot(train_1$CAR_AGE, col = "blue", xlab = "CAR_AGE", main = "CAR_AGE BoxPlot")
par(mfrow=c(1,1))

vars_continuous <- c("AGE", "YOJ", "TRAVTIME", "TIF", "CAR_AGE",
                     "OLDCLAIM_NUM", "BLUEBOOK_NUM", "HOME_VAL_NUM",
                     "INCOME_NUM")

# Merge training and testing for analyzing predictors
df1 <- train_1 %>%   select(vars_continuous) %>%  mutate(Data = "Training")
df2 <- test_1 %>%   select(vars_continuous) %>%  mutate(Data = "Testing")
df <- rbind(df1, df2) %>% melt(id.vars = "Data") %>% na.omit()

# High level summary of all predictors
summary(rbind(df1, df2))

# Box plots for Continuous Predictor Variables 
ggplot(df, aes(x = variable, y = value, color = Data)) +
  xlab("Variable") +
  ylab("Count") +
  ggtitle("Box Plots for Continuous Predictor Variables") +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_blank())

# Histograms for Continuous Predictor Variables
ggplot(df, aes(value, fill = Data)) +
  xlab("Variable Value") +
  ylab("Count") +
  ggtitle("Histograms for Continuous Predictor Variables") +
  geom_histogram(bins = 30, color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))


# Training predictor correlation for continuous variables
corr_input <- train_1 %>%  select(vars_continuous, TARGET_FLAG, TARGET_AMT) %>%   na.omit()

corr <- cor(corr_input)
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


# Low-cardinality categorical variables that happen to be numeric
vars_categorical1 <- c("KIDSDRIV", "HOMEKIDS", "CLM_FREQ", "MVR_PTS",
                       "PARENT1_IND", "MARRIED_IND", "MALE_IND", "COMM_USE_IND",
                       "RED_CAR_IND", "REVOKED_IND", "RURAL_IND")

# Histograms for training and testing data: Categorical Group 1
df1 <- train_1 %>%
  select(vars_categorical1) %>%
  mutate(Data = "Training")

df2 <- test_1 %>%
  select(vars_categorical1) %>%
  mutate(Data = "Testing")

df <- rbind(df1, df2) %>%
  melt(id.vars = "Data") %>%
  na.omit()

ggplot(df, aes(value, fill = Data)) +
  xlab("Variable Value") +
  ylab("Count") +
  ggtitle("Categorical Predictor Variables") +
  geom_bar(color = "black") +
  facet_wrap(~variable, scales = "free") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

# Non-numeric high-cardinality categorical variables
vars_categorical2 <- c("JOB", "EDUCATION", "CAR_TYPE")

# Histograms for training and testing data: Categorical Group 2
df1 <- train_1 %>%
  select(vars_categorical2) %>%
  mutate(Data = "Training")

df2 <- test_1 %>%
  select(vars_categorical2) %>%
  mutate(Data = "Testing")

df <- rbind(df1, df2) %>%
  melt(id.vars = "Data") %>%
  na.omit()

p1 <- ggplot(df[df$variable == "JOB", ], aes(value, fill = Data)) +
  xlab("Job") +
  ylab("Count") +
  ggtitle("Categorical Predictor Variables") +
  geom_bar(color = "black") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6))

p2 <- ggplot(df[df$variable == "EDUCATION", ], aes(value, fill = Data)) +
  xlab("Education") +
  ylab("Count") +
  geom_bar(color = "black") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  theme(legend.position = "none")

p3 <- ggplot(df[df$variable == "CAR_TYPE", ], aes(value, fill = Data)) +
  xlab("Car Type") +
  ylab("Count") +
  geom_bar(color = "black") +
  theme(strip.text.x = element_text(size = 7),
        axis.text.x = element_text(size = 6),
        axis.text.y = element_text(size = 6)) +
  theme(legend.position = "none")

grid.arrange(grobs = list(p1, p2, p3), layout_matrix = rbind(c(1, 1), c(2, 3)))



# Visualize missing values in the data
MissingCount <- train_1 %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>% filter(value > 0) %>% rename("Missing_Value" = value)

MissingFraction <- train_1 %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
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

MissingCountT <- test_1 %>% select(-TARGET_FLAG,-TARGET_AMT) %>%  mutate_all(is.na) %>% summarise_all(sum) %>%
  melt() %>% filter(value > 0) %>% rename("Missing_Value" = value)

MissingFractionT <- test_1 %>% select(-TARGET_FLAG,-TARGET_AMT) %>% mutate_all(is.na) %>% summarise_all(sum) %>%
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

################################################################
# Part 2: Data Preparation
# Impute missing with average
# Create missing indicators (except for age - low sample size)
# Floor car age to 0
# Create zero indicators for continuous variables that can have meaning
# Cap travel time at 65, MVR points at 7, and YOJ at 15
# Cap income, home value, and bluebook value at 98th percentile


train_2 <- train_1 %>%
  mutate(AGE_IMPUTE = ifelse(is.na(AGE), mean(train_1$AGE, na.rm = TRUE), AGE),
         YOJ_IMPUTE = ifelse(is.na(YOJ), na.aggregate(train_1$YOJ, train_1$JOB, mean, na.rm = TRUE), YOJ),
         CAR_AGE_IMPUTE = ifelse(is.na(CAR_AGE), na.aggregate(train_1$CAR_AGE, train_1$CAR_TYPE, mean, na.rm = TRUE), CAR_AGE),
         HOME_VAL_IMPUTE = ifelse(is.na(HOME_VAL_NUM),na.aggregate(train_1$HOME_VAL_NUM, train_1$JOB, mean, na.rm = TRUE ), HOME_VAL_NUM),
         INCOME_IMPUTE = ifelse(is.na(INCOME_NUM),na.aggregate(train_1$INCOME_NUM, train_1$JOB, mean, na.rm = TRUE),INCOME_NUM),
         
         YOJ_MISS_IND = ifelse(is.na(YOJ), 1, 0),
         CAR_AGE_MISS_IND = ifelse(is.na(CAR_AGE), 1, 0),
         HOME_VAL_MISS_IND = ifelse(is.na(HOME_VAL_NUM), 1, 0),
         INCOME_MISS_IND = ifelse(is.na(INCOME_NUM), 1, 0),
         
         CAR_AGE_FLR = ifelse(CAR_AGE_IMPUTE < 0, 0, CAR_AGE_IMPUTE),
         
         HOME_VAL_ZERO_IND = ifelse(HOME_VAL_IMPUTE == 0, 1, 0),
         INCOME_ZERO_IND = ifelse(INCOME_IMPUTE == 0, 1, 0),
         YOJ_CAP = ifelse(YOJ_IMPUTE > 15, 15, YOJ_IMPUTE),
         TRAVTIME_CAP = ifelse(TRAVTIME > 65, 65, TRAVTIME),
         MVR_PTS_CAP = ifelse(MVR_PTS > 7, 7, MVR_PTS),
         INCOME_CAP = ifelse(INCOME_IMPUTE > quantile(INCOME_NUM, 0.98,
                                                      na.rm = TRUE),
                             quantile(INCOME_NUM, 0.98, na.rm = TRUE),
                             INCOME_IMPUTE),
         HOME_VAL_CAP = ifelse(HOME_VAL_IMPUTE > quantile(HOME_VAL_NUM, 0.98,
                                                          na.rm = TRUE),
                               quantile(HOME_VAL_NUM, 0.98, na.rm = TRUE),
                               HOME_VAL_IMPUTE),
         BLUEBOOK_CLOG = log(ifelse(BLUEBOOK_NUM > quantile(BLUEBOOK_NUM, 0.98,
                                                            na.rm = TRUE),
                                    quantile(BLUEBOOK_NUM, 0.98, na.rm = TRUE),
                                    BLUEBOOK_NUM)),
         OLDCLAIM_CLOG = log1p(ifelse(OLDCLAIM_NUM > quantile(OLDCLAIM_NUM,
                                                              0.98,
                                                              na.rm = TRUE),
                                      quantile(OLDCLAIM_NUM, 0.98,
                                               na.rm = TRUE),
                                      OLDCLAIM_NUM)),
         AGE_BIN = ntile(AGE_IMPUTE, 10),
         KIDSDRIV_IND = ifelse(KIDSDRIV > 1, 1, KIDSDRIV),
         HOMEKIDS_IND = ifelse(HOMEKIDS > 1, 1, HOMEKIDS),
         CLM_FREQ_IND = ifelse(CLM_FREQ > 1, 1, CLM_FREQ),
         CAR_TYPE_GRP = ifelse(as.character(CAR_TYPE) == "Panel Truck",
                               "PT_Van",
                               ifelse(as.character(CAR_TYPE) == "Van",
                                      "PT_Van",
                                      as.character(CAR_TYPE)))) %>%
  select(-AGE, -YOJ, -CAR_AGE, -HOME_VAL_NUM, -INCOME_NUM, -CAR_AGE_IMPUTE,
         -TRAVTIME, -INCOME_IMPUTE, -HOME_VAL_IMPUTE, -BLUEBOOK_NUM,
         -KIDSDRIV, -HOMEKIDS, -CLM_FREQ, -CAR_TYPE, -MVR_PTS, -YOJ_IMPUTE,
         -OLDCLAIM_NUM)

# Create factor version of the binary target for use with caret package
train_2$TARGET_FACTOR <- factor(ifelse(train_2$TARGET_FLAG == 1,
                                              "Claim", "No_Claim"),
                                       levels = c("No_Claim", "Claim"))

summary(train_2)

vars_continuous <- c("YOJ_CAP", "TRAVTIME_CAP", "TIF", "MVR_PTS_CAP",
                     "CAR_AGE_FLR", "OLDCLAIM_CLOG", "BLUEBOOK_CLOG",
                     "HOME_VAL_CAP", "INCOME_CAP")

vars_categorical1 <- c("KIDSDRIV_IND", "HOMEKIDS_IND", "CLM_FREQ_IND",
                       "PARENT1_IND", "MARRIED_IND", "MALE_IND", "COMM_USE_IND",
                       "REVOKED_IND", "RURAL_IND", "YOJ_MISS_IND",
                       "CAR_AGE_MISS_IND", "HOME_VAL_MISS_IND")
vars_categorical2 <- c("INCOME_MISS_IND",  "HOME_VAL_ZERO_IND", "YOJ_MISS_IND",
                       "CAR_AGE_MISS_IND", "HOME_VAL_MISS_IND")

# Training predictor correlation 
corr_input <- train_2 %>%  select(-TARGET_FACTOR, -EDUCATION, -JOB, -CAR_TYPE_GRP, -AGE_IMPUTE) %>%  na.omit()

corr <- cor(corr_input)
corrplot(corr, method = "square")

corr_melt <- melt(round(corr, 2))
ggplot(data = corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5) +
  scale_fill_gradient2(low = pal[3], high = pal[4], mid = "white", midpoint = 0,
                       limit = c(-1, 1), space = "Lab", name = "Correlation") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        axis.text.y = element_text(size = 8),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 8),
        legend.key.size = unit(.75, "line")) +
  xlab("") +
  ylab("") +
  ggtitle("Correlation - Training Data with Transformations")


# Part 3: Build Models

metric_printout <- function(model_object) {
  par(mfrow = c(1, 1))
  
  # Print final model summary
  print(summary(model_object))
  
  # Cross-Validation (Resampled) Metrics
  # ROCR requires numeric labels
  cv_scores <- model_object$pred %>%
    mutate(actual = ifelse(obs == "Claim", 1, 0))
  
  # KS statistic
  pred <- prediction(model_object$pred$Claim, cv_scores$actual)
  perf <- performance(pred, "tpr", "fpr")
  ks <- max(perf@y.values[[1]] - perf@x.values[[1]])
  
  # ROC curve
  plot(perf, main = "ROC Curve (Resampled)")
  lines(x = c(0, 1), y = c(0, 1), lty = 2)
  
  # AUC
  auc <- performance(pred, measure = "auc")@y.values[[1]]
  
  # Training Data Metrics
  # Format for ROCR
  scores <- data.frame(model_object$finalModel$fitted.values) # nolint
  
  names(scores) <- c("Claim")
  
  scores <- scores %>%
    cbind(., model_object$finalModel$data) %>% # nolint
    rename(obs = .outcome) %>%
    mutate(actual = ifelse(obs == "Claim", 1, 0))
  
  # KS statistic
  pred <- prediction(scores$Claim, scores$actual)
  perf <- performance(pred, "tpr", "fpr")
  kst <- max(perf@y.values[[1]] - perf@x.values[[1]])
  
  # ROC curve
  plot(perf, main = "ROC Curve (Training)")
  lines(x = c(0, 1), y = c(0, 1), lty = 2)
  
  # AUC
  auct <- performance(pred, measure = "auc")@y.values[[1]]
  

  # Print Metrics
  print(paste0("AUC (Resampled): ", auc))
  print(paste0("AUC (Training): ", auct))
  print(paste0("KS Stat (Resampled): ", ks))
  print(paste0("KS Stat (Training): ", kst))
}

# Models will be assessed based on five-fold cross validation
set.seed(827)
fit_control <- trainControl(method = "cv", number = 5,
                            savePredictions = TRUE, classProbs = TRUE,
                            summaryFunction = twoClassSummary)

# Models 1-4: Binary Target
# Model 1: Simple Model (Prior Claim or Not)
glm_fit1 <- train(TARGET_FACTOR ~ CLM_FREQ_IND, data = train_2,
                  method = "glm", family = binomial(link = "logit"),
                  trControl = fit_control, metric = "ROC")

metric_printout(glm_fit1)

# Models 1-4: Binary Target
# Model 1: Simple Model (Prior Claim or Not)
glm_fit1 <- train(TARGET_FACTOR ~ CLM_FREQ_IND, data = train_2,
                  method = "glm", family = binomial(link = "logit"),
                  trControl = fit_control, metric = "ROC")

metric_printout(glm_fit1)

str(train_2)
# Model 2: Forwards Selection
glm_fit2 <- train(TARGET_FACTOR ~ TIF + PARENT1_IND +
                    MARRIED_IND + MALE_IND + COMM_USE_IND + REVOKED_IND +
                    RURAL_IND + AGE_IMPUTE + YOJ_MISS_IND + CAR_AGE_MISS_IND +
                    HOME_VAL_MISS_IND + INCOME_MISS_IND + CAR_AGE_FLR +
                    HOME_VAL_ZERO_IND + INCOME_ZERO_IND + YOJ_CAP +
                    TRAVTIME_CAP + MVR_PTS_CAP + INCOME_CAP + HOME_VAL_CAP +
                    BLUEBOOK_CLOG + OLDCLAIM_CLOG +  KIDSDRIV_IND + HOMEKIDS_IND +
                    CLM_FREQ_IND ,
                  data = train_2,
                  method = "glmStepAIC",
                  direction = "forward",
                  trace = 0,
                  family = binomial(link = "logit"),
                  trControl = fit_control,
                  metric = "ROC")

metric_printout(glm_fit2)
vif(glm_fit2$finalModel) # nolint

# Model 3: Bonus Model PROBIT MODEL, Model 2 but Probit
glm_fit3 <- train(TARGET_FACTOR ~ TIF + PARENT1_IND +
                    MARRIED_IND + MALE_IND + COMM_USE_IND + REVOKED_IND +
                    RURAL_IND + AGE_IMPUTE + YOJ_MISS_IND + CAR_AGE_MISS_IND +
                    HOME_VAL_MISS_IND + INCOME_MISS_IND + CAR_AGE_FLR +
                    HOME_VAL_ZERO_IND + INCOME_ZERO_IND + YOJ_CAP +
                    TRAVTIME_CAP + MVR_PTS_CAP + INCOME_CAP + HOME_VAL_CAP +
                    BLUEBOOK_CLOG + OLDCLAIM_CLOG +  KIDSDRIV_IND + HOMEKIDS_IND +
                    CLM_FREQ_IND ,
                  data = train_2,
                  method = "glmStepAIC",
                  direction = "forward",
                  trace = 0,
                  family = binomial(link = "probit"),
                  trControl = fit_control,
                  metric = "ROC")

metric_printout(glm_fit3)

# Model 4: Bonus Model using "lrm" method from rms R package
library(rms)
glm_fit4 <- lrm(TARGET_FACTOR ~ TIF + PARENT1_IND +
                  MARRIED_IND + MALE_IND + COMM_USE_IND + REVOKED_IND +
                  RURAL_IND + AGE_IMPUTE + YOJ_MISS_IND + CAR_AGE_MISS_IND +
                  HOME_VAL_MISS_IND + INCOME_MISS_IND + CAR_AGE_FLR +
                  HOME_VAL_ZERO_IND + INCOME_ZERO_IND + YOJ_CAP +
                  TRAVTIME_CAP + MVR_PTS_CAP + INCOME_CAP + HOME_VAL_CAP +
                  OLDCLAIM_CLOG +  KIDSDRIV_IND + HOMEKIDS_IND ,
                  data = train_2)

glm_fit4
AIC(glm_fit4)

# Model 2c: same as model 2 but without OLDCLAIM_CLOG 
glm_fit2c <- train(TARGET_FACTOR ~ TIF + PARENT1_IND +
                    MARRIED_IND + MALE_IND + COMM_USE_IND + REVOKED_IND +
                    RURAL_IND + AGE_IMPUTE + YOJ_MISS_IND + CAR_AGE_MISS_IND +
                    HOME_VAL_MISS_IND + INCOME_MISS_IND + CAR_AGE_FLR +
                    HOME_VAL_ZERO_IND + INCOME_ZERO_IND + YOJ_CAP +
                    TRAVTIME_CAP + MVR_PTS_CAP + INCOME_CAP + HOME_VAL_CAP +
                    BLUEBOOK_CLOG + KIDSDRIV_IND + HOMEKIDS_IND +
                    CLM_FREQ_IND ,
                  data = train_2,
                  method = "glmStepAIC",
                  direction = "forward",
                  trace = 0,
                  family = binomial(link = "logit"),
                  trControl = fit_control,
                  metric = "ROC")

metric_printout(glm_fit2c)

AIC(glm_fit1$finalModel)
BIC(glm_fit1$finalModel)
AIC(glm_fit2$finalModel)
BIC(glm_fit2$finalModel)
AIC(glm_fit3$finalModel)
BIC(glm_fit3$finalModel)
AIC(glm_fit4)
BIC(glm_fit4)
AIC(glm_fit2c$finalModel)
BIC(glm_fit2c$finalModel)

print(-2*logLik(glm_fit1$finalModel, REML = TRUE))
print(-2*logLik(glm_fit2$finalModel, REML = TRUE))
print(-2*logLik(glm_fit3$finalModel, REML = TRUE))
print(-2*logLik(glm_fit4, REML = TRUE))
print(-2*logLik(glm_fit2c$finalModel, REML = TRUE))

predict(glm_fit1$finalModel, type = "response")

library(InformationValue)

ks_stat(actuals=train_2$TARGET_FLAG, predictedScores=predict(glm_fit1$finalModel, type = "response"))
ks_stat(actuals=train_2$TARGET_FLAG, predictedScores=predict(glm_fit2$finalModel, type = "response"))
ks_stat(actuals=train_2$TARGET_FLAG, predictedScores=predict(glm_fit3$finalModel, type = "response"))
ks_stat(actuals=train_2$TARGET_FLAG, predictedScores=predict(glm_fit4))
ks_stat(actuals=train_2$TARGET_FLAG, predictedScores=predict(glm_fit2c$finalModel, type = "response"))


# Change control function for Traget Amt models
fit_control <- trainControl(method = "cv", number = 5,
                            savePredictions = TRUE)

# Model 5: OLS Regression for Continuous Target
glm_fit5 <- train(TARGET_AMT ~ TIF + OLDCLAIM_CLOG + PARENT1_IND +
                    MARRIED_IND + MALE_IND + COMM_USE_IND + REVOKED_IND +
                    RURAL_IND + YOJ_MISS_IND + CAR_AGE_MISS_IND +
                    HOME_VAL_MISS_IND + INCOME_MISS_IND + CAR_AGE_FLR +
                    HOME_VAL_ZERO_IND + INCOME_ZERO_IND + YOJ_CAP +
                    TRAVTIME_CAP + MVR_PTS_CAP + INCOME_CAP + HOME_VAL_CAP +
                    BLUEBOOK_CLOG +  KIDSDRIV_IND + HOMEKIDS_IND +
                    CLM_FREQ_IND + AGE_IMPUTE + EDUCATION + JOB + CAR_TYPE_GRP +
                    AGE_IMPUTE ^ 2,
                  data = train_2[train_2$TARGET_AMT > 0, ],
                  method = "glmStepAIC",
                  direction = "backward",
                  trace = 0,
                  family = gaussian(link = "identity"),
                  trControl = fit_control)

glm_fit5
summary(glm_fit5)
par(mfrow = c(2, 2))
plot(glm_fit5$finalModel) 
vif(glm_fit5$finalModel) 


# Model 2 Coefficients for 
glm_fit2$finalModel$coefficients 

# Model 5 Coefficients
glm_fit5$finalModel$formula
glm_fit5$finalModel$coefficients 

#########################################################################
# Part 4: Stand Alone Scoring Program

library(dplyr)

# Load training and testing data
train <- read.csv("logit_insurance.csv")
test <- read.csv("logit_insurance_test.csv")

# Some initial formating and indicator creation since we have a mix of numerical and categorical
# Train Data
train_1 <- train %>%
  mutate(PARENT1_IND = ifelse(PARENT1 == "Yes", 1, 0),
         MARRIED_IND = ifelse(MSTATUS == "z_No", 0, 1),
         MALE_IND = ifelse(SEX == "z_F", 0, 1),
         COMM_USE_IND = ifelse(CAR_USE == "Commercial", 1, 0),
         RED_CAR_IND = ifelse(RED_CAR == "yes", 1, 0),
         REVOKED_IND = ifelse(REVOKED == "Yes", 1, 0),
         RURAL_IND = ifelse(URBANICITY == "z_Highly Rural/ Rural", 1, 0),
         JOB = as.factor(ifelse(JOB == "", "Other", as.character(JOB))),
         OLDCLAIM_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", OLDCLAIM))),
         BLUEBOOK_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", BLUEBOOK))),
         INCOME_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", INCOME))),
         HOME_VAL_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", HOME_VAL)))) %>%
  dplyr::select(-OLDCLAIM, -BLUEBOOK, -HOME_VAL, -INCOME, -PARENT1, -MSTATUS,
                -SEX, -CAR_USE, -RED_CAR, -REVOKED, -URBANICITY, -INDEX)
# Test Data
test_1 <- test %>%
  mutate(PARENT1_IND = ifelse(PARENT1 == "Yes", 1, 0),
         MARRIED_IND = ifelse(MSTATUS == "z_No", 0, 1),
         MALE_IND = ifelse(SEX == "z_F", 0, 1),
         COMM_USE_IND = ifelse(CAR_USE == "Commercial", 1, 0),
         RED_CAR_IND = ifelse(RED_CAR == "yes", 1, 0),
         REVOKED_IND = ifelse(REVOKED == "Yes", 1, 0),
         RURAL_IND = ifelse(URBANICITY == "z_Highly Rural/ Rural", 1, 0),
         JOB = as.factor(ifelse(JOB == "", "Other", as.character(JOB))),
         OLDCLAIM_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", OLDCLAIM))),
         BLUEBOOK_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", BLUEBOOK))),
         INCOME_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", INCOME))),
         HOME_VAL_NUM = suppressWarnings(as.numeric(gsub("[^0-9\\.]", "", HOME_VAL)))) %>%
  dplyr::select(-OLDCLAIM, -BLUEBOOK, -HOME_VAL, -INCOME, -PARENT1, -MSTATUS,
                -SEX, -CAR_USE, -RED_CAR, -REVOKED, -URBANICITY)


# Data Preparation

train_2 <- train_1 %>%
  mutate(AGE_IMPUTE = ifelse(is.na(AGE), mean(train_1$AGE, na.rm = TRUE), AGE),
         YOJ_IMPUTE = ifelse(is.na(YOJ), na.aggregate(train_1$YOJ, train_1$JOB, mean, na.rm = TRUE), YOJ),
         CAR_AGE_IMPUTE = ifelse(is.na(CAR_AGE), na.aggregate(train_1$CAR_AGE, train_1$CAR_TYPE, mean, na.rm = TRUE), CAR_AGE),
         HOME_VAL_IMPUTE = ifelse(is.na(HOME_VAL_NUM),na.aggregate(train_1$HOME_VAL_NUM, train_1$JOB, mean, na.rm = TRUE ), HOME_VAL_NUM),
         INCOME_IMPUTE = ifelse(is.na(INCOME_NUM),na.aggregate(train_1$INCOME_NUM, train_1$JOB, mean, na.rm = TRUE),INCOME_NUM),
         
         YOJ_MISS_IND = ifelse(is.na(YOJ), 1, 0),
         CAR_AGE_MISS_IND = ifelse(is.na(CAR_AGE), 1, 0),
         HOME_VAL_MISS_IND = ifelse(is.na(HOME_VAL_NUM), 1, 0),
         INCOME_MISS_IND = ifelse(is.na(INCOME_NUM), 1, 0),
         
         CAR_AGE_FLR = ifelse(CAR_AGE_IMPUTE < 0, 0, CAR_AGE_IMPUTE),
         
         HOME_VAL_ZERO_IND = ifelse(HOME_VAL_IMPUTE == 0, 1, 0),
         INCOME_ZERO_IND = ifelse(INCOME_IMPUTE == 0, 1, 0),
         YOJ_CAP = ifelse(YOJ_IMPUTE > 15, 15, YOJ_IMPUTE),
         TRAVTIME_CAP = ifelse(TRAVTIME > 65, 65, TRAVTIME),
         MVR_PTS_CAP = ifelse(MVR_PTS > 7, 7, MVR_PTS),
         INCOME_CAP = ifelse(INCOME_IMPUTE > quantile(INCOME_NUM, 0.98,
                                                      na.rm = TRUE),
                             quantile(INCOME_NUM, 0.98, na.rm = TRUE),
                             INCOME_IMPUTE),
         HOME_VAL_CAP = ifelse(HOME_VAL_IMPUTE > quantile(HOME_VAL_NUM, 0.98,
                                                          na.rm = TRUE),
                               quantile(HOME_VAL_NUM, 0.98, na.rm = TRUE),
                               HOME_VAL_IMPUTE),
         BLUEBOOK_CLOG = log(ifelse(BLUEBOOK_NUM > quantile(BLUEBOOK_NUM, 0.98,
                                                            na.rm = TRUE),
                                    quantile(BLUEBOOK_NUM, 0.98, na.rm = TRUE),
                                    BLUEBOOK_NUM)),
         OLDCLAIM_CLOG = log1p(ifelse(OLDCLAIM_NUM > quantile(OLDCLAIM_NUM,
                                                              0.98,
                                                              na.rm = TRUE),
                                      quantile(OLDCLAIM_NUM, 0.98,
                                               na.rm = TRUE),
                                      OLDCLAIM_NUM)),
         AGE_BIN = ntile(AGE_IMPUTE, 10),
         KIDSDRIV_IND = ifelse(KIDSDRIV > 1, 1, KIDSDRIV),
         HOMEKIDS_IND = ifelse(HOMEKIDS > 1, 1, HOMEKIDS),
         CLM_FREQ_IND = ifelse(CLM_FREQ > 1, 1, CLM_FREQ),
         CAR_TYPE_GRP = ifelse(as.character(CAR_TYPE) == "Panel Truck",
                               "PT_Van",
                               ifelse(as.character(CAR_TYPE) == "Van",
                                      "PT_Van",
                                      as.character(CAR_TYPE)))) %>%
  select(-AGE, -YOJ, -CAR_AGE, -HOME_VAL_NUM, -INCOME_NUM, -CAR_AGE_IMPUTE,
         -TRAVTIME, -INCOME_IMPUTE, -HOME_VAL_IMPUTE, -BLUEBOOK_NUM,
         -KIDSDRIV, -HOMEKIDS, -CLM_FREQ, -CAR_TYPE, -MVR_PTS, -YOJ_IMPUTE,
         -OLDCLAIM_NUM)


test_2 <- test_1 %>%
  mutate(AGE_IMPUTE = ifelse(is.na(AGE), mean(train_1$AGE, na.rm = TRUE), AGE),
         YOJ_IMPUTE = ifelse(is.na(YOJ), na.aggregate(train_1$YOJ, train_1$JOB, mean, na.rm = TRUE), YOJ),
         CAR_AGE_IMPUTE = ifelse(is.na(CAR_AGE), na.aggregate(train_1$CAR_AGE, train_1$CAR_TYPE, mean, na.rm = TRUE), CAR_AGE),
         HOME_VAL_IMPUTE = ifelse(is.na(HOME_VAL_NUM),na.aggregate(train_1$HOME_VAL_NUM, train_1$JOB, mean, na.rm = TRUE ), HOME_VAL_NUM),
         INCOME_IMPUTE = ifelse(is.na(INCOME_NUM),na.aggregate(train_1$INCOME_NUM, train_1$JOB, mean, na.rm = TRUE),INCOME_NUM),
         
         YOJ_MISS_IND = ifelse(is.na(YOJ), 1, 0),
         CAR_AGE_MISS_IND = ifelse(is.na(CAR_AGE), 1, 0),
         HOME_VAL_MISS_IND = ifelse(is.na(HOME_VAL_NUM), 1, 0),
         INCOME_MISS_IND = ifelse(is.na(INCOME_NUM), 1, 0),
         
         CAR_AGE_FLR = ifelse(CAR_AGE_IMPUTE < 0, 0, CAR_AGE_IMPUTE),
         
         HOME_VAL_ZERO_IND = ifelse(HOME_VAL_IMPUTE == 0, 1, 0),
         INCOME_ZERO_IND = ifelse(INCOME_IMPUTE == 0, 1, 0),
         YOJ_CAP = ifelse(YOJ_IMPUTE > 15, 15, YOJ_IMPUTE),
         TRAVTIME_CAP = ifelse(TRAVTIME > 65, 65, TRAVTIME),
         MVR_PTS_CAP = ifelse(MVR_PTS > 7, 7, MVR_PTS),
         INCOME_CAP = ifelse(INCOME_IMPUTE > quantile(INCOME_NUM, 0.98,
                                                      na.rm = TRUE),
                             quantile(INCOME_NUM, 0.98, na.rm = TRUE),
                             INCOME_IMPUTE),
         HOME_VAL_CAP = ifelse(HOME_VAL_IMPUTE > quantile(HOME_VAL_NUM, 0.98,
                                                          na.rm = TRUE),
                               quantile(HOME_VAL_NUM, 0.98, na.rm = TRUE),
                               HOME_VAL_IMPUTE),
         BLUEBOOK_CLOG = log(ifelse(BLUEBOOK_NUM > quantile(BLUEBOOK_NUM, 0.98,
                                                            na.rm = TRUE),
                                    quantile(BLUEBOOK_NUM, 0.98, na.rm = TRUE),
                                    BLUEBOOK_NUM)),
         OLDCLAIM_CLOG = log1p(ifelse(OLDCLAIM_NUM > quantile(OLDCLAIM_NUM,
                                                              0.98,
                                                              na.rm = TRUE),
                                      quantile(OLDCLAIM_NUM, 0.98,
                                               na.rm = TRUE),
                                      OLDCLAIM_NUM)),
         AGE_BIN = ntile(AGE_IMPUTE, 10),
         KIDSDRIV_IND = ifelse(KIDSDRIV > 1, 1, KIDSDRIV),
         HOMEKIDS_IND = ifelse(HOMEKIDS > 1, 1, HOMEKIDS),
         CLM_FREQ_IND = ifelse(CLM_FREQ > 1, 1, CLM_FREQ),
         CAR_TYPE_GRP = ifelse(as.character(CAR_TYPE) == "Panel Truck",
                               "PT_Van",
                               ifelse(as.character(CAR_TYPE) == "Van",
                                      "PT_Van",
                                      as.character(CAR_TYPE)))) %>%
  select(-AGE, -YOJ, -CAR_AGE, -HOME_VAL_NUM, -INCOME_NUM, -CAR_AGE_IMPUTE,
         -TRAVTIME, -INCOME_IMPUTE, -HOME_VAL_IMPUTE, -BLUEBOOK_NUM,
         -KIDSDRIV, -HOMEKIDS, -CLM_FREQ, -CAR_TYPE, -MVR_PTS, -YOJ_IMPUTE,
         -OLDCLAIM_NUM)


# Scored Data File

scores <- test_2 %>%
  mutate(P_TARGET_FLAG_1 = 2.749264 
         - 2.252701 * RURAL_IND 
         - 0.000001138021 * HOME_VAL_CAP
         + 0.1030372 * MVR_PTS_CAP
         + 0.9464012 * COMM_USE_IND
         - 0.3950574 * BLUEBOOK_CLOG
         + 0.8738710 * REVOKED_IND
         + 0.2473310 * HOMEKIDS_IND
         + 0.01621714 * TRAVTIME_CAP 
         - 0.05285844 * TIF 
         + 1.855246 * CLM_FREQ_IND 
         - 0.000006211482 * INCOME_CAP 
         - 0.5613587 * MARRIED_IND 
         + 0.5632398 * KIDSDRIV_IND 
         - 0.2516945 * MALE_IND 
         - 0.02293248 * CAR_AGE_FLR 
         - 0.1618346 * OLDCLAIM_CLOG 
         + 0.2890876 * INCOME_ZERO_IND 
         + 0.2038418 * PARENT1_IND,
         P_TARGET_FLAG = exp(P_TARGET_FLAG_1) / (1 + exp(P_TARGET_FLAG_1))) %>%
  mutate(HighSchool_IND = ifelse(EDUCATION == "z_High School", 1, 0),
         PhD_IND = ifelse(EDUCATION == "PhD", 1, 0),
         Doctor_IND = ifelse(JOB == "Doctor", 1, 0),
         Manager_IND = ifelse(JOB == "Manager", 1, 0),
         P_TARGET_AMT = P_TARGET_FLAG * ( - 6579.28293
         - 897.49565 * MARRIED_IND 
         + 589.44890 * MALE_IND 
         - 697.04293 * REVOKED_IND 
         - 71.56917 * CAR_AGE_FLR 
         - 676.67639 *  HOME_VAL_ZERO_IND 
         + 137.39614 * MVR_PTS_CAP 
         + 1418.87337 * BLUEBOOK_CLOG 
         + 1372.14895 * PhD_IND 
         - 551.62170 * HighSchool_IND 
         - 2471.08475 * Doctor_IND 
         - -1155.45783 * Manager_IND)) %>%
  select(INDEX, P_TARGET_FLAG, P_TARGET_AMT)

# Save output
write.csv(scores, "PrabhatThakur_Unit2_Assignment_test_scores.csv", row.names = FALSE)