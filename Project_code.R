install.packages(c("dplyr", "stringr", "ggplot2", "tidyr","lmtest","sf","car"))

library(dplyr)
library(stringr)
library(ggplot2)
library(tidyr)
library(lmtest)
library(sf)
library(car)

Sys.setlocale("LC_ALL", "en_US.UTF-8")

data <- read.csv("data_final.csv",
                 sep = ";",
                 encoding = "UTF_8",
                 stringsAsFactors = FALSE)

# Correct the encoding problem of the data
replace_from <- c("¶","Ú","\\?","Þ", "þ", "Û", "Ó", "Cote", "Ó", "Ô", "Geroges")
replace_to <- c("ô","é","É","è", "ç", "ê", "à", "Côte", "à", "â", "Georges")
cols <- c("rue_1", "street_1", "street_2", "borough")
for (i in 1:length(replace_from)) {
  for (col in cols) {
    data[[col]] <- str_replace_all(data[[col]], replace_from[i], replace_to[i])
  }
}

# vizualize data on a map-----------------------------------------------



# Create a Simple Features (sf) object
data <- st_as_sf(data, coords = c("x", "y"), crs = 32198)

# Check the spatial object
print(data)


# Ensure `pi` and `fi` are numeric
data$pi <- as.numeric(data$pi)
data$fi <- as.numeric(data$fi)

# Create the plot with corrected data
ggplot(data) +
  geom_sf(aes(color = pi, size = fi)) +  # Add color for injuries and size for flows
  scale_color_gradient(low = "blue", high = "red", name = "Pedestrian Injuries") +  # Gradient legend for `pi`
  scale_size_continuous(name = "Pedestrian Flows", range = c(0.5, 4)) +  # Size legend for `fi`
  theme_minimal() +
  labs(
    title = "Spatial Data Visualization with Pedestrian Metrics",
    subtitle = "Color indicates injuries, and size indicates pedestrian flows",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme(
    legend.position = "right",       # Position of the legend
    plot.title = element_text(size = 14, face = "bold"),  # Bold title
    plot.subtitle = element_text(size = 10)  # Smaller subtitle
  )


#remove geometry data
data <- st_drop_geometry(data)
#---------------------------------------------------------------------
# Data  pre-processing
#---------------------------------------------------------------------

str(data)

unique(data$rue_1)%>%length() # 564
unique(data$rue_2)%>%length() # 662
unique(data$borough)%>%length() # 28
# There are too many unique valued for rue_1 and rue_2. If converted to dummies, it will
# create 564+662=1226 new columns.This leads to:
# 1. Curse of dimensionality - too many variables relative to the dataset size
# 2. Overfitting - the model memorize specific streets instead of learning general patterns
# We remove the street names and meaningless columns X and X.1 from the dataset

col_to_remove <- c("street_1", "street_2", "X", "X.1", "rue_1", "rue_2")
data <- data %>% dplyr::select(-col_to_remove)


is.na(data)%>%sum()
# there is 1 NA in ln_distdt. The corresponding distdt value is 0. So we
# can replace ln_distdt with 0.
data[is.na(data$ln_distdt),]$ln_distdt <- 0


# transfer date_ column type to Date and check if there are all dates are valid.
# select rows where date_ is NA
data$date_ <- as.Date(data$date_, format="%d/%m/%Y")
is.na(data$date_)%>%sum() #13
ind_bad_date <- is.na(data$date_)
data[ind_bad_date,]
# there are 13 rows with NA date.
# the missing dates are not showing any pattern and could be classified as
# missing completely at random. We will replace the NA values of $date_ with
#  median of the $date_ column.
data$date_[ind_bad_date] <- median(data$date_, na.rm=TRUE) # "2009-01-21"


# There are duplicated values in column int_no
data[data$int_no == data[duplicated(data$int_no)==TRUE,]$int_no,]
# there are 2 duplicated values in int_no 9151 which represents different
# intersections. We will assign a new intersection number to one of them.
# We will assign the number following to the max int_no plus 1 to one of the
# duplicates.
data[duplicated(data$int_no)==TRUE,]$int_no <- max(data$int_no)+1
duplicated(data$int_no)%>%sum()
# there are no duplicates in int_no anymore.

# Function to plot distribution of numeric variables
plot_num_pair <- function(df, var1, var2){
  df_long <- df %>% pivot_longer(cols= all_of(c(var1,var2)) ,
                                 names_to="variable", values_to="value")
  ggplot(df_long, aes(x=value, fill=variable))+
    geom_histogram(color="black", alpha=0.5, bins=30)+
    facet_wrap(~variable, scales="free_x")+
    theme_minimal()+
    labs(title=paste("Distribution of", var1, "and", var2),
         x="Value",
         y="Frequency")
}

# Let's explore continuous variables (original vs log transformed) #GOOD TO SHOW RELATIONSHIP WITHOTU FITTING
plot_num_pair(data, "traffic_10000", "ln_fi")


# ???? SAME AS BEFORE ??-------------------
# We explore the distribution of variables distdt,  and ln_distdt
plot_num_pair(data, "distdt", "ln_distdt")
# The original scale appears better for this model because ln transformation
# while reduce the skewness, it also concentrates most values in the right side
# of the distribution, potentially masknin importance of the lower values.
# To reduce the scale we can simply convert the variable to km.

data$distdt_1000 <- data$distdt/1000

plot_num_pair(data, "distdt_1000", "distdt")
# By converting the variable to a more interpretable scale (meters to kilometers),
# we make the coefficient easier to understand and interpret. The model result
# will be mathematically equivalent, but the km scale will be more practical
# for interpretation and presentation.

# --------------------------------------------------------------------
# Examine the categorical variables
# --------------------------------------------------------------------
table(data$commercial)
# There are 30 or less observation with 5, 6, 7, and 8 ent/exit to commercial properties.
# We decided to set up a threshold of 30 observations to combine these categories into one.
# Create new column where these categories combined into one category "5", which
# represents "from 5 to 8 ent/exit to commercial properties
data <- data %>% mutate(commercial_comb = case_when(
  commercial %in% c(5,6,7,8) ~ 5,
  TRUE ~ as.numeric(commercial)))
table(data$commercial_comb)

table(data$of_exclusi)
# There are only 25 observations with 4 exclusive left turn lanes. We will combine
# this category with the 3 exclusive left turn lanes category.
data <- data %>% mutate(of_exclusi_comb = case_when(
  of_exclusi == 4 ~ 3,
  TRUE ~ as.numeric(of_exclusi)))
table(data$of_exclusi_comb)

table(data$total_lane)
# The number of lanes of 0, 1 has small frequency, so we will combine them with
# category with lable 2 wich will be represent 2 and less lanes. Lable 9 and 10
# will be combined with 8 which will represent 8 and more lanes.
data <- data %>% mutate(total_lane_comb = case_when(
  total_lane %in% c(0,1) ~ 2,
  total_lane %in% c(9,10) ~ 8,
  TRUE ~ as.numeric(total_lane)))
table(data$total_lane_comb)

table(data$number_of_)
data <- data %>% mutate(number_of_comb = case_when(
  number_of_ == 1 ~ 2,
  TRUE ~ as.numeric(number_of_)))
table(data$number_of_comb)

# see distribution of response variable ---------------------

ggplot(data, aes(x = acc)) +
  geom_histogram(
    bins = 30,
    fill = "skyblue",
    alpha = 0.7,
    color = "black"
  ) +
  geom_density(
    aes(y = ..count..),
    color = "red",
    linewidth = 1.2 # Replace `size` with `linewidth`
  ) +
  theme_minimal() +
  labs(
    title = "Distribution of Pedestrian Accidents (acc)",
    x = "Number of Accidents",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    axis.title = element_text(size = 12)
  )


#applying log transform on resposne variable (DID NOT FIX NON NORMALITY) ------------------------------------------------
#
# # Transform the 'acc' variable into 'acc_ln'
# data <- data %>%
#   mutate(acc_ln = log(acc+1))
#
# # Plot the log-transformed variable
# ggplot(data, aes(x = acc_ln)) +
#   geom_histogram(
#     bins = 30,
#     fill = "skyblue",
#     alpha = 0.7,
#     color = "black"
#   ) +
#   geom_density(
#     aes(y = ..count..),
#     color = "red",
#     size = 1.2
#   ) +
#   theme_minimal() +
#   labs(
#     title = "Distribution of Pedestrian Accidents (Log Transformed)",
#     x = "Log(Number of Accidents)",
#     y = "Frequency"
#   ) +
#   theme(
#     plot.title = element_text(size = 14, face = "bold"),
#     axis.title = element_text(size = 12)
#   )
#



# Check Boroughs
unique(data$borough)%>% length()
# there are 28 unique boroughs
# Total accidents by borough
borough_acc <- data %>% group_by(borough) %>% summarise(acc=sum(acc))
# plot borough vs total accidents with horizontal line at 40.

# Unique borough count
borough_count <- data %>% group_by(borough) %>% summarise(count=n())
ggplot(borough_count, aes(x=borough, y=count))+
  geom_bar(stat="identity", fill="skyblue", color="black")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Frequency of Boroughs",
       x="Borough",
       y="Frequency")
summary(borough_count)
table(data$borough)

ggplot(borough_acc, aes(x=borough, y=acc))+
  geom_bar(stat="identity", fill="skyblue", color="black")+
  geom_hline(yintercept=40, color="red")+
  geom_hline(yintercept=200, color="red")+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title="Total Accidents by Borough",
       x="Borough",
       y="Total Accidents")
summary(borough_acc)

# categorize boroughs with less than 40 accidents as "low", more than
# 200 as "high", and between 40 and 200 as "medium". These thresholds are
# based on natural breaks in the data distribution, ensuring clear differentiation
# between categories, and provide a meaningful classification for the model.
borough_acc <- borough_acc %>%
  mutate(borough_comb = case_when(
    acc <= 40 ~ "low",
    acc > 200 ~ "high",
    TRUE ~ "medium"))
table(borough_acc$borough_comb)


# merge the data with borough_acc only borogh_cat
data <- merge(data, borough_acc[,c("borough", "borough_comb")], by="borough", all.x=TRUE)
str(data)

# Convert character variable borough_comb to dummy variables, as no assumption of equal
# spacing between categories is required.
matrix_dum <- model.matrix(~borough_comb-1, data=data)
data_dum <- cbind(data, matrix_dum)
str(data_dum)


#checking proportion n binary variables --------------------------------------------------------------
# 1. List all the binary variables you want to check:
binary_vars <- c(
  "median",
  "green_stra",
  "all_red_an",
  "any_ped_pr",
  "ped_countd",
  "lt_protect",
  "lt_restric",
  "new_half_r"
)

# 2. Loop through each variable, display counts and proportions:
for (var in binary_vars) {
  cat("\nVariable:", var, "\n")

  # Create a frequency table of 0/1 (plus any NA)
  tb <- table(data[[var]], useNA = "ifany")
  print(tb)

  # Show proportions (relative frequencies)
  prop_tb <- prop.table(tb)
  print(prop_tb)
}

# --------------------------------------------------------------------
# check for redundancy
# --------------------------------------------------------------------
data_col <- data_dum %>%
  mutate(sum_dir_ped = north_ped + east_ped + south_ped + west_ped,
         sum_dir_veh = north_veh + east_veh + south_veh + west_veh,
         sum_fi = fli + fri + fti)

sum(data_dum$pi)- sum(data_dum$north_ped + data_dum$east_ped + data_dum$south_ped + data_dum$west_ped)
# The difference of sum of pedestrian directions and pi is almost 0
# which suggest precence of multicollinearity between these variables.
# We will check the VIF values to confirm this.
mult_col <- vif(lm(acc ~ pi + north_ped + east_ped + south_ped + west_ped, data = data_col))
mult_col
# The VIF values are very high, indicating multicollinearity between these variables.

# similar situation with vehicle directions variables and fi
sum(data_col$fi)- sum(data_dum$north_veh + data_dum$east_veh + data_dum$south_veh + data_dum$west_veh)

sum(data_col$fi) - sum(data_dum$fli + data_dum$fri + data_dum$fti)


# remove redundant columns "ln_fli","ln_fri","ln_fti","north_veh",
# "north_ped","east_veh","east_ped","south_veh","south_ped","west_veh",
# "west_ped", "fli", "fri", "fti"

col_to_remove2 <- c("pi", "fi", "fli", "fri", "fti", "cli", "cri",
                    "cti", "tot_crossw","number_of_", "half_phase",
                    "any_exclus", "lt_prot_re", "total_lane",
                    "of_exclusi", "any_exclus", "commercial",
                    "distdt", "ln_distdt", "traffic_10000", "ped_100",
                    "borough", "borough_comb", "borough_comblow",
                    "all_red_an","ln_fli","ln_fri","ln_fti","north_veh",
                    "north_ped","east_veh","east_ped","south_veh","south_ped",
                    "west_veh","west_ped")


data <- data_dum %>% dplyr::select(-all_of(col_to_remove2))
summary(data)
# # before fit to a model, we need to set ln_pi and ln_fi as offset terms

ncol(data)
str(data)

#---------------------------------------------------------------------
# end of EDA
#---------------------------------------------------------------------



# --------------------------------------------------------------------
# Models
# --------------------------------------------------------------------

install.packages("glmnet")
install.packages("MASS")
install.packages("lmtest")
install.packages("randomForest")
install.packages("caret")
library(glmnet)
library(MASS)
library(lmtest)
library(dplyr)
library(randomForest)
library(caret)

# Data frame to store AIC and BIC results
AIC_BIC_res <- data.frame(Model=NULL, AIC=NULL, BIC=NULL, coef=NULL)
# Function to add AIC and BIC results to the data frame
add_to_res <- function(model, model_name)
{
  AIC_BIC_res <<- rbind(AIC_BIC_res, data.frame(Model=model_name,
                                                AIC=AIC(model),
                                                BIC=BIC(model),
                                                coef=length(model$coefficients)))
}

# --------------------------------------------------------------------
# OLS Full model
# --------------------------------------------------------------------
str(data)
variance_acc <- var(data$acc)
variance_acc
# Full model: All predictors
lm.fit <- lm(acc ~ ., data = data)
summary(lm.fit)
AIC(lm.fit)
BIC(lm.fit)

# AIC BIC result df
add_to_res(lm.fit, "LM Full")

#--------------------------checking normality  and homoscedasticity  --------------------
par(mfrow=c(2,2))
plot(lm.fit)
dev.off()
# # Step 1: Get residuals and fitted values
# residuals <- residuals(lm.fit)
# fitted_values <- fitted(lm.fit)
#
# # Step 2: Plot residuals vs fitted values (check for heteroscedasticity) THERE IS
# plot(fitted_values, residuals,
#      xlab = "Fitted Values", ylab = "Residuals",
#      main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red", lty = 2) # Add a horizontal line at 0
#
# # Step 3: Q-Q plot (check for normality of residuals)  NORMALITY "IS" VIOLATED"
# qqnorm(residuals)
# qqline(residuals, col = "red") # Add a reference line


# Step 5: Breusch-Pagan Test for Heteroscedasticity
bptest(lm.fit) # p-value < 0.05 suggests heteroscedasticity ( we get 2.2*-16 so yes)

#---------------------------------------------------------------------
# LM with offset term
#---------------------------------------------------------------------
# Remove ln_pi and ln_fi from regular predictors, add them as a combined offset:
lm.fit_offset <- lm(acc ~ .-ln_pi - ln_fi + offset(ln_pi + ln_fi),
                    data = data
)
summary(lm.fit_offset)
AIC(lm.fit_offset)
BIC(lm.fit_offset)

add_to_res(lm.fit_offset, "LM with Offset")


# --------------------------------------------------------------------
# Poisson
# --------------------------------------------------------------------
# The response variable is count data, we assume that it follows a Poisson distribution.
poisson.fit <- glm(acc ~ ., data = data, family = poisson)
summary(poisson.fit)
# calculate dispersion parameter
dispersion <- sum(residuals(poisson.fit, type = "pearson")^2) / poisson.fit$df.residual
dispersion
# dispersion is 2.75 which is greater than 1, indicating overdispersion
AIC(poisson.fit)
BIC(poisson.fit)

add_to_res(poisson.fit, "Poisson Full")

par(mfrow=c(2,2))
plot(poisson.fit)

AIC_BIC_res
# --------------------------------------------------------------------
# Negative binomial (to account for non-normality and dispersion)
# --------------------------------------------------------------------

glm.fit <- glm.nb(acc ~ ., data = data)
summary(glm.fit)
AIC(glm.fit)
BIC(glm.fit)

add_to_res(glm.fit, "NB Full")


# Negative binomial ( to account for non-normality and dispersion (+offset term) ---------------------
# Remove ln_pi and ln_fi from regular predictors, add them as a combined offset:
glm.fit_offset <- glm.nb(acc ~ . -ln_pi - ln_fi + offset(ln_pi + ln_fi),data = data)

summary(glm.fit_offset)
AIC(glm.fit_offset)
BIC(glm.fit_offset)

add_to_res(glm.fit_offset, "NB with Offset")

AIC_BIC_res

# redoing steps for normality and heterosdacity----------------------------------------

#--------------------------checking normality  and heterosdacity--------------------
par(mfrow=c(2,2))
plot(glm.fit, main = "Negative Binomial Model")
plot(glm.fit_offset, main = "Negative Binomial Model (with Offset)")
dev.off()
# # Step 1: Get residuals and fitted values
# residuals <- residuals(glm.fit)
# fitted_values <- fitted(glm.fit)
#
# # Step 2: Plot residuals vs fitted values (check for heteroscedasticity) THERE IS
# plot(fitted_values, residuals,
#      xlab = "Fitted Values", ylab = "Residuals",
#      main = "Residuals vs Fitted Values")
# abline(h = 0, col = "red", lty = 2) # Add a horizontal line at 0
#
# # Step 3: Q-Q plot (check for normality of residuals)  NORMALITY "IS" VIOLATED"
# qqnorm(residuals)
# qqline(residuals, col = "red") # Add a reference line


# Step 5: Breusch-Pagan Test for Heteroscedasticity

bptest(glm.fit) # p-value < 0.05 suggests heteroscedasticity ( we get 2.2*-16 so yes)
bptest(glm.fit_offset) # p-value < 0.05 suggests heteroscedasticity ( we get 2.2*-16 so yes)



#----------- STEPWISE AIC (BIC)---------------------------------------

# Step 4: Perform Stepwise Variable Selection using stepAIC
n <- nrow(data) # number of observations
stepwise_lm_BIC <- stepAIC(lm.fit, direction = "both", k=log(n))

# Step 5: Display the Summary of the Final Model
summary(stepwise_lm_BIC)
add_to_res(stepwise_lm_BIC, "StepBIC LM")


# Optional: Display the selected predictors
cat("Selected predictors:\n", names(coef(stepwise_lm_BIC)), "\n")

# The algorithm did not converge.
# step_glm.fit_BIC <- stepAIC(glm.fit, direction = "backward", k=log(n),
#                             maxit=10000)
# mult_col <- vif(glm.nb(acc ~ ., data = data))
# mult_col
# summary(step_glm.fit_BIC)
# add_to_res(step_glm.fit_BIC, "StepBIC NB")

# The algorithm did not converge.
#step_glm.fit_offset_BIC <- stepAIC(glm.fit_offset, direction = "both",
#                                   k=log(n), maxit = 500)
#summary(step_glm.fit_offset_BIC)
# the algorithm did not converge. That could happened due to multicolinearity,
#mult_col <- vif(glm.nb(acc ~ . -ln_pi - ln_fi + offset(ln_pi + ln_fi), data = data))
#mult_col
# There are no values above 10, so multicolinearity is not the issue.


#----------------------STEPWISE aic (AIC)-----------------

stepwise_lm_AIC <- stepAIC(lm.fit, direction = "both")

# Step 5: Display the Summary of the Final Model
summary(stepwise_lm_AIC)

# Optional: Display the selected predictors
cat("Selected predictors:\n", names(coef(stepwise_lm_AIC)), "\n")
add_to_res(stepwise_lm_AIC, "StepAIC LM")

# The algorithm did not converge.
# step_glm.fit_AIC <- stepAIC(glm.fit, direction = "both")
# summary(step_glm.fit_AIC)
# add_to_res(step_glm.fit_AIC, "StepAIC NB")

# not coverging
#step_glm.fit_offset_AIC <- stepAIC(glm.fit_offset, direction = "both")
# manually set offset term on stepwise AIC glm.nb model


sort_by(AIC_BIC_res, AIC_BIC_res$AIC)


# #-------------------LASSO ------------------------
#
# X <- model.matrix(acc ~ ., data = data)[, -1]
# y <- data$acc
#
# cv_lasso <- cv.glmnet(X, y, alpha = 1)
# plot(cv_lasso)
#
# best_lambda <- cv_lasso$lambda.min
# cat("Best lambda:", best_lambda, "\n")
#
#
# lasso_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
# print(lasso_model)
#
# coef_lasso <- coef(lasso_model)
# print(coef_lasso)
#
# lambda_1se <- cv_lasso$lambda.1se
# cat("Lambda 1se:", lambda_1se, "\n")
# lasso_mod_1se <- glmnet(X, y, alpha = 1, lambda = lambda_1se)
# coef_lasso_1se <- coef(lasso_mod_1se)
# print(coef_lasso_1se)
#
# #----------RIDGE REGRESSION-----------------
#
#
# cv_RIDGE <- cv.glmnet(X, y, alpha = 0)
# plot(cv_RIDGE)
#
# best_lambda <- cv_lasso$lambda.min
# cat("Best lambda:", best_lambda, "\n")
#
# ridge_model <- glmnet(X, y, alpha = 0, lambda = best_lambda)
# print(ridge_model)
# summary(ridge_model)
#
# coef_ridge <- coef(ridge_model)
# print(coef_ridge)
#
# lambda_1se <- cv_RIDGE$lambda.1se
# cat("Lambda 1se:", lambda_1se, "\n")
# ridge_mod_1se <- glmnet(X, y, alpha = 0, lambda = lambda_1se)
# coef_ridge_1se <- coef(ridge_mod_1se)
# print(coef_ridge_1se)


# Comparing with MSE, MAE ------

set.seed(123098)
# Splitting data
idx <- sample(seq_len(nrow(data)), size = 0.7 * nrow(data))
train_data <- data[idx, ]
test_data  <- data[-idx, ]


lm.fit <- lm(acc ~ ., data = train_data)
stepwise_lm_BIC <- stepAIC(lm.fit, direction = "both",
                           k = log(nrow(train_data)))
stepwise_lm_AIC <- stepAIC(lm.fit, direction = "both")

# Negative Binomial
glm.fit <- glm.nb(acc ~ ., data = train_data)
#The algorithm did not converge.
#stepwise_glm_BIC <- stepAIC(glm.fit, direction = "both",
#                            k = log(nrow(train_data)))
#The algorithm did not converge.
#stepwise_glm_AIC <- stepAIC(glm.fit, direction = "both")

#Negative Binomial with offset
glm.fit_offset <- glm.nb(acc ~ . -ln_pi - ln_fi + offset(ln_pi + ln_fi),
                         data = train_data)


# Preparing matrices for glmnet models
X_train <- model.matrix(acc ~ ., data = train_data)[, -1]
y_train <- train_data$acc
X_test  <- model.matrix(acc ~ ., data = test_data)[, -1]
y_test  <- test_data$acc

# LASSO Regression
cv_lasso <- cv.glmnet(X_train, y_train, alpha = 1)
lasso_model <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.min)
lasso_model_1se <- glmnet(X_train, y_train, alpha = 1, lambda = cv_lasso$lambda.1se)
plot(cv_lasso)


# Ridge Regression
cv_ridge <- cv.glmnet(X_train, y_train, alpha = 0)
ridge_model <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.min)
ridge_model_1se <- glmnet(X_train, y_train, alpha = 0, lambda = cv_ridge$lambda.1se)
plot(cv_ridge)

# Elastic Net
# find best alpha
alpha_values <- seq(0,1,0.1)
# store results
alph_res <- data.frame(alpha=numeric(), lambda=numeric(), mse=numeric())
# loop through the alpha values
for(a in alpha_values){
  cv_elastic <- cv.glmnet(X_train, y_train, alpha = a)
  best_lambda <- cv_elastic$lambda.min
  min_mse <- min(cv_elastic$cvm)
  result <- rbind(data.frame(alpha=a, lambda=best_lambda, mse=min_mse))
}
best_alpha <- result$alpha[which.min(result$mse)]
best_alpha
# best_alpha is 1 which makes result of elastic net identical to lasso

# Relaxed Lasso
cv_relax <- cv.glmnet(X_train, y_train, alpha = 1, relax=TRUE)
best_lambda_relax <- cv_relax$lambda.min
relax_model <- glmnet(X_train, y_train, alpha = 1, lambda = cv_relax$lambda.min, relax=TRUE)
relax_model_1se <- glmnet(X_train, y_train, alpha = 1, lambda = cv_relax$lambda.1se, relax=TRUE)
plot(cv_relax, xvar="lambda", label=TRUE)


# Random Forest Model
# for RF we need to change back dummy variables to factors
# remove dummy variables and add boroughs as factors
dummy_vars <- c("borough_combhigh","borough_combmedium")
data_RF <- data %>% dplyr::select(-all_of(dummy_vars))
data_RF <- data_RF %>% left_join(dplyr::select(data_dum,int_no,borough_comb), by="int_no")
data_RF$borough_comb <- as.factor(data_RF$borough_comb)
#data_RF <- data_RF %>% dplyr::select(-int_no)
str(data_RF)

# split rf data
train_rf <- data_RF[idx,]
test_rf <- data_RF[-idx,]

rf_model <- randomForest(acc ~. -int_no, data = train_rf, ntree = 500, importance = TRUE)
best_tree <- which.min(rf_model$mse)
rf_model_opt <- randomForest(acc ~. -int_no, data = train_rf, ntree = best_tree, importance = TRUE)
# find important variables in rf_model

importance(rf_model_opt)
varImpPlot(rf_model_opt)

# Predictions
pred_BIC   <- predict(stepwise_lm_BIC, newdata = test_data)
pred_AIC   <- predict(stepwise_lm_AIC, newdata = test_data)
#pred_glm_BIC <- predict(stepwise_glm_BIC, newdata = test_data)
#pred_glm_AIC <- predict(stepwise_glm_AIC, newdata = test_data)
pred_glm_offset <- predict(glm.fit_offset, newdata = test_data)
pred_lasso <- predict(lasso_model, newx = X_test)
pred_lasso_1se <- predict(lasso_model_1se, newx = X_test)
pred_ridge <- predict(ridge_model, newx = X_test)
pred_ridge_1se <- predict(ridge_model_1se, newx = X_test)
pred_relax <- predict(relax_model, newx = X_test)
pred_relax_1se <- predict(relax_model_1se, newx = X_test)
pred_rf    <- predict(rf_model_opt, newdata = test_rf)

# Calculating MSE
mse_BIC   <- mean((y_test - pred_BIC)^2)
mse_AIC   <- mean((y_test - pred_AIC)^2)
#mse_glm_BIC <- mean((y_test - pred_glm_BIC)^2)
#mse_glm_AIC <- mean((y_test - pred_glm_AIC)^2)
mse_glm_offset <- mean((y_test - pred_glm_offset)^2)
mse_lasso <- mean((y_test - pred_lasso)^2)
mse_lasso_1se <- mean((y_test - pred_lasso_1se)^2)
mse_ridge <- mean((y_test - pred_ridge)^2)
mse_ridfe_1se <- mean((y_test - pred_ridge_1se)^2)
mse_relax <- mean((y_test - pred_relax)^2)
mse_relax_1se <- mean((y_test - pred_relax_1se)^2)
mse_rf    <- mean((y_test - pred_rf)^2)

# Calculating MAE
mae_BIC   <- mean(abs(y_test - pred_BIC))
mae_AIC   <- mean(abs(y_test - pred_AIC))
#mae_glm_BIC <- mean(abs(y_test - pred_glm_BIC))
#mae_glm_AIC <- mean(abs(y_test - pred_glm_AIC))
mae_glm_offset <- mean(abs(y_test - pred_glm_offset))
mae_lasso <- mean(abs(y_test - pred_lasso))
mae_lasso_1se <- mean(abs(y_test - pred_lasso_1se))
mae_ridge <- mean(abs(y_test - pred_ridge))
mae_ridge_1se <- mean(abs(y_test - pred_ridge_1se))
mae_relax <- mean(abs(y_test - pred_relax))
mae_relax_1se <- mean(abs(y_test - pred_relax_1se))
mae_rf    <- mean(abs(y_test - pred_rf))

# Number of coefficients
n_coef_BIC <- length(stepwise_lm_BIC$coefficients)
n_coef_AIC <- length(stepwise_lm_AIC$coefficients)
n_coef_glm_offset <- length(glm.fit_offset$coefficients)
n_coef_lasso <- length(rownames(lasso_model$beta)[which(lasso_model$beta != 0)])
n_coef_lasso_1se <- length(rownames(lasso_model_1se$beta)[which(lasso_model_1se$beta != 0)])
n_coef_ridge <- length(rownames(ridge_model$beta)[which(ridge_model$beta != 0)])
n_coef_ridge_1se <- length(rownames(ridge_model_1se$beta)[which(ridge_model_1se$beta != 0)])
n_coef_relax <- length(rownames(relax_model$beta)[which(relax_model$beta != 0)])
n_coef_relax_1se <- length(rownames(relax_model_1se$beta)[which(relax_model_1se$beta != 0)])
n_coef_rf <- 24


# table of results
results <- data.frame(
  Model = c("Lm Step BIC", "Lm Step AIC", #"NB BIC", "NB AIC",
            "NBl Offset",
            "LASSO", "LASSO 1se", "RIDGE", "RIDGE 1se",
            "Relaxed LASSO", "Relaxed LASSO 1se","Random Forest"),
  MSE = c(mse_BIC, mse_AIC, #mse_glm_BIC, mse_glm_AIC,
          mse_glm_offset, mse_lasso, mse_lasso_1se, mse_ridge,
          mse_ridfe_1se, mse_relax, mse_relax_1se, mse_rf),
  MAE = c(mae_BIC, mae_AIC, #mae_glm_BIC, mae_glm_AIC,
          mae_glm_offset, mae_lasso, mae_lasso_1se, mae_ridge,
          mae_ridge_1se,mae_relax, mae_relax_1se, mae_rf),
  Coef = c(n_coef_BIC, n_coef_AIC, #n_coef_glm_BIC, n_coef_glm_AIC,
           n_coef_glm_offset, n_coef_lasso, n_coef_lasso_1se, n_coef_ridge,
           n_coef_ridge_1se, n_coef_relax, n_coef_relax_1se, n_coef_rf)
)

sort_by(results, results$MSE)

# --------------------------------------------------------------------
# GAM
# --------------------------------------------------------------------
install.packages("mgcv")
#install.packages("gam")
install.packages("splines")

library(gam)
library(splines)
library(mgcv)

str(data)

# during the process of variable selection model with Lasso method
# plus one se interval display the best combination of low MSE and MAE
# terms and number of selected variables. We will use these variables
# to fit the GAM model.

coef_lasso_df <- as.data.frame(as.matrix(coef(lasso_model_1se)))
# filter the rows with value not equal to 0
coef_lasso_df <- coef_lasso_df %>%
  filter(coef_lasso_df$s0 != 0)
# select the variables according to lasso selection
data_lasso <- data %>%
  dplyr::select(c("acc",all_of(row.names(coef_lasso_df)[-1])))
str(data_lasso)
data_lasso$green_stra <- as.factor(data_lasso$green_stra)
data_lasso$commercial_comb <- as.factor(data_lasso$commercial_comb)
data_lasso$number_of_comb <- as.factor(data_lasso$number_of_comb)

# select continuous variables
double_df <- data_lasso %>%
  dplyr::select(where(is.double))
str(double_df)

double_df$acc <- as.numeric(data$acc)

# Plot the relationship between accidents and continuous variables
# pivot the data
double_df_long <- double_df %>%
  pivot_longer(cols = -acc,names_to = "variable", values_to = "value")
# scatter plot
ggplot(double_df_long, aes(x=value, y=acc))+
  geom_point(alpha=0.5, shape=21, size=2)+
  facet_wrap(~variable, scales="free")+
  geom_smooth(method="loess", color="red", size=0.5)+
  theme_classic()+
  labs(title="Accidents vs Continuous Variables",
       x="Value",
       y="Accidents")

# GAM model
# split the data
train_gam <- data_lasso[idx,]
test_gam <- data_lasso[-idx,]

# fit the model
gam_model <- mgcv::gam(acc ~ lo(ln_pi) + lo(ln_fi) + lo(tot_road_w) +
                   factor(green_stra) + factor(commercial_comb) +
                   factor(number_of_comb), family=nb(), data=train_gam, cv=FALSE)
pred_gam <- predict(gam_model, newdata=test_gam)

# calculate MSE and MAE
mse_gam <- mean((test_gam$acc - pred_gam)^2)
mae_gam <- mean(abs(test_gam$acc - pred_gam))
n_coef_gam <- length(gam_model$coefficients)
AIC(gam_model)
BIC(gam_model)

summary(gam_model)

results <- rbind(results, data.frame(Model="GAM", MSE=mse_gam,
                                     MAE=mae_gam, Coef=n_coef_gam))
# Output results
sort_by(results,results$MSE)



AIC_BIC_res <- rbind(AIC_BIC_res, data.frame(Model="GAM", AIC=AIC(gam_model),
                                              BIC=BIC(gam_model), coef=n_coef_gam))
sort_by(AIC_BIC_res, AIC_BIC_res$AIC)

# --------------------------------------------------------------------
# GAM_FULL
# --------------------------------------------------------------------

# coef_nb_df <- as.data.frame(
#   summary(glm.fit)$coefficients[which(summary(glm.fit)$coefficients[,4] < 0.05),])
# str(coef_nb_df)
# data_nb <- data %>%
#   dplyr::select(c("acc",all_of(row.names(coef_nb_df)[-1])))
str(data)

double_df <- data %>%
  dplyr::select(where(is.double))
str(double_df)

#split the data
train_gam_full <- data[idx,]
test_gam_full <- data[-idx,]

gam_full <- mgcv::gam(acc ~ lo(as.numeric(date_)) + all_pedest + lo(ln_pi)+
                   lo(ln_fi) + lo(ln_cli) + lo(ln_cri) + lo(ln_cti) +
                  lo(avg_crossw) + lo(tot_road_w) + median + green_stra +
                  any_ped_pr + ped_countd + lt_protect + lt_restric +
                  factor(parking) + curb_exten + new_half_r + lo(distdt_1000) +
                  factor(commercial_comb) + factor(of_exclusi_comb) +
                  factor(total_lane_comb) + factor(number_of_comb) +
                  borough_combhigh + borough_combmedium,
                data=train_gam_full, family=nb(), cv=FALSE)
pred_gam_full <- predict(gam_full, newdata=test_gam_full)

mse_gam_full <- mean((test_gam_full$acc - pred_gam_full)^2)
mae_gam_full <- mean(abs(test_gam_full$acc - pred_gam_full))
n_coef_gam_full <- length(gam_full$coefficients)


results <- rbind(results, data.frame(Model="GAM Full", MSE=mse_gam_full,
                                     MAE=mae_gam_full, Coef=n_coef_gam_full))

AIC_BIC_res <- rbind(AIC_BIC_res, data.frame(Model="GAM Full", AIC=AIC(gam_full),
                                              BIC=BIC(gam_full), coef=n_coef_gam_full))

sort_by(results,results$MSE)
sort_by(AIC_BIC_res, AIC_BIC_res$AIC)

summary(gam_full)

# --------------------------------------------------------------------
# GAM_FULL with offset
# --------------------------------------------------------------------
gam_offset <- mgcv::gam(acc ~ offset(ln_pi+ln_fi) + lo(as.numeric(date_)) + all_pedest +
                   lo(ln_cli) + lo(ln_cri) + lo(ln_cti) +
                   lo(avg_crossw) + lo(tot_road_w) + median + green_stra +
                   any_ped_pr + ped_countd + lt_protect + lt_restric +
                   factor(parking) + curb_exten + new_half_r + lo(distdt_1000) +
                   factor(commercial_comb) + factor(of_exclusi_comb) +
                   factor(total_lane_comb) + factor(number_of_comb) +
                   borough_combhigh + borough_combmedium,
                data=train_gam_full, family=nb(), cv=FALSE)
pred_gam_offset <- predict(gam_offset, newdata=test_gam_full)

mse_gam_offset <- mean((test_gam_full$acc - pred_gam_offset)^2)
mae_gam_offset <- mean(abs(test_gam_full$acc - pred_gam_offset))
n_coef_gam_offset <- length(gam_offset$coefficients)


summary(gam_offset)
results <- rbind(results, data.frame(Model="GAM Full Offset", MSE=mse_gam_offset,
                                     MAE=mae_gam_offset, Coef=n_coef_gam_offset))

AIC_BIC_res <- rbind(AIC_BIC_res, data.frame(Model="GAM Full Offset", AIC=AIC(gam_offset),
                                              BIC=BIC(gam_offset), coef=n_coef_gam_offset))
sort_by(results,results$MSE)
sort_by(AIC_BIC_res, AIC_BIC_res$AIC)


# Variance of target variable
variance_acc <- var(data$acc)
variance_acc


# most dangerous intersections ----------------------------------------------------------


# Make predictions on the full dataset using the trained Random Forest model
data_RF$predicted_acc <- predict(rf_model_opt, newdata = data_RF)

# Get the top 10 most dangerous intersections
top_10_intersections <- data_RF %>%
  group_by(int_no) %>%
  summarise(predicted_accidents = sum(predicted_acc)) %>%
  arrange(desc(predicted_accidents)) %>%
  slice_head(n = 10)

# Display the top 10 dangerous intersections
print(top_10_intersections)

# Filter original dataset to include only rows from the top 10 intersections
top_10_in_data <- data %>%
  filter(int_no %in% top_10_intersections$int_no) %>%
  dplyr::select (int_no, acc) %>%
  arrange(desc(acc))

# Display filtered data
print(top_10_in_data)
top_10 <- data %>%
  filter(int_no %in% top_10_intersections$int_no)
View(top_10)
# Variable importance ----------------------------------------------------------------------------

# Get variable importance from the trained Random Forest model
var_imp <- importance(rf_model_opt)

# Convert to a dataframe for easy visualization
var_imp_df <- data.frame(
  Variable = rownames(var_imp),
  Importance = var_imp[, 1] # MeanDecreaseGini or IncNodePurity
) %>%
  arrange(desc(Importance))

# Display variable importance
print(var_imp_df)



#random forest adjusted to rate -----------------------------------------------------------

# Adjust accidents based on total estimated exposure over 10 years
# data <- data %>%
#   mutate(acc_rate = acc / ((ln_pi + ln_fli +ln_fti + ln_fri) * 3650 + 1))  # Normalize by 10-year pedestrian & vehicle volume

# Train Random Forest on accident rate
rf_model_rate <- randomForest(acc ~ ., data = data_RF, ntree = best_tree, importance = TRUE)

# Predict accident rate
data_RF$predicted_acc_rate <- predict(rf_model_rate, newdata = data_RF)

# Convert back to accident predictions
data_RF$predicted_acc_adjusted <- data_RF$predicted_acc_rate * ((data$ln_pi ) * 3650)



#variable importance -------------------------------------------------------

var_imp <- importance(rf_model_rate)

# Convert to a dataframe for easy visualization
var_imp_df <- data.frame(
  Variable = rownames(var_imp),
  Importance = var_imp[, 1] # MeanDecreaseGini or IncNodePurity
) %>%
  arrange(desc(Importance))

# Display variable importance
print(var_imp_df)


#---------------------------------------------------------------------
# preparing CSV
#---------------------------------------------------------------------
# create a dataframe with the int_no and predicted accidents
intersectin_risk <- data_RF %>%
  select(int_no, predicted_acc) %>%
  arrange(desc(predicted_acc)) %>%
  mutate(rank=row_number())

# select only ID and rank
intersection_rank <- intersectin_risk %>%
  select(int_no, rank)

# write to csv
write.csv(intersection_rank, "intersection_rank.csv")






