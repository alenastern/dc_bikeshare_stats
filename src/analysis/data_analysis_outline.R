library(lubridate)
library(reshape)
library(tidyverse)
library(readxl)
library(scales)
library(devtools)
library(here)
library(dplyr)
library(treemapify)
library(grid)
library(gridExtra)
library(ggplot2)
library(ggmap)
library(zipcode)
library(geojsonR)
library(geojsonsf)
library(sf)
library(rjson)
library(zoo)

# Set Working Directory

setwd("/Users/alenastern/Documents/Win2019/MultiTesting/dc_bikeshare_stats/")
#source(here("src/exploration","get_data.R"))
#source(here("src/exploration","data_timelags.R"))
source("src/exploration/get_data.R")
source("src/exploration/data_timelags.R")

### Step 0: Prep Final Data for Analysis

#df.final.timelag <- df.final.timelag %>% mutate_all(funs(replace(., is.na(.), 0)))

total_data_panel <- total_data_panel %>% mutate_all(funs(replace(., is.na(.), 0)))


### Step 1: Split Data into Training, Validation, Testing Sets ###

train_test_split <- function(data, y_var, bg){
  # data = data frame
  # y_var = dependent variable
  # bg = boolean, whether randomizing on block group
 
  # Case when not randomizing on block group 
  if(bg != TRUE) {
    X <- data %>% select(-y_var)
    y <- data[[y_var]]
    
    ### sample observations across BG/Months
    n = length(X)
    pct_train_val = .5
    num_samp = n*pct_train_val
    set.seed(2019); samp = sample(n,num_samp) 
    train = samp[1:(num_samp/2)] # these observations will be used as a training data set
    val = samp[(num_samp/2 + 1):num_samp] # these observations will be used as a validation set
    Xtrain = X[train,]; ytrain = y[train]
    Xval = X[val,]; yval = y[val]
    Xtest = X[-samp,]; ytest = y[-samp] # the remaining n*(1-pct_train_val) observations are the test set
  } else {

  ### sample BGs, take all months for each BG ###

    unique_geo <- unique(as.vector(data$GEOID))
    n = length(unique_geo)
    pct_train_val = .5
    num_samp = ceiling(n*pct_train_val)
    set.seed(2019); samp = sample(n,num_samp) 
    train = samp[1:ceiling(num_samp/2)] # these GEOIDs will be used as a training data set
    val = samp[(ceiling(num_samp/2) + 1):num_samp] # these GEOIDs will be used as a validation set
    train_geo = unique_geo[train]; val_geo = unique_geo[val]; train_val_geo = append(train_geo, val_geo)
    train_set = data[data$GEOID %in% train_geo,]
    val_set = data[data$GEOID %in% val_geo,]
    test_set = data[!data$GEOID %in% train_val_geo, ]
    
  # Testing Code  
  # reduce(union, list(unique(as.vector(train_set$GEOID)), unique(as.vector(val_set$GEOID)), unique(as.vector(test_set$GEOID))))
  # reduce(intersect, list(unique(as.vector(train_set$GEOID)), unique(as.vector(val_set$GEOID)), unique(as.vector(test_set$GEOID))))
  # identify no GEOIDs in common and 85 in union

    Xtrain <- data.matrix(train_set[ , ! colnames(train_set) %in% c(y_var) ])
    ytrain <- data.matrix(train_set[[y_var]])
    Xval <- data.matrix(val_set[ , ! colnames(val_set) %in% c(y_var) ])
    yval <- data.matrix(val_set[[y_var]])
    Xtest <- data.matrix(test_set[ , ! colnames(test_set) %in% c(y_var) ])
    ytest <- data.matrix(test_set[[y_var]])
  }
  df_list = list(Xtrain, ytrain, Xval, yval, Xtest, ytest)
  return(df_list) 
}


df_list <- train_test_split(df.final.timelag, "n_rides_tot", TRUE)
Xtrain <- df_list[[1]]
ytrain <- df_list[[2]]
Xval <- df_list[[3]]
yval <- df_list[[4]]
Xtest <- df_list[[5]]
ytest <- df_list[[6]]

### Step 2: Set penalty ###

### find column indices for variables we do not want to apply shrinkage (eg. definitely include these variables in final model)

no_shrinkage_list = c("total_bl", "season_year", "race")

# identify indices of 'no-shrinkage' variables
var_indices <- c()
for (var in no_shrinkage_list) {
  idx <- grep("B", colnames(Xtrain))
  var_indices <- append(var_indices, c(idx))
}

# initializes vector of 1s
penalty_factor <- rep(1, length(colnames(Xtrain)))

# replaces indices corresponding to 'no-shrinkage' variables with 0
penalty_factor <- replace(penalty_factor, var_indices, 0)



### Step 3a:LR
coef_lm = lm(ytrain~Xtrain)$coef 
lm = list(name = "coef_lm", b0 = coef_lm[1], b = coef_lm[-1])

### Step 3a:Lasso
cv_lasso = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 1, n = 10)
lamb_ = cv_lasso$lambda.min
trained_lasso = glmnet(x = Xtrain, y = ytrain,  alpha = 1, lambda = lamb_) 
coef_lasso = coef(trained_lasso)
lasso = list(name = "coef_lasso", b0 = coef_lasso[1], b = coef_lasso[-1])
reg_path_lasso = cv_lasso$beta

### Step 3b: Ridge
cv_ridge = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 0, n = 10)
lamb_ = cv_ridge$lambda.min
trained_ridge = glmnet(x = Xtrain, y = ytrain,  alpha = 0, lambda = lamb_) 
coef_ridge = coef(trained_ridge)
ridge = list(name = "coef_ridge", b0 = coef_ridge[1], b = coef_ridge[-1])
reg_path_ridge = cv_ridge$beta

### Step 3c: Elastic Net
cv_elastic_net = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 0.5, n = 10)
lamb_ = cv_elastic_net$lambda.min
trained_elastic_net = glmnet(x = Xtrain, y = ytrain,  alpha = 0, lambda = lamb_) 
coef_elastic_net = coef(trained_elastic_net)
elastic_net = list(name = "coef_elastic_net", b0 = coef_elastic_net[1], b = coef_elastic_net[-1])
reg_path_elastic_net = cv_elastic_net$beta

### Step 3d: Forward Selection

### Step 3e: Poisson
cv_poisson = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, family = "poisson", n = 10)
lamb_ = cv_poisson$lambda.min
trained_poison = glmnet(Xtrain, ytrain, family = "poisson", lambda = lamb_)
coef_poisson = coef(trained_poison)
poisson = list(name = "coef_poiss", b0 = coef_poisson[1], b = coef_poisson[-1])
reg_path_poisson = cv_poisson$beta

### Step 4: Assess Model on Test Set ###

### Step 4a: Assess accuracy in terms of minimizing RMSE 

### Step 4b: Calculate 90% quantile

### Step 4c: Calculate prediction interval and discuss width

### Step 4d: Assess coverage of prediction interval on test set

### Step 4e: Plot Residuals for Each Model

# inspired by: https://drsimonj.svbtle.com/visualising-residuals

plot_resids <- function(residuals, y, predicted, Xdf, var_list) {
  
  d <- cbind(y, predicted, residuals, Xdf)
  d <- data.frame(d)
  d <- d %>% rename(y = V1, predicted = V2, residuals = V3)
  var_list <- append(var_list, c("y", "predicted", "residuals"))
  d <- d[var_list]
  
  d %>% gather(key = "iv", value = "x", -y, -predicted, -residuals) %>%  # Get data into shape
    ggplot(aes(x = x, y = y)) +  # Note use of `x` here and next line
    geom_segment(aes(xend = x, yend = predicted), alpha = .2) +
    geom_point(aes(color = residuals)) +
    scale_color_gradient2(low = "blue", mid = "white", high = "red") +
    guides(color = FALSE) +
    geom_point(aes(y = predicted), shape = 1) +
    facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
    theme_bw()
  
}

### Step 4f: Plot Prediction Intervals + Coverage

plot_pi <- function(q90, y, predicted, Xdf, var_list) {
  
  d <- cbind(y, predicted, Xdf)
  d <- data.frame(d)
  d <- d %>% rename(y = V1, predicted = V2)
  d <- d %>% mutate(upper = predicted + q90, lower = predicted - q90, covered = ifelse(y >= lower & y <= upper, 1, 0))
  d$covered <- factor(d$covered, levels = c(0, 1))
  
  var_list <- append(var_list, c("y", "predicted", "upper", "lower", "covered"))
  d <- d[var_list]
  
  d %>% 
    gather(key = "iv", value = "x", -y, -predicted, -upper, -lower, -covered) %>%  # Get data into shape
    ggplot(aes(x = x, y = y)) +  # Note use of `x` here and next line
    geom_segment(aes(xend = x, y = lower, yend = upper), alpha = .2) +
    geom_point(shape = 21, size = 2, aes(fill = covered)) +
    scale_fill_manual(values = c("#cc0000","#339933")) +
    facet_grid(~ iv, scales = "free_x") +  # Split panels here by `iv`
    theme_bw()
  
}

model_performance = data.frame()
index = 0
#for (model in list(lm, lasso, ridge, elastic_net, poisson)){
for (model in list(lm)) {
  index = index + 1
  model_performance[index, "model"] = model$name
  model_performance[index, "type"] = "non-truncated"
  
  # PI width using training set
  yhat_train = model$b0 + Xtrain%*%model$b 
  resids_train = ytrain - yhat_train # Residuals
  q_train = quantile(abs(resids_train),0.9)
  
  # PI width using validation set
  yhat_val = model$b0 + Xval%*%model$b
  resids_val = yval - yhat_val # Residuals
  q_val = quantile(abs(resids_val),0.9)
  
  model_performance[index, "q90_train"] = q_train
  model_performance[index, "q90_val"] = q_val
  model_performance[index, "pi_train"] = q_train*2
  model_performance[index, "pi_val"] = q_val*2
  
  # predictions on test set
  yhat_test = model$b0 + Xtest%*%model$b
  resids_test = ytest - yhat_test
  
  # prediction error on the training set:
  RMSE_train = sqrt(mean((ytrain - yhat_train)^2))
  
  # prediction error on the test set:
  RMSE_test = sqrt(mean((ytest - yhat_test)^2))
  
  # differences between both: 
  model_performance[index, "RMSE_train"] = RMSE_train
  model_performance[index, "RMSE_test"] = RMSE_test
  model_performance[index, "RMSE_dif"] = abs(RMSE_test - RMSE_train)
  
  # coverage on training & test set, using q_train
  model_performance[index, "cov_train_qtrain"] = mean(yhat_train - q_train <= ytrain & ytrain <= yhat_train + q_train)
  model_performance[index, "cov_test_qtrain"] = mean(yhat_test - q_train <= ytest & ytest <= yhat_test + q_train)
  
  # coverage on training & test set, using q_val
  model_performance[index, "cov_train_qval"] = mean(yhat_train - q_val <= ytrain & ytrain <= yhat_train + q_val)
  model_performance[index, "cov_test_qval"] = 	mean(yhat_test - q_val <= ytest & ytest <= yhat_test + q_val)
  
  var_list = c("n_bl_tot")
  
  filename_pr_train = paste("src/analysis/images/", model$name,"_train_pr.png", sep = "")
  plot_resids(resids_train, ytrain, yhat_train, Xtrain, var_list)
  ggsave(filename_pr_train, width = 30, height = 20, units = "cm")
  
  filename_pr_test = paste("src/analysis/images/", model$name,"_test_pr.png", sep = "")
  plot_resids(resids_test, ytest, yhat_test, Xtest, var_list)
  ggsave(filename_pr_test, width = 30, height = 20, units = "cm")
  
  filename_pi_train = paste("src/analysis/images/", model$name,"_train_pi.png", sep = "")
  plot_pi(q_train, ytrain, yhat_train, Xtrain, var_list)
  ggsave(filename_pi_train, width = 30, height = 20, units = "cm")
  
  filename_pi_test = paste("src/analysis/images/", model$name,"_test_pi.png", sep = "")
  plot_pi(q_val, ytest, yhat_test, Xtest, var_list)
  ggsave(filename_pi_test, width = 30, height = 20, units = "cm")
  
} 


### Step 5: Local Hypothesis Testing ###

#Q: Not splitting data
### 5a: fit final model on each local hypothesis (each block group = one hypothesis)

### 5b: use false discovery 





