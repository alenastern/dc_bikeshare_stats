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
library(ggmap)
library(zipcode)
library(geojsonR)
library(geojsonsf)
library(sf)
library(rjson)
library(zoo)
library(glmnet)
library(grpreg)
library(car)


# Set Working Directory

#setwd("~/Desktop/UChi/Classes/Stats/MultipleTesting_ModernInference/project_bikeshare/dc_bikeshare_stats/") #Cris' directory
#setwd("/Users/alenastern/Documents/Win2019/MultiTesting/dc_bikeshare_stats/")
#setwd('/mnt/dm-3/alix/Documents/Multiple Testing/dc_bikeshare_stats/')
source("src/exploration/get_data.R")
source("src/exploration/data_timelags.R")

### Step 0: Prep Final Data for Analysis

df.final.timelags <- df.no.dups
df.final.timelags <- df.final.timelags[ , ! colnames(df.final.timelags) %in% c('county', '(Intercept)', 'tract') ]
df.final.timelags <- df.final.timelags %>% mutate_all(funs(replace(., is.na(.), 0)))

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
  
    # seve geoid column for each set
    train_set_geo <- train_set$GEOID
    val_set_geo <- val_set$GEOID
    test_set_geo <- test_set$GEOID
    geo_list <- c(train_set_geo, val_set_geo, test_set_geo)
    Xtrain <- data.matrix(train_set[ , ! colnames(train_set) %in% c(y_var, 'GEOID') ])
    ytrain <- data.matrix(train_set[[y_var]])
    Xval <- data.matrix(val_set[ , ! colnames(val_set) %in% c(y_var, 'GEOID') ])
    yval <- data.matrix(val_set[[y_var]])
    Xtest <- data.matrix(test_set[ , ! colnames(test_set) %in% c(y_var, 'GEOID') ])
    ytest <- data.matrix(test_set[[y_var]])
  }
  df_list = list(Xtrain, ytrain, Xval, yval, Xtest, ytest, geo_list)
  return(df_list) 
}


df_list <- train_test_split(df.no.dups, "n_rides_tot", TRUE)
Xtrain <- df_list[[1]]
ytrain <- df_list[[2]]
Xval <- df_list[[3]]
yval <- df_list[[4]]
Xtest <- df_list[[5]]
ytest <- df_list[[6]]
geo_list <- df_list[[7]]

### Step 2: Set penalty ###

### Step 2a: find column indices for variables we do not want to apply shrinkage (eg. definitely include these variables in final model)

no_shrinkage_list = c("total_bl", "season", "race_white", "race_black", "race_asian", "race_other", "^male", "female", "median_age", "age_under18", "age_18to24", "age_25to34", "age_35to44", "age_45to54", "age_55to64", "age_65up", "income_less_than_30k", "income_30to59k", "income_60to99k", "income_100up")


# identify indices of 'no-shrinkage' variables
ns_var_indices <- c()
for (var in no_shrinkage_list) {
  idx <- grep(var, colnames(Xtrain))
  print(var)
  print(colnames(Xtrain)[idx])
  print(idx)
  ns_var_indices <- append(ns_var_indices, c(idx))
}

colnames(Xtrain)[ns_var_indices]

# initializes vector of 1s
penalty_factor <- rep(1, length(colnames(Xtrain)))

# replaces indices corresponding to 'no-shrinkage' variables with 0
penalty_factor <- replace(penalty_factor, ns_var_indices, 0)

sum(penalty_factor[ns_var_indices])

### Step 2b: define groups for grouped lasso

groups_time = c("1bef", "2bef", "3bef", "4bef", "5bef", "6bef", "7bef", "8bef", 
           "9bef", "10bef", "11bef", "12bef")

groups_bl_type = c("l_cat", "l_name")

groups_bl_type_v2 = c("l_name")

# function to create index parameter for grouped lasso
make_index_list <- function(groups, ns_var_indices, Xtrain){
  # groups = list of groups
  # ns_var_indices = indices of variables to not penalize
  # Xtrain = training features
  index = rep(NA, length(colnames(Xtrain)))
  gp_index_num = 1
  for(gp in groups){
    gp_idx <- grep(gp, colnames(Xtrain))
    index <- replace(index, gp_idx, gp_index_num)
    gp_index_num = gp_index_num + 1
  }
  
  for(i in 1:length(index)){
    if (is.na(index[i])){
      index[i] = gp_index_num
      gp_index_num = gp_index_num + 1
    }
  }
  
  # don't penalize non-shrinkage vars
  index = replace(index, ns_var_indices, 0)
  return(index)
}


### Step 3a:LR
coef_lm = lm(ytrain~Xtrain)$coef 
lm = list(name = "coef_lm", b0 = coef_lm[1], b = coef_lm[-1])

### Step 3a:Lasso

cv_lasso = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 1, nfolds = 10, penalty.factor = penalty_factor)
lamb_ = cv_lasso$lambda.min
trained_lasso = glmnet(x = Xtrain, y = ytrain,  alpha = 1, lambda = lamb_, penalty.factor = penalty_factor) 
coef_lasso = coef(trained_lasso)
lasso = list(name = "coef_lasso", b0 = coef_lasso[1], b = coef_lasso[-1])
reg_path_lasso = trained_lasso$beta
lasso_coef <- as.data.frame(as.matrix(reg_path_lasso))
non_zero_lasso <- lasso_coef %>%
  rownames_to_column('var') %>%
  filter(s0 != 0) 

### Step 3b: Ridge
cv_ridge = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 0, nfolds = 10, penalty.factor = penalty_factor)
lamb_ = cv_ridge$lambda.min
trained_ridge = glmnet(x = Xtrain, y = ytrain,  alpha = 0, lambda = lamb_, penalty.factor = penalty_factor) 
coef_ridge = coef(trained_ridge)
ridge = list(name = "coef_ridge", b0 = coef_ridge[1], b = coef_ridge[-1])
reg_path_ridge = cv_ridge$beta

### Step 3c: Elastic Net

cv_elastic_net = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 0.5, nfolds = 10, penalty.factor = penalty_factor)
lamb_ = cv_elastic_net$lambda.min
trained_elastic_net = glmnet(x = Xtrain, y = ytrain,  alpha = 0, lambda = lamb_, penalty.factor = penalty_factor) 
coef_elastic_net = coef(trained_elastic_net)
elastic_net = list(name = "coef_elastic_net", b0 = coef_elastic_net[1], b = coef_elastic_net[-1])
reg_path_elastic_net = cv_elastic_net$beta

### Step 3d: Poisson
cv_poisson = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, family = "poisson", nfolds = 10, penalty.factor = penalty_factor)
lamb_ = cv_poisson$lambda.min
trained_poison = glmnet(Xtrain, ytrain, family = "poisson", lambda = lamb_, penalty.factor = penalty_factor)
coef_poisson = coef(trained_poison)
poisson = list(name = "coef_poiss", b0 = coef_poisson[1], b = coef_poisson[-1])
reg_path_poisson = cv_poisson$beta

### Step 3e.1: Grouped Lasso Linear (bl group)

index_bl = make_index_list(groups_bl_type, ns_var_indices, Xtrain)
index_time = make_index_list(groups_time, ns_var_indices, Xtrain)
index_bl2 = make_index_list(groups_bl_type_v2, ns_var_indices, Xtrain)


cvgrlasso = cv.grpreg(Xtrain, ytrain, index_bl, penalty = 'grLasso', family = "gaussian")
lamb_ = cvgrlasso$lambda.min
lamb_idx = cvgrlasso$min
grlasso = cvgrlasso$fit
reg_path_grlasso = grlasso$beta
coef_grlasso = reg_path_grlasso[ ,lamb_idx]
gp_lasso = list(name = "coef_gp_lasso", b0 = coef_grlasso[1], b = coef_grlasso[-1])

### Step 3e.2: Grouped Lasso Linear (lag group)

cvgrlasso = cv.grpreg(Xtrain, ytrain, index_time, penalty = 'grLasso', family = "gaussian")
lamb_ = cvgrlasso$lambda.min
lamb_idx = cvgrlasso$min
grlasso = cvgrlasso$fit
reg_path_grlasso = grlasso$beta
coef_grlasso = reg_path_grlasso[ ,lamb_idx]
gp_lasso_time = list(name = "coef_gp_lasso_time", b0 = coef_grlasso[1], b = coef_grlasso[-1])
### Step 3f: Grouped Lasso Linear

### Step 3e.3: Grouped Lasso Linear (bl group just name)

cvgrlasso = cv.grpreg(Xtrain, ytrain, index_bl2, penalty = 'grLasso', family = "gaussian")
lamb_ = cvgrlasso$lambda.min
lamb_idx = cvgrlasso$min
grlasso = cvgrlasso$fit
reg_path_grlasso = grlasso$beta
coef_grlasso = reg_path_grlasso[ ,lamb_idx]
gp_lasso_v2 = list(name = "coef_gp_lasso_v2", b0 = coef_grlasso[1], b = coef_grlasso[-1])

### Step 3f.1: Grouped Lasso Poisson (bl group)

cvgrlasso_poisson = cv.grpreg(Xtrain, ytrain, index_bl, penalty = 'grLasso', family = "poisson")
lamb_ = cvgrlasso_poisson$lambda.min
lamb_idx = cvgrlasso_poisson$min
grlasso_poisson = cvgrlasso_poisson$fit
reg_path_grlasso_poisson = grlasso_poisson$beta
coef_grlasso_poisson = reg_path_grlasso_poisson[ ,lamb_idx]
gp_lasso_poisson = list(name = "coef_gp_lasso_poisson", b0 = coef_grlasso_poisson[1], b = coef_grlasso_poisson[-1])
gp_poisson_coef <- as.data.frame(as.matrix(coef_grlasso_poisson))
non_zero_gp_poisson <- gp_poisson_coef %>%
  rownames_to_column('var') %>%
  filter(V1 != 0) 

### Step 3f.2: Grouped Lasso Poisson (lag group)

cvgrlasso_poisson = cv.grpreg(Xtrain, ytrain, index_time, penalty = 'grLasso', family = "poisson")
lamb_ = cvgrlasso_poisson$lambda.min
lamb_idx = cvgrlasso_poisson$min
grlasso_poisson = cvgrlasso_poisson$fit
reg_path_grlasso_poisson = grlasso_poisson$beta
coef_grlasso_poisson = reg_path_grlasso_poisson[ ,lamb_idx]
gp_lasso_poisson_time = list(name = "coef_gp_lasso_poisson_time", b0 = coef_grlasso_poisson[1], b = coef_grlasso_poisson[-1])

### Step 3f.3: Grouped Lasso Poisson (bl group just name)

cvgrlasso_poisson = cv.grpreg(Xtrain, ytrain, index_bl2, penalty = 'grLasso', family = "poisson")
lamb_ = cvgrlasso_poisson$lambda.min
lamb_idx = cvgrlasso_poisson$min
grlasso_poisson = cvgrlasso_poisson$fit
reg_path_grlasso_poisson = grlasso_poisson$beta
coef_grlasso_poisson = reg_path_grlasso_poisson[ ,lamb_idx]
gp_lasso_poisson_v2 = list(name = "coef_gp_lasso_poisson_v2l", b0 = coef_grlasso_poisson[1], b = coef_grlasso_poisson[-1])


### Step 4: Assess Model on Test Set ###

### Step 4a: Assess accuracy in terms of minimizing RMSE 

### Step 4b: Calculate 90% quantile

### Step 4c: Calculate prediction interval and discuss width

### Step 4d: Assess coverage of prediction interval on test set

### Step 4e: Plot Residuals for Each Model

# inspired by: https://drsimonj.svbtle.com/visualising-residuals
# not included in paper, see images folder of GitHub repository for results

plot_resids <- function(residuals, y, predicted, Xdf, var_list) {
  
  d <- cbind(y, predicted, residuals, Xdf)
  d <- data.frame(d)
  d <- d %>% dplyr::rename(y = V1, predicted = V2, residuals = V3)
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
  d <- d %>% dplyr::rename(y = V1, predicted = V2)
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
for (model in list(lm, lasso, ridge, elastic_net, poisson, gp_lasso, gp_lasso_poisson)){
  if (model$name == 'coef_lm'){
    model$b[is.na(model$b)] <- 0
    print(model$b)
  }

  index = index + 1
  model_performance[index, "model"] = model$name
  model_performance[index, "type"] = "non-truncated"
  
  print(model$name)
  
  # PI width using training set
  yhat_train = model$b0 + Xtrain%*%model$b 
  resids_train = ytrain - yhat_train # Residuals
  q_train = quantile(abs(resids_train),0.9, na.rm = TRUE)
  
  # PI width using validation set
  yhat_val = model$b0 + Xval%*%model$b
  resids_val = yval - yhat_val # Residuals
  q_val = quantile(abs(resids_val),0.9, na.rm = TRUE)
  
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
  
  var_list = c("total_bl")
  
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


### Step 5: Fit Model with Selected Coefficients from Lasso ###

# subset Xtest to just include variables selected by lasso and remove collinear variables
Xtest_subset <- Xtest[ ,colnames(Xtest) %in% non_zero_lasso$var & !colnames(Xtest) %in% c("l_name_Athletic_Exhibition.7bef", 
                                                                          "age_65up", 
                                                                          "female")]

# linear regresion
lm_subset <- glm(ytest ~ Xtest_subset, family = gaussian())
summary(lm_subset)

# poisson regression
poisson_subset <- glm(ytest ~ Xtest_subset, family = poisson())
summary(poisson_subset)

# Export linear regression results to csv
coef <- summary(lm_subset)$coefficients[,1]
std <- summary(lm_subset)$coefficients[,2]
t <- summary(lm_subset)$coefficients[,3]
p <- summary(lm_subset)$coefficients[,4]
lm_out <- cbind(coef, std, t, p)
write.csv(lm_out, 'lm_result.csv')


### Step 6: Significance Tests

# Test Joint Significance 

### ALL VARS ###
linearHypothesis(lm_subset, c("Xtest_subsettotal_bl.1bef", "Xtest_subsettotal_bl.2bef", 
                              "Xtest_subsettotal_bl.3bef", "Xtest_subsettotal_bl.4bef", 
                              "Xtest_subsettotal_bl.5bef", "Xtest_subsettotal_bl.6bef",
                              "Xtest_subsettotal_bl.7bef", "Xtest_subsettotal_bl.8bef",
                              "Xtest_subsettotal_bl.9bef", "Xtest_subsettotal_bl.10bef",
                              "Xtest_subsettotal_bl.11bef", "Xtest_subsettotal_bl.12bef"))

### Significant w/out Bonferroni Correction ###
linearHypothesis(lm_subset, c("Xtest_subsettotal_bl.1bef",  "Xtest_subsettotal_bl.4bef", 
                              "Xtest_subsettotal_bl.6bef",
                              "Xtest_subsettotal_bl.7bef", "Xtest_subsettotal_bl.8bef",
                              "Xtest_subsettotal_bl.10bef"))

# Test pairwise difference of means
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.4bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.6bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.7bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.8bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.9bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.1bef - Xtest_subsettotal_bl.10bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.4bef - Xtest_subsettotal_bl.6bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.4bef - Xtest_subsettotal_bl.7bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.4bef - Xtest_subsettotal_bl.8bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.4bef - Xtest_subsettotal_bl.9bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.4bef - Xtest_subsettotal_bl.10bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.6bef - Xtest_subsettotal_bl.7bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.6bef - Xtest_subsettotal_bl.8bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.6bef - Xtest_subsettotal_bl.9bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.6bef - Xtest_subsettotal_bl.10bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.7bef - Xtest_subsettotal_bl.8bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.7bef - Xtest_subsettotal_bl.9bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.7bef - Xtest_subsettotal_bl.10bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.8bef - Xtest_subsettotal_bl.9bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.8bef - Xtest_subsettotal_bl.10bef")
linearHypothesis(lm_subset, "Xtest_subsettotal_bl.9bef - Xtest_subsettotal_bl.10bef")


# Test joint significance of bl variables in each month lag
#### ALL VARS ####
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Housing:_Transient.1bef", "Xtest_subsetl_name_Consumer_Goods_(Auto_Repair).1bef", 
                              "Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).1bef", "Xtest_subsetl_name_Grocery_Store.1bef", 
                              "Xtest_subsetl_name_Motor_Vehicle_Dealer.1bef", "Xtest_subsetl_name_Pet_Shop.1bef",
                              "Xtest_subsetl_name_Security_Agent_(Person).1bef", "Xtest_subsetl_name_Swimming_Pool.1bef",
                              "Xtest_subsetl_name_Valet_Parking.1bef"))

#### ALL VARS ####
linearHypothesis(lm_subset, c("Xtest_subsetl_name_Bed_and_Breakfast.3bef",
                             "Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).3bef",
                             "Xtest_subsetl_name_General_Business_Licenses.3bef",
                             "Xtest_subsetl_name_Hotel.3bef",
                             "Xtest_subsetl_name_Security_Agency_(Firm).3bef"))

#### ALL VARS ####
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Housing:_Transient.4bef",
                              "Xtest_subsetl_name_Inn_And_Motel.4bef",
                              "Xtest_subsetl_name_Pesticide_Applicator.4bef",
                              "Xtest_subsetl_name_Security_Agency_(Firm).4bef",
                              "Xtest_subsetl_name_Security_Alarm_Dealer.4bef"))

#### ALL VARS ####
linearHypothesis(lm_subset, c("Xtest_subsetl_name_Consumer_Goods_(Auto_Repair).6bef",
                              "Xtest_subsetl_name_Cooperative_Association.6bef",
                              "Xtest_subsetl_name_Grocery_Store.6bef",
                              "Xtest_subsetl_name_Pet_Shop.6bef",
                              "Xtest_subsetl_name_Public_Hall.6bef",
                              "Xtest_subsetl_name_Security_Alarm_Dealer.6bef",
                              "Xtest_subsetl_name_Valet_Parking.6bef"))

#### ALL VARS ####
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Housing:_Transient.7bef",
                              "Xtest_subsetl_name_Auto_Wash.7bef",
                              "Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).7bef",
                              "Xtest_subsetl_name_Hotel.7bef",
                              "Xtest_subsetl_name_Inn_And_Motel.7bef",
                              "Xtest_subsetl_name_Pet_Shop.7bef",
                              "Xtest_subsetl_name_Security_Agency_(Firm).7bef",
                              "Xtest_subsetl_name_Security_Alarm_Agent.7bef",
                              "Xtest_subsetl_name_Valet_Parking.7bef"))

#### Significant Vars - Bonferroni ####
linearHypothesis(lm_subset, c("Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).7bef",
                              "Xtest_subsetl_name_Security_Agency_(Firm).7bef",
                              "Xtest_subsetl_name_Valet_Parking.7bef"))

### ALL VARS ###
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Housing:_Transient.8bef",
                              "Xtest_subsetl_name_Dry_Cleaners.8bef",
                              "Xtest_subsetl_name_General_Business_Licenses.8bef",
                              "Xtest_subsetl_name_Hotel.8bef",
                              "Xtest_subsetl_name_Pet_Shop.8bef",
                              "Xtest_subsetl_name_Restaurant.8bef",
                              "Xtest_subsetl_name_Security_Agency_(Firm).8bef",
                              "Xtest_subsetl_name_Security_Alarm_Agent.8bef",
                              "Xtest_subsetl_name_Security_Alarm_Dealer.8bef",
                              "Xtest_subsetl_name_Swimming_Pool.8bef"))
### ALL VARS ###
linearHypothesis(lm_subset, c("Xtest_subsetl_name_Caterers.9bef",
                              "Xtest_subsetl_name_Cooperative_Association.9bef",
                              "Xtest_subsetl_name_Dry_Cleaners.9bef",
                              "Xtest_subsetl_name_Gen_Contr-Construction_Mngr.9bef",
                              "Xtest_subsetl_name_Hotel.9bef",
                              "Xtest_subsetl_name_Public_Hall.9bef",
                              "Xtest_subsetl_name_Solid_Waste_Vehicle.9bef",
                              "Xtest_subsetl_name_Swimming_Pool.9bef",
                              "Xtest_subsetl_name_Valet_Parking.9bef"))

### ALL VARS ###
linearHypothesis(lm_subset, c("Xtest_subsetl_name_Mechanical_Amusement_Machine.10bef",
                              "Xtest_subsetl_name_Pet_Shop.10bef",
                              "Xtest_subsetl_name_Public_Hall.10bef",
                              "Xtest_subsetl_name_Security_Agency_(Firm).10bef",
                              "Xtest_subsetl_name_Security_Alarm_Agent.10bef"))
### ALL VARS ###
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Entertainment.11bef",
                              "Xtest_subsetl_name_Pesticide_Applicator.11bef",
                              "Xtest_subsetl_name_Public_Hall.11bef",
                              "Xtest_subsetl_name_Special_Events.11bef"))

### Significant w/ Bonferroni Adjustment ###
linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Entertainment.11bef",
                              "Xtest_subsetl_name_Special_Events.11bef"))


linearHypothesis(lm_subset, c("Xtest_subsetl_cat_Employment_Services.12bef",
                              "Xtest_subsetl_name_Barber_Chair.12bef",
                              "Xtest_subsetl_name_Health_Spa_Sales.12bef",
                              "Xtest_subsetl_name_Hotel.12bef",
                              "Xtest_subsetl_name_Motion_Picture_Theatre.12bef",
                              "Xtest_subsetl_name_Parking_Facility.12bef",
                              "Xtest_subsetl_name_Special_Events.12bef"))  

linearHypothesis(lm_subset, c("thresh3.6bef",
                              "thresh4.6bef"))


### Pairwise tests of significant variables in a given lag

#### Significant Vars - Bonferroni ####
linearHypothesis(lm_subset, "Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).7bef = Xtest_subsetl_name_Security_Agency_(Firm).7bef")
linearHypothesis(lm_subset, "Xtest_subsetl_name_Consumer_Goods_(Elect_Repair).7bef = Xtest_subsetl_name_Valet_Parking.7bef")
linearHypothesis(lm_subset, "Xtest_subsetl_name_Security_Agency_(Firm).7bef = Xtest_subsetl_name_Valet_Parking.7bef")
linearHypothesis(lm_subset, "Xtest_subsetl_cat_Entertainment.11bef = Xtest_subsetl_name_Special_Events.11bef")
                          








