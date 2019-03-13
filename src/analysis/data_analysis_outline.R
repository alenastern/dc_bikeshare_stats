
# Set Working Directory

setwd("~/Desktop/UChi/Classes/Stats/MultipleTesting_ModernInference/project_bikeshare/dc_bikeshare_stats/src/exploration/") #Cris' directory

source(here("src/exploration","get_data.R"))
source(here("src/exploration","data_timelags.R"))

### Step 1: Split Data into Training, Validation, Testing Sets ###

train_test_split <- function(data, y_var, bg){
  # data = data frame
  # y_var = dependent variable
  # bg = boolean, whether randomizing on block group
 
  # Case when not randomizing on block group 
  if(bg == TRUE) {
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
    
  # reduce(union, list(unique(as.vector(train_set$GEOID)), unique(as.vector(val_set$GEOID)), unique(as.vector(test_set$GEOID))))
  # reduce(intersect, list(unique(as.vector(train_set$GEOID)), unique(as.vector(val_set$GEOID)), unique(as.vector(test_set$GEOID))))
  # identify no GEOIDs in common and 85 in union

    Xtrain <- train_set[ , ! colnames(train_set) %in% c(y_var) ]
    #Xtrain <- train_set %>% select(-y_var)
    ytrain <- train_set[[y_var]]
    Xval <- val_set[ , ! colnames(val_set) %in% c(y_var) ]
    yval <- train_set[[y_var]]
    Xtest <- test_set[ , ! colnames(test_set) %in% c(y_var) ]
    ytest <- train_set[[y_var]]
  }
  df_list = list(Xtrain, ytrain, Xval, yval, Xtest, ytest)
  return(df_list) 
}
#Q: do we have enough data to do this?

### Step 2: Define All Variables to Use in Model and Appropriate Model ###

#Q: I think there are standard statistical tests that help you pick the right model (eg Poison, linear, etc.)
# Use training data to identify good model 

### Step 3: Use Linear regression, Lasso Regression, Ridge Regression, Elastic Net, and Forward Selection to Identify Subset of Variables ###

#Q: we'd have to rewrite code for forward selection

### Step 3a:LR
coef_lm = lm(ytrain~Xtrain)$coef 
lm = list(name = "coef_lm", b0 = coef_lm[1], b = coef_lm[-1])

### Step 3a:Lasso
lamb_ = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 1, n = 10)$lambda.min
trained_lasso = glmnet(x = Xtrain, y = ytrain,  alpha = 1, lambda = lamb_) 
coef_lasso = coef(trained_lasso)
lasso = list(name = "coef_lasso", b0 = coef_lasso[1], b = coef_lasso[-1])

### Step 3b: Ridge
lamb_ = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, type.measure = "deviance", alpha = 0, n = 10)$lambda.min
trained_ridge = glmnet(x = Xtrain, y = ytrain,  alpha = 0, lambda = lamb_) 
coef_ridge = coef(trained_ridge)
ridge = list(name = "coef_ridge", b0 = coef_ridge[1], b = coef_ridge[-1])

### Step 3c: Elastic Net

### Step 3d: Forward Selection

### Step 3e: Poisson

lamb_ = cv.glmnet(x = Xtrain, y = ytrain, lambda = NULL, family = "poisson", n = 10)$lambda.min
trained_poison = glmnet(Xtrain, ytrain, family = "poisson", lambda = lamb_)
coef_poisson = coef(trained_poison)
poisson = list(name = "coef_poiss", b0 = coef_poisson[1], b = coef_poisson[-1])

### Step 4: Assess Model on Test Set ###

### Step 4a: Assess accuracy in terms of minimizing RMSE 

### Step 4b: Calculate 90% quantile

### Step 4c: Calculate prediction interval and discuss width

### Step 4d: Assess coverage of prediction interval on test set

model_performance = data.frame()
index = 0
for (model in list(lm, lasso, ridge, poisson)){
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
  
} 


### Step 5: Local Hypothesis Testing ###

#Q: Not splitting data
### 5a: fit final model on each local hypothesis (each block group = one hypothesis)

### 5b: use false discovery 





