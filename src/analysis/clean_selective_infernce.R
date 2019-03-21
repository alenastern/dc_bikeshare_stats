# ------------------------------------------------
# Selective Inference clean script
# ------------------------------------------------
# Following https://cran.r-project.org/web/packages/selectiveInference/selectiveInference.pdf & Rina Barber's "2/27/2019" code for selective inference. 
# Requirements: Run data_timelags.R before 

rm(X)
rm(Y)
# Getting X and Y 
turn_into_matrix <- function(data, y_var){
  
  X <- data.matrix(data[ , ! colnames(data) %in% c(y_var, 'GEOID') ])
  Y <- data.matrix(data[[y_var]])
  
  df_list = list(X, Y)
  return(df_list) 
  }


df_list <- turn_into_matrix(df.no.dups, "n_rides_tot")
X <- df_list[[1]]
Y <- df_list[[2]]
n = nrow(df.no.dups)
p = ncol(df.no.dups)

# Lambda = 1
rm(lambda)
lambda = 1
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l1 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 10 
rm(lambda)
lambda = 10
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l10 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 100 
rm(lambda)
lambda = 100
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l100 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 150
rm(lambda)
lambda = 150
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l150 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 200
rm(lambda)
lambda = 200 
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l200 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 250
rm(lambda)
lambda = 250
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l250 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 500
rm(lambda)
lambda = 500
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l500 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

# Lambda = 1000
rm(lambda)
lambda = 1000
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l1000 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)

rm(lambda)
lambda = 2000
betahat = as.numeric(glmnet(X,Y,lambda = lambda, alpha = 1, standardize=FALSE,intercept=FALSE)$beta)
out_l2000 = fixedLassoInf(X,Y,betahat,lambda/n,sigma=1)