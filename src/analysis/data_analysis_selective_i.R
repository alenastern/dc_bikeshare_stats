

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
library(glmnet)
library(grpreg)

# Set Working Directory

#setwd("/Users/alenastern/Documents/Win2019/MultiTesting/dc_bikeshare_stats/")
setwd('/mnt/dm-3/alix/Documents/Multiple Testing/dc_bikeshare_stats/')
source("src/exploration/get_data.R")
source("src/exploration/data_timelags.R")


# Selective inference - referencing Rina Barber's Inference on linear regression code ("2/27/2019")
# Step 0: Pre-processing for*

#### Code to get X, Y and betas.
turn_into_matrix <- function(data, y_var){
  # data = data frame
  # y_var = dependent variable
    X <- data.matrix(data[ , ! colnames(data) %in% c(y_var) ])
    Y <- data.matrix(data[[y_var]])

  df_list = list(X, Y)
  return(df_list)
}


#### Running Lasso with cross-validation
  df_list <- turn_into_matrix(df.no.dups, "n_rides_tot")
  X <- df_list[[1]]
  Y <- df_list[[2]]

  lambda = cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = "deviance", alpha = 1, n = 10)$lambda.min
  si_lasso = glmnet(x = X, y = Y,  alpha = 1, lambda = lambda)
  coef_lasso = coef(si_lasso)
  lasso = list(name = "coef_lasso", b0 = coef_lasso[1], b = coef_lasso[-1])
  n = nrow(df.no.dups)
  p = ncol(df.no.dups)
  #  betahat = coef(si_lasso, s=lambda/n)[-1]
  #  beta_hat  = coef(gfit, s=lambda/n, exact=TRUE)
  si_lasso = glmnet(x = X, y = Y,  alpha = 1, lambda = lambda/n)
  betahat = coef(si_lasso)[-1]
  betahat =  as.numeric(glmnet(X,Y,lambda = lambda/n, alpha = 1,intercept=FALSE)$beta) # Is this debiassed version?

    #### Selective inference
library(selectiveInference)
out = fixedLassoInf(X,Y,betahat,lambda,sigma=1)
plot(0:1,0:1,type='n',xlim=c(1,p),ylim=range(beta)+c(-1,1),xlab='j',ylab='beta')
points(betahat,pch=20,col='blue')
for(i in 1:length(out$vars)){
  j = out$vars[i]
  segments(j,out$ci[i,1],j,out$ci[i,2],col='blue')
}
points(beta)
legend('topleft',legend=c('beta','lasso + selectiveinf'),pch=c(1,20,20),col=c('black','blue'))
