
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
  df_list <- turn_into_matrix(df.final.timelags, "n_rides_tot")
  X <- df_list[[1]]
  Y <- df_list[[2]]

  lambda = cv.glmnet(x = X, y = Y, lambda = NULL, type.measure = "deviance", alpha = 1, n = 10)$lambda.min
  si_lasso = glmnet(x = X, y = Y,  alpha = 1, lambda = lambda) 
  coef_lasso = coef(si_lasso)
  lasso = list(name = "coef_lasso", b0 = coef_lasso[1], b = coef_lasso[-1])
  betahat = coef(si_lasso, s=lambda/n)[-1] 
  
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
