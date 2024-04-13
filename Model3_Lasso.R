## Tuned parameter by k-fold Cross-Validation 

X = data.frame(dat$weight, dat$height, dat$height*dat$weight, dat$heartrate_mean)
X = as.matrix(X)
Y = (dat$los)^(1/2)

set.seed(777)
lasso_mod = cv.glmnet(X, Y, alpha = 1, nfolds = 10) 

## Build a Lasso regression
set.seed(777)
bestlam = lasso_mod$lambda.min 

# Fit the final Lasso model using the best lambda value
final_lasso = glmnet(X, Y, alpha = 1, lambda = bestlam)

m1 = lm((los)^(1/2)~weight+height+weight*height+heartrate_mean,data=dat)

## Build a linear regression 
dat0 = data.frame(dat$los,dat$weight,dat$height,dat$heartrate_mean)
names(dat0) = c("los","weight","height","heartrate_mean")

m1 = lm((los)^(1/2)~weight+height+weight*height+heartrate_mean,data=dat)

#The linear model is built. It is fitted with the same features for comparison. 

lassobeta = data.frame(as.matrix(final_lasso$beta))
m1beta = data.frame(m1$coefficients)
names(m1beta) = c("ols_coefficients")
names(lassobeta) = c("lasso_coefficients")

kable(m1beta) %>% 
  kable_styling(position = "center", latex_options = "HOLD_position")
kable(lassobeta) %>% 
  kable_styling(position = "center", latex_options = "HOLD_position")


## Model evaluation using K-fold cross-validation
kfold = function(n){
  k = 10
  data = dat0
  
  set.seed(777) 
  folds = createFolds(data$los, k)
  dat = data[-folds[[n]], ]
  dtest = data[folds[[n]], ]
  
  set.seed(777) 
  m1 = lm((los)^(1/2)~weight+height+weight*height+heartrate_mean,data=dat)
  pred = predict(m1, newdata=dtest)
  res = data.frame(Id=folds[[n]], los=pred)
  return(sqrt(mean((res$los^2 - dtest$los)^2)))
}

cv_score = 0
for (i in 1:10){
  cv_score = kfold(i) + cv_score
}
cv_score/10
