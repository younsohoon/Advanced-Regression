## Identify the significant variables
cor_df = data.frame()
name = c('weight * los','height * los', 'weight * height', 'heartrate_mean * los')
cor_ = c(cor(dat$weight,dat$los),cor(dat$height,dat$los),cor(dat$weight,dat$height),cor(dat$heartrate_mean,dat$los))
cor_df = data.frame(name,cor_)
colnames(cor_df) = c('Significant_terms','Correlation')

kable(cor_df) %>%   kable_styling(position = "center", latex_options = "HOLD_position")

# Significant variables are identified by correlation and scatter plots (scatter plots are drawn on 1-5). Since each correlation coefficient of `weight * los`,`height * los`, `weight * height`, `heartrate_mean * los` are greater than 0.35, they are significantly correlated. Hence, they are included in the following smoothing spline models. 

## Build smoothing spline models
ss1 = gam(los ~ s(weight) + s(height) + s(heartrate_mean), data = dat) #16.60
ss2 = gam(los ~ s(weight) + s(height) + s(heartrate_mean) + ti(weight, height), data = dat) # 16.44
ss3 = gam(los ~ s(weight) + s(height) + ti(weight, height), data = dat) # 16.5027
ss4 = gam(los ~ s(weight) + s(height), data = dat) #16.769
gam.check(ss2)

# For the basis function, cubic spline `s()` is utilized to specify the smoothness of the spline. The larger the value of s(), the smoother the curve will be. Also, `ti()` is used over `te()`, for interaction terms, since `ti()` specifies a single knot location for each term while `te()` specifies a separate knot location for each observation. This makes `ti()` more efficient than `te()`. 

# If effective degree of freedom (edf) is significantly less than k, k needs to decrease. For those four models, each edf are signifinatly less than each k, any k is not tuned.

kfold = function(n){
  k = 10
  data = dat
  predictors = names(data)[2:ncol(data)]
  response = names(data)[1]
  
  set.seed(777) 
  folds = createFolds(data$los, k)
  dat = data[-folds[[n]], ]
  dtest = data[folds[[n]], ]
  
  set.seed(777) 
  #fit = gam(los ~ s(weight) + s(height) + s(heartrate_mean), data = dat) #16.60 ss1
  fit = gam(los ~ s(weight) + s(height) + s(heartrate_mean) + ti(weight, height), 
            data = dat) # 16.44 ss2
  #fit = gam(los ~ s(weight) + s(height) + ti(weight, height), data = dat) # 16.5027 ss3
  # fit = gam(los ~ s(weight) + s(height), data = dat) #16.769 ss4
  pred = predict(fit, newdata=dtest)
  res = data.frame(Id=folds[[n]], los=pred)
  return(sqrt(mean((res$los - dtest$los)^2)))
}


cv_score = 0
for (i in 1:10){
  cv_score = kfold(i) + cv_score
}
cv_score/10


# Mean squared errors (MSE) by 10-fold crossvalidation is calculated as above code. The 10-fold cross-validation score is a measure of how well a model generalizes to new data. A lower score indicates better performance.

df_s1 = c('s(weight) + s(height) + s(heartrate_mean)','s(weight) + s(height) + s(heartrate_mean) + ti(weight, height)','s(weight) + s(height) + ti(weight, height)','s(weight) + s(height)')
df_s2 = c(16.60413,16.44116,16.5027,16.769)
df_ss = data.frame(df_s1,df_s2)
colnames(df_ss) = c('Model','MSE')

kable(df_ss) %>% kable_styling(position = "center", latex_options = "HOLD_position")

# According to the 10-fold crossvalidation, `los ~ gam(s(weight) + s(height) + s(heartrate_mean) + ti(weight, height)` has the least mean squared errors (MSE), which means it predicts the data better than other three. 


### Check model fit 
par(mfrow=c(2,2))
gam.check(ss)

### 3D plot
par(mfrow=c(1,2))
vis.gam(ss,view=c("height","weight"),type="response",phi=10,theta=80)
vis.gam(ss,view=c("height","weight"),type="response",phi=10,theta=170)

#3D plots are drawn with respect to `height` and `weight`. `los` decreases with respect to `weight` when `height` is significantly large. However, `los` increases with respect to `height` when `weight` is significantly large. Hence, `height` and `weight` are strongly correlated.
par(mfrow=c(1,2))
vis.gam(ss,view=c("height","weight"),type="response",phi=10,theta=270)
vis.gam(ss,view=c("height","weight"),type="response",phi=10,theta=340)

#When `height` is low, `los` is relatively constant with regardless of `weight`. When `weight` is low, `los` is very changeable with respect to `height`. To be specific, `los` significantly increases when holding `weight` in a low value.