## Identify the importance variables
set.seed(777)
fit = randomForest(los ~ ., data = dat, importance=TRUE)

imp=data.frame(importance(fit))
ord <- order(imp$X.IncMSE, decreasing=TRUE)
imp=imp[ord, ]
imp2 = imp[1:10,]
imp2
# The importance of each variable is measured by the percentage increase in the mean squared error (MSE) when that variable is removed from the model. According to the above table, `weight`, `height`, `admission_location`, and `heartrate_mean` show the significantly greater values in X.IncMSE. However, `hematocrit_min`, `hemoglobin_min`, `hematocrit_max`, `aniongap` have similar values in X.IncMSE but `aniongap` has a significant value in IncNodePurity.

# Therefore, `weight`, `height`, `heartrate_mean`, `aniongap`, `admission_location` are added to the random forests. 

##  Model matrix for hyperparameters 
X = model.matrix(~ weight + height + heartrate_mean + aniongap + admission_location, data=dat)
Y = dat$los
# The model matrix is built to run rfcv(), which is a cross validation function for random forests. 

hyperparams = expand.grid(mtry = c(15,5,3,1), ntree=c(1000), 
                          maxnodes=c(10,100,300), nodesize=c(1,20,50), sampsize=c(250,300,350))
hyperparams[1:10,]


# The ranges for each hyperparameter are randomly chosen. Those hyperparameters will be tuned by cross-validation based on hyperparagrams. A random forest is fitted with all the combinations of hyperparameters according to the hyperparams, then the hyperparameters with the least mean squared error will be fitted to the final model.

## Tuned hyperparameters by 10-fold Cross-Validation
result = data.frame()

for (i in 1:nrow(hyperparams)) {
  set.seed(777)
  model = rfcv(X, Y, cv.fold = 10, mtry = function(x){hyperparams$mtry[i]},
               ntree=hyperparams$ntree[i], maxnodes=hyperparams$maxnodes[i],
               nodesize=hyperparams$nodesize[i], sampsize=hyperparams$sampsize[i])
  tem = list(n.var = model$n.var, error.cv = model$error.cv)
  tem2 = c(mse = min(tem$error.cv), mtry = hyperparams$mtry[i],
           ntree=hyperparams$ntree[i],maxnodes=hyperparams$maxnodes[i],
           nodesize=hyperparams$nodesize[i], sampsize=hyperparams$sampsize[i])
  print(tem2)
  result = rbind(result, tem2)
}
colnames(result) = c('mse','mtry','ntree','maxnodes','nodesize','sampsize')
head(result)
optimalFeatures = c(result$mtry[which.min(result$mse)],result$ntree[which.min(result$mse)],
                    result$maxnodes[which.min(result$mse)],
                    result$nodesize[which.min(result$mse)],
                    result$sampsize[which.min(result$mse)])
optimalFeatures 


#The list of hyperparameters to be tuned :
# mtry = Number of variables randomly sampled as candidates at each split
# ntree = Number of trees to grow. This should not be set to too small a number, to ensure that every input row gets predicted at least a few times.
# maxnodes = Maximum number of terminal nodes trees in the forest can have. 
# nodesize = Minimum size of terminal nodes.
# sampsize = Size(s) of sample to draw.

# The ranges for each hyperparameter are randomly chosen, and those hyperparameters are tuned by 10-fold cross-validation. A random forest is fitted with all the combinations of hyperparameters according to the hyperparams, then the hyperparameters with the least mean squared error are chosen to the the final model.

## Optimized random forest 
rf = randomForest(los ~ weight + height + heartrate_mean + aniongap + admission_location, 
                  mtry = 3, ntree=1000, maxnodes= 300,
                  nodesize = 20, sampsize = 350,
                  data = dat, importance=TRUE)

## Model evaluation using K-fold cross-validation
kfold = function(n){
  data = dat
  k = 10
  predictors = names(data)[2:ncol(data)]
  response = names(data)[1]
  
  folds = createFolds(data$los, k)
  dat = data[-folds[[n]], ]
  dtest = data[folds[[n]], ]
  
  set.seed(777) 
  fit = randomForest(los ~ weight + height + heartrate_mean + aniongap + admission_location, 
                     mtry = 3, ntree=1000, maxnodes= 300,
                     nodesize = 20, sampsize = 350,
                     data = dat) # rf
  pred = predict(fit, newdata=dtest)
  res = data.frame(Id=folds[[n]], los=pred)
  return(sqrt(mean((res$los - dtest$los)^2)))
}

cv_score = 0
for (i in 1:10){
  cv_score = kfold(i) + cv_score
}
cat("According to the 10-fold cross-validation, 
    the MSE of an optimized randomForest is approximately ", cv_score/10)



###  Partial Dependence Plots (PDP)
par(mfrow=c(2,2))
partialPlot(rf,dat,"weight",cex.main=0.6,asp=0.5)
partialPlot(rf,dat,"height",cex.main=0.6)
partialPlot(rf,dat,"heartrate_mean",cex.main=0.6)
partialPlot(rf,dat,"aniongap",cex.main=0.6)


### First tree of Random forest
# extract a single tree
tree1 = getTree(rf, 1, labelVar=TRUE)

left_daughter = c(tree1$`left daughter`[1:20])
right_daughter = c(tree1$`right daughter`[1:20])
split_var = c(tree1$`split var`[1:20])
split_point = c(tree1$`split point`[1:20])
prediction = c(tree1$prediction[1:20])

pval=data.frame(left_daughter,right_daughter,split_var,split_point,prediction)
kable(pval) %>% kable_styling(position = "center", latex_options = "HOLD_position")


### Visualization
rf_model = rf

rpart_tree = rpart::rpart(as.formula(rf_model$call$formula), 
                          data = dat, 
                          control = rpart::rpart.control(cp = 0), 
                          model = FALSE)

# plot tree using rpart.plot
rpart.plot(rpart_tree, main = "Random Forest Tree Diagram")
