library(ggplot2)

models_feats <-  c("abstellplatz", "soziodaten_arbeitslosenquote" , "soziodaten_bevoelkerungsdichte" , "soziodaten_pkw_je_einwohnerzahl"                   
                   , "abw_halter"     , "bundesland"          , "diff_nutzeralter_kasko"         
                   , "fahrzeugalter"  , "fahrzeugalter_erwerb", "freie_werkstattwahl"            
                   , "gap_deckung"    , "jahresfahrleistung"  , "kennzeichentyp"                 
                   , "laufzeit"       , "nutzergruppe"        , "nutzungsart"                    
                   , "rabattschutz"   , "regionalklasse_vk"   , "TK" ,"VK"                
                   , "schutzbrief"    , "sf_klasse"           , "statistikjahr"                  
                   , "tarifgruppe"    , "tarifvariante"       , "typklasse_vk"                   
                   , "wohngebaeude"   , "zahlungsart"         , "zahlungsweise"    
                   , "schadenaufwand"  )


train_data_models <- train_data[, which(names(train_data) %in% models_feats)]
validation_data_models <- validation_data[, which(names(validation_data) %in% models_feats)]
test_data_models <- test_data[, which(names(test_data) %in% models_feats)]


calc_wmse <- function(actuals, predictions, weights) {
  sum(weights * (actuals - predictions)^2) / sum(weights)
}



# ------------------------------------------------------------------------------
# GLM Tweedie Model
# ------------------------------------------------------------------------------

library(tweedie)
library(statmod)
par(mar = c(5.1, 4.1, 4.1, 2.1))
options(warn = -1)

tweedie_profile_model <- tweedie.profile(schadenaufwand ~ ., data = train_data_models,
                                         xi.vec = seq(1.2, 1.6, by=0.1), weights = train_data$jahreseinheiten,
                                         do.plot=TRUE, link.power = 0)
summary(tweedie_profile_model)
plot(tweedie_profile_model, xlab = "Index Parameter" , ylab = "Profile Log-Likelihood")

fit_tweedie <- glm(schadenaufwand ~ ., family = tweedie(var.power = tweedie_profile_model$xi.max , link.power = 0),
                   data = train_data_models, weights = train_data$jahreseinheiten)

fit_tweedie_summary <- summary(fit_tweedie)
tweedie_coefs <- fit_tweedie_summary$coefficients
significant_coefs <- tweedie_coefs[tweedie_coefs[,4]< 0.05, ]
sorted_significant_coefs <- significant_coefs[order(significant_coefs[, 4]), ]
print(sorted_significant_coefs)

sorted_estimate_coefs <- round(significant_coefs[order(-(abs(significant_coefs[, 1]))), ],4)
print(sorted_estimate_coefs)

options(warn = 0)

# Errors
preds_tweedie_train <- exp(predict(fit_tweedie, newdata = train_data_models))
preds_tweedie_val <- exp(predict(fit_tweedie, newdata = validation_data_models))
preds_tweedie_test <- exp(predict(fit_tweedie, newdata = test_data_models))


tweedie_train_mse <- mean((preds_tweedie_train - train_data_models$schadenaufwand)^2) 
tweedie_val_mse <- mean((preds_tweedie_val - validation_data_models$schadenaufwand)^2) 
tweedie_test_mse <- mean((preds_tweedie_test - test_data_models$schadenaufwand)^2) 

tweedie_train_wmse <- calc_wmse(train_data_models$schadenaufwand, preds_tweedie_train, train_data$jahreseinheiten) 
tweedie_val_wmse <- calc_wmse(validation_data_models$schadenaufwand, preds_tweedie_val, validation_data$jahreseinheiten) 
tweedie_test_wmse <- calc_wmse(test_data_models$schadenaufwand, preds_tweedie_test, test_data$jahreseinheiten) 




# ------------------------------------------------------------------------------
# Decision Tree Model
# ------------------------------------------------------------------------------
library(rpart)


# Define a grid for tuning the hyper-parameters

tree_params_results <- data.frame(cp = numeric(), 
                                  minsplit = numeric(), 
                                  maxdepth = numeric(), 
                                  wmse_tree = numeric())

cp_values = seq(0 , 0.1, by = 0.001)
minsplit_values = c(50, 100)
maxdepth_values = c(15, 20, 30)

# Loop through all combinations
for(cp in cp_values) {
  for(minsplit in minsplit_values) {
    for(maxdepth in maxdepth_values) {
      
      fit_tree <- rpart(schadenaufwand ~ ., 
                        data = train_data_models,
                        weights = train_data$jahreseinheiten,
                        control = rpart.control(cp = cp, 
                                                minsplit = minsplit, 
                                                maxdepth = maxdepth))
      
      preds_tree <- predict(fit_tree, newdata = validation_data_models)
      wmse_tree <- calc_wmse(validation_data_models$schadenaufwand, preds_tree, validation_data$jahreseinheiten) 
      
      tree_params_results <- rbind(tree_params_results, c(cp, minsplit, maxdepth, wmse_tree))
    }
  }
}

colnames(tree_params_results) <- c("cp", "minsplit", "maxdepth", "WMSE")

# Parameters with minimum validation error
which.min(tree_params_results$WMSE) #3
tree_params_results[3,]

# Using Parameters for constructing the tree
control <- rpart.control(cp=0,minsplit=50, maxdepth=30)
tree_tuned_final <- rpart(schadenaufwand ~ ., 
                          data = train_data_models,
                          weights = train_data$jahreseinheiten, control=control)





# Errors

preds_tree_train <- predict(tree_tuned_final, newdata=train_data_models)
preds_tree_val <- predict(tree_tuned_final, newdata=validation_data_models)
preds_tree_test <- predict(tree_tuned_final, newdata=test_data_models)


trees_train_mse <- mean((preds_tree_train - train_data_models$schadenaufwand)^2) 
trees_val_mse <- mean((preds_tree_val - validation_data_models$schadenaufwand)^2) 
trees_test_mse <- mean((preds_tree_test - test_data_models$schadenaufwand)^2)

trees_train_wmse <- calc_wmse(train_data_models$schadenaufwand, preds_tree_train, train_data$jahreseinheiten) 
trees_val_wmse <- calc_wmse(validation_data_models$schadenaufwand, preds_tree_val, validation_data$jahreseinheiten) 
trees_test_wmse <- calc_wmse(test_data_models$schadenaufwand, preds_tree_test, test_data$jahreseinheiten) 





residuals_tree <- data.frame(residuals_tree =train_data_models$schadenaufwand - preds_tree_train)
prediction_tree <- data.frame(predict_tree = preds_tree_train)





# ------------------------------------------------------------------------------
# Random Forest Model
# ------------------------------------------------------------------------------

library(randomForestSRC)

mtry_range = seq(from = 1, to = floor(ncol(train_data_models)/2), length.out = 5)
nodesize_range = c(5, 10, 20, 50)

rf_tuning <- tune(schadenaufwand ~ ., data = train_data_models, 
                  nsplit = 10, # Number of cross-validation splits
                  ntree = 100, # Number of trees during tuning
                  grid = list(mtry = mtry_range,
                              nodesize = nodesize_range))
rf_tuning$optimal
# nodesize= 85   mtry = 6

rfsrc_tuned <- rfsrc(schadenaufwand ~ ., data = train_data_models, 
                     ntree=100, mtry=6, nodesize = 85,case.wt = train_data$jahreseinheiten, importance = TRUE)

print(rfsrc_tuned)


# Errors

preds_rf_train <- predict(rfsrc_tuned, newdata=train_data_models)
preds_rf_val <- predict(rfsrc_tuned, newdata=validation_data_models)
preds_rf_test <- predict(rfsrc_tuned, newdata=test_data_models)


rf_train_mse <- mean((preds_rf_train$predicted- train_data_models$schadenaufwand)^2) #460212.9
rf_val_mse <- mean((preds_rf_val$predicted - validation_data_models$schadenaufwand)^2) #507248.8
rf_test_mse <- mean((preds_rf_test$predicted - test_data_models$schadenaufwand)^2)


rf_train_wmse <- calc_wmse(train_data_models$schadenaufwand, preds_rf_train$predicted, train_data$jahreseinheiten) 
rf_val_wmse <- calc_wmse(validation_data_models$schadenaufwand, preds_rf_val$predicted, validation_data$jahreseinheiten) 
rf_test_wmse <- calc_wmse(test_data_models$schadenaufwand, preds_rf_test$predicted, test_data$jahreseinheiten) 








# ------------------------------------------------------------------------------
# XGBoost Tweedie Model
# ------------------------------------------------------------------------------


library(caret)
library(xgboost)
# Prepare data in xgb.DMatrix format

#define predictor and response variables in training set
train_x_xboost = data.matrix(train_data_models[, -which(names(train_data_models) == c("schadenaufwand"))])
train_y_xboost = train_data_models$schadenaufwand

#define predictor and response variables in validation testing set
val_x_xboost = data.matrix(validation_data_models[, -which(names(validation_data_models) == c("schadenaufwand"))])
val_y_xboost = validation_data_models$schadenaufwand

test_x_xboost = data.matrix(test_data_models[, -which(names(test_data_models) == c("schadenaufwand"))])
test_y_xboost = test_data_models$schadenaufwand

#define final training and testing sets
xgb_dtrain = xgb.DMatrix(data =train_x_xboost , label = train_y_xboost)
xgb_dval = xgb.DMatrix(data = val_x_xboost, label = val_y_xboost)
xgb_dtest = xgb.DMatrix(data = test_x_xboost, label = test_y_xboost)

setinfo(xgb_dtrain, "weight", train_data$jahreseinheiten)
setinfo(xgb_dval, "weight", validation_data$jahreseinheiten)
setinfo(xgb_dtest, "weight", test_data$jahreseinheiten)


# Hyperparameter grid
xgb_grid <- expand.grid(
  eta = c(0.01, 0.1, 0.3),
  max_depth = c(6, 10),
  subsample = c(0.5, 1.0),
  colsample_bytree = c(0.6, 0.8, 1.0),
  nrounds=100,
  gamma = c(0, 1),
  min_child_weight=1)


xgb_train <- train(
  schadenaufwand ~ ., data = train_data_models, 
  tuneGrid = xgb_grid,
  method = "xgbTree",
  trControl = trainControl(method = "cv", number = 5),
  metric = "RMSE"
)


# Display the row in the tuning grid that corresponds to the optimal model
xgb_optimal_params <- xgb_train$bestTune
print(xgb_optimal_params)

#nrounds = 100, max_depth = 6, eta = 0.1, gamma = 0,  colsample_bytree =0.6, min_child_weight = 1, subsample = 1


# best hyperparameters found during grid search
xgboost_params <- list(
  eta = xgb_optimal_params$eta,
  max_depth = xgb_optimal_params$max_depth,
  gamma = xgb_optimal_params$gamma,
  colsample_bytree = xgb_optimal_params$colsample_bytree,
  min_child_weight = xgb_optimal_params$min_child_weight,
  subsample = xgb_optimal_params$subsample,
  objective = "reg:squarederror"
)

# Create a list of the tuned hyperparameters (explicit)
xgboost_params <- list(
  eta = 0.1,
  max_depth = 6,
  gamma = 0,
  colsample_bytree = 0.6,
  min_child_weight = 1,
  subsample = 1,
  objective = "reg:squarederror"
)


# Initialize variables to keep track of the best model
best_rmse <- Inf 
best_nround <- NA 

# Loop over different early_stopping_rounds values
for (nround in c(10, 20, 30, 40, 50)) {
  
  set.seed(123)
  
  # Train the model
  best_xgb_model <- xgb.train(
    params = xgboost_params,
    data = xgb_dtrain,
    nrounds = 1000,
    watchlist = list(train = xgb_dtrain, val = xgb_dval),
    early_stopping_rounds = nround,
    maximize = FALSE,
    eval_metric = "rmse"
  )
  
  # Check if early stopping was triggered
  if (!is.null(best_xgb_model$best_iteration)) {
    
    # Get the RMSE at the best iteration
    current_rmse <- min(best_xgb_model$evaluation_log$val_rmse)
    
    # Check if this RMSE is better than the best so far
    if (current_rmse < best_rmse) {
      best_rmse <- current_rmse
      best_nround <- nround
    }
    
    print(paste("Stopped after", best_xgb_model$best_iteration, "with early_stopping_rounds =", nround, "and RMSE =", current_rmse))
  } else {
    print(paste("Early stopping was not triggered with early_stopping_rounds =", nround))
  }
}

# Printing the best early_stopping_rounds value
if (!is.na(best_nround)) {
  print(paste("Best early_stopping_rounds is", best_nround, "with RMSE =", best_rmse))
} else {
  print("No early stopping was triggered in any of the models.")
}

#[1] "Best early_stopping_rounds is 10 with RMSE = 702.165303678512"


# Construct the tuned xgboost model with the tuned params and the tuned early stopping rounds

# Set the seed for reproducibility
set.seed(123)

# Train the final model using the tuned hyperparameters
final_xgb_model <- xgb.train(
  params = xgboost_params,   # Tuned hyperparameters
  data = xgb_dtrain,          
  nrounds = 1000,             
  watchlist = list(train = xgb_dtrain, val = xgb_dval),  
  early_stopping_rounds = 10,  # Optimal early_stopping_rounds value
  maximize = FALSE,           
  eval_metric = "rmse"        
)

# Print the model
print(final_xgb_model)




preds_xgboost_train <- predict(final_xgb_model, newdata = xgb_dtrain)
preds_xgboost_val <- predict(final_xgb_model, newdata = xgb_dval)
preds_xgboost_test <- predict(final_xgb_model, newdata = xgb_dtest)


xgboost_train_mse <- mean((preds_xgboost_train - train_data_models$schadenaufwand)^2) 
xgboost_val_mse <- mean((preds_xgboost_val - validation_data_models$schadenaufwand)^2) 
xgboost_test_mse <- mean((preds_xgboost_test - test_data_models$schadenaufwand)^2) 


xgboost_train_wmse <- calc_wmse(train_data_models$schadenaufwand, preds_xgboost_train, train_data$jahreseinheiten) #429980
xgboost_val_wmse <- calc_wmse(validation_data_models$schadenaufwand, preds_xgboost_val, validation_data$jahreseinheiten) #486407.3
xgboost_test_wmse <- calc_wmse(test_data_models$schadenaufwand, preds_xgboost_test, test_data$jahreseinheiten) #515772.1




# ------------------------------------------------------------------------------
# MODEL COMPARISION
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# 1. Errors COMPARISION
# ------------------------------------------------------------------------------

# Create the data frame
error_df <- data.frame(
  Model = c("Tweedie", "Tree", "Random Forest", "XGBoost"),
  Training_MSE = c(tweedie_train_mse, trees_train_mse, rf_train_mse, xgboost_train_mse),
  Validation_MSE = c(tweedie_val_mse, trees_val_mse, rf_val_mse, xgboost_val_mse),
  Test_MSE = c(tweedie_test_mse, trees_test_mse, rf_test_mse, xgboost_test_mse)
)

error_weighted_df <- data.frame(
  Model = c("Tweedie", "Tree", "Random Forest", "XGBoost"),
  Training_WMSE = c(tweedie_train_wmse, trees_train_wmse, rf_train_wmse, xgboost_train_wmse),
  Validation_WMSE = c(tweedie_val_wmse, trees_val_wmse, rf_val_wmse, xgboost_val_wmse),
  Test_WMSE = c(tweedie_test_wmse, trees_test_wmse, rf_test_wmse, xgboost_test_wmse)
)

# View the data frame
print(error_df)
print(error_weighted_df)



# ------------------------------------------------------------------------------
# 2. Predictied VS actual claim amount
# ------------------------------------------------------------------------------



## Tweedie
plot(test_data_models$schadenaufwand, preds_tweedie_test, 
     xlab="Observed Claims Amount", ylab="Predicted Claims Amount")
abline(0, 1, col="red") # Adds
legend("topright", legend="Identity Line", col="red", lty=1)



## Tree
plot(test_data_models$schadenaufwand, preds_tree_test, 
     xlab="Observed Claims Amount", ylab="Predicted Claims Amount")
abline(0, 1, col="red") # Adds
legend("topright", legend="Identity Line", col="red", lty=1)


## RF
plot(test_data_models$schadenaufwand, preds_rf_test$predicted, 
     xlab="Observed Claims Amount", ylab="Predicted Claims Amount")
abline(0, 1, col="red") # Adds
legend("topright", legend="Identity Line", col="red", lty=1)



## XGBoost
plot(test_data_models$schadenaufwand, preds_xgboost_test, 
     xlab="Observed Claims Amount", ylab="Predicted Claims Amount")
abline(0, 1, col="red") # Adds
legend("topright", legend="Identity Line", col="red", lty=1)


# ------------------------------------------------------------------------------
# 3. Feature Importance 
# ------------------------------------------------------------------------------
library(ggplot2)
def_par<- par(5.1, 4.1, 4.1, 2.1)
## Tree
tree_importance <- data.frame(
  Feature = names(tree_tuned_final$variable.importance),
  Importance = tree_tuned_final$variable.importance
)


ggplot(tree_importance, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Features")
#ggtitle("Decision Tree Feature Importance")



## RF

# Extracting feature importance
rf_importance <- rfsrc_tuned$importance

# Convert it to a data.frame for ggplot
rf_importance_df <- data.frame(
  Feature = names(rf_importance),
  Importance = as.numeric(rf_importance)
)



ggplot(rf_importance_df, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Features")

#  ggtitle("Random Forest Feature Importance")


## XGBoost
xgb_dtrain = xgb.DMatrix(data =train_x_xboost , label = train_y_xboost)

xgb_importance_matrix <- xgb.importance(feature_names = colnames(xgb_dtrain), model = final_xgb_model)
xgb_importance_df <- as.data.frame(xgb_importance_matrix)
ggplot(xgb_importance_df, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  xlab("Features")+
  ylab("Importance")

#  ggtitle("XGBoost Feature Importance")



