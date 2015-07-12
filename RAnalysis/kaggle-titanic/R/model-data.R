#' Splitting training data.
#' @param df Data frame.
#' @return The rows.

split.data.rows <- function(df){
    set.seed(1024);
    rows = createDataPartition(df$Survived, p = 0.8, list=FALSE);
    return (rows);
}

#' Default formula
#' @return The formula
get.default.formula <- function(){
    return (Survived~Sex+Pclass+Title2+FamilySize+IsNoble+Embarked+Age+IsNaCabin+IsPoorOld+IsRichGirl+Fare2);
}

#' Get control function for train function.
#' @return The control function.
get.control <- function(){
    cv.ctrl = trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           summaryFunction = twoClassSummary,
                           classProbs = TRUE);
    return (cv.ctrl);
}

#' Get gbm tune grid.
#' @return The tune grid.
get.gbm.grid <- function(){
    gbm.grid = expand.grid(n.trees = c(50, 100, 150, 200),
                          shrinkage = c(0.1, 0.001, 0.0005),
                          n.minobsinnode = c(5, 10, 15),
                          interaction.depth = c(1, 3, 5));
    return (gbm.grid);
}

#' Get random forest tune grid.
#' @return The tune grid.
get.rf.grid <- function(){
    rf.grid = data.frame(.mtry = c(2,3));
    return (rf.grid);
}


#' Get ada tune grid.
#' @return The tune grid.
get.ada.grid <- function(){
    ada.grid = expand.grid(.iter = c(50, 100),
                           .maxdepth = c(4, 8),
                           .nu = c(0.1, 1));
    return (ada.grid);
}

#' Get net tune grid.
#' @return The tune grid.
get.net.grid <- function(){
    net.grid = expand.grid(.decay = c(0.5, 0.1), .size = c(3, 6, 9));
    return (net.grid);
}

#' Simple generalized linear model
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
simple.glm.train <- function(formula, df){
    set.seed(35);
    model = glm(formula, data = df, family = binomial("logit"));
    return (model);
}


#' Generalized linear model
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
glm.train <- function(formula, df){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "glm",
                  metric = "ROC",
                  trControl = get.control());
#     model = glm(formula, data = df, family = binomial("logit"));
    return (model);
}


#' Gradient boosting training.
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
gbm.train <- function(formula, df, tune.grid = NULL){
    set.seed(35);
    if (is.null(tune.grid))
    {
        model = train(formula,
                      data = df,
                      method = "gbm",
                      metric = "ROC",
                      trControl = get.control());
    } else {
        model = train(formula,
                      data = df,
                      method = "gbm",
                      metric = "ROC",
                      trControl = get.control(),
                      tuneGrid = tune.grid);
    }
    return (model);
}

#' Random forest training.
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
rf.train <- function(formula, df){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "rf",
                  metric = "ROC",
                  trControl = get.control(),
                  tuneGrid = get.rf.grid());
    
}

#' Support vector machine model training.
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
svm.train <- function(formula, df){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "svmRadial",
                  tuneLength = 9,
                  metric = "ROC",
                  trControl = get.control(),
                  preProcess = c("center", "scale"));
    
}

#' Ada model training.
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
ada.train <- function(formula, df){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "ada",
                  metric = "ROC",
                  tuneGrid = get.ada.grid(),
                  trControl = get.control());
    return (model);
}

#' Neural network model training.
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
net.train <- function(formula, df){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "nnet",
                  metric = "ROC",
                  tuneGrid = get.net.grid(),
                  trControl = get.control());
    return (model);
}

#' Predict training data.
#' @param tune The tuned model.
#' @param df The data frame.
#' @return The prediction.
train.predict <- function(tune, df){
    pred = predict(tune, df);
    return (pred);
}

#' Get train data accuracy  
#' @param tune The tuned model.
#' @param df The data frame.
train.accuracy <- function(tune, df){
    confusionMatrix(train.predict(tune, df), df$Survived);
}

#' Predict testing data.
#' @param tune The tuned model.
#' @param df The data frame.
#' @return The prediction.
test.predict <- function(tune, df, fileName = NULL){
    Survived = predict(tune, df);
    Survived = revalue(Survived, c("Survive" = "1", "Perish" = "0"));
    pred = as.data.frame(Survived);
    pred$PassengerId = df$PassengerId;
    fileName= sprintf("%s.csv", format(Sys.time(), "%Y%m%d%H%M%S"));
    fileName = sprintf("export/%s", fileName);
    pred = pred[,c("PassengerId","Survived")];
    write.csv(pred,
              file = fileName, 
              row.names = FALSE,
              quote = FALSE);
    return (pred);
}

#' Get repeated cross-validation of the model.
#' @param tune The tuned model.
#' @param df The data frame.
#' @return The roc.
get.roc <- function(tune, df){
    model.probs = predict(tune, df, type = "prob");
    model.roc = roc(response = df$Survived,
                    predictor = model.probs$Survive,
                    levels = levels(df$Survived));
    return (model.roc);
}