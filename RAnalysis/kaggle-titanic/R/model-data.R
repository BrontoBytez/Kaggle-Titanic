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
    return (Survived~Age+Sex+Pclass+Title+Embarked+Fare+IsChild+FamilySize+IsRichGirl+IsPoorOld);
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
    gbm.grid = expand.grid(n.trees = 80:200,
                          shrinkage = seq(0,1,0.05),
                          n.minobsinnode = 8:15,
                          interaction.depth = 1:5);
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

#' Generalized linear model
#' @param formula The dependence formula.
#' @param df The data frame.
#' @return The model.
glm.train <- function(formula, df, tune.grid = NULL){
    set.seed(35);
    model = train(formula,
                  data = df,
                  method = "glm",
                  metric = "ROC",
                  trControl = get.control());
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