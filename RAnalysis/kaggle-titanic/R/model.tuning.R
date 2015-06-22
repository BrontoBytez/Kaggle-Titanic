
get.formula.list <- function(){
    formula.list = c(Survived~Age+Sex+Pclass+Title+Embarked+Fare,
                     Survived~Age+Sex+Pclass+Title+Embarked+Fare+IsChild,
                     Survived~Age+Sex+Pclass+Title+Embarked+Fare+IsChild+FamilySize+IsRichGirl,
                     Survived~Age+Sex+Pclass+Title+Embarked+Fare+IsChild+FamilySize+IsRichGirl+IsPoorOld
                     );
    return (formula.list);
}


run.formula.list <- function(model, df.batch, df.unbatch){
    ret = lapply(get.formula.list(), 
                 FUN = function(f) {
                     tune = model(f, df.batch);
                     return (train.accuracy(tune, df.unbatch));
                    });
    return (ret);
}