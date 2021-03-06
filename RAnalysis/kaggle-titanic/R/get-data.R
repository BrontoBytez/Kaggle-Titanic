#' Generic read data from csv files.
#' @param file.name The file name
#' @param column.types The column types.
#' @return The data frame.
read.data <- function(file.name, column.types) {
    return (read.table( file = file.name,
              header = TRUE,
              sep = ",",
              quote = "\"",
              stringsAsFactors = FALSE,
              colClasses=column.types,
              na.strings=c("NA", "")));
    
}

#' Get title from names.
#' @param names A list of names
#' @return A list of titles.
get.title.from <- function(names){
    title = substr(names, regexpr(",", names)+2, regexpr("\\.", names)-1);
    title[which(title %in% c("the Countess", "Ms"))] = "Mrs";
    title[which(title %in% c("Mlle", "Mme"))] = "Miss";
#     title[which(title %in% c("Capt", "Col", "Don", "Dr",
#                               "Jonkheer", "Lady", "Major", 
#                               "Rev", "Sir" ))] = "Noble";
    title[-which(title %in% c("Master", "Miss", "Mr", "Mrs"))] = "Noble";
    title = as.factor(title);
    return (title);
}

#' Get title from names.
#' @param names A list of names
#' @return A list of titles.
get.title2.from <- function(names){
    title2 = substr(names, regexpr(",", names)+2, regexpr("\\.", names)-1);
    title2[which(title2 %in% c("Mme", "Mlle"))] = "Mlle";
    title2[which(title2 %in% c("Capt", "Don", "Major", "Sir", "Col"))] = "Sir";
    title2[which(title2 %in% c("Dona", "Lady", "the Countess", "Jonkheer"))] = "Lady";
    title2 = as.factor(title2);
    return (title2);
}

#' Get surname from names.
#' @param names A list of names
#' @return A list of surnames.
get.surname.from <- function(names){
    return (substr(names, rep(1, length(names)), regexpr(",", names)-1));
}

#' Get family size.
#' @param df The data frame.
#' @return List of family size.
get.familySize <- function(df){
    familySize = df$SibSp + df$Parch + 1;
    return (familySize);
}

#' Get family size 2.
#' @param df The data frame.
#' @return List of family size.
get.familySize2 <- function(df){
    familySize2 = df$SibSp + df$Parch + 1;
    familySize2[1:length(familySize2)] = "Small";
    familySize2[df$FamilySize >= 3 & df$FamilySize < 6] = "Medium";
    familySize2[df$FamilySize >= 6] = "Large";
    familySize2 = as.factor(familySize2)
    return (familySize2);
}

#' Get to whether the name contains bracket.
#' @param names The list of names.
#' @return List of booleans.
get.specialName <- function(names){
    specialName = grepl("\\(", names);
    return (specialName);
}

#' Get whether it is a child.
#' @param df The data frame.
#' @return List of boolean to determine if it is a child.
get.isChild <- function(df){
    isChild = df$Age < 18 & (df$SibSp > 0 || df$Parch > 0);
    return (isChild);
}

#' Get whether it is a young rich girl.
#' @param df The data frame.
#' @return List of boolean to determine if it is a young rich girl.
get.isRichGirl <- function(df){
    isRichGirl = df$Age < 30 & df$Sex == "female" & df$Pclass %in% c("1", "2");
    return (isRichGirl);
}

#' Get whether it is a poor old guy.
#' @param df The data frame.
#' @return List of boolean to determine if it is a poor old guy.
get.isPoorOld <- function(df){
    isPoorOld = df$Age > 40 & df$Pclass %in% c("3");
    return (isPoorOld);
}

#' Get whether it is a noble.
#' @param df The data frame.
#' @return List of boolean to determine if it is a noble.
get.isNoble <- function(df){
    isNoble = df$Title != "Mr" & df$Pclass %in% c("1", "2");
    return (isNoble);
}

#' Get whether the cabin is na.
#' @param df The data frame.
#' @return List of boolean to determine if the cabin is na.
get.isNaCabin <- function(df){
    IsNaCabin = is.na(df$Cabin);
    return (IsNaCabin);
}

#' Get engineered fare.
#' @param df The data frame.
#' @return List of grouped fare.
get.fare2 <- function(df){
    fare2 = df$Fare;
    fare2[1:length(fare2)] = 'more50';
    fare2[df$Fare < 50 & df$Fare >= 30] = 'f30t50';
    fare2[df$Fare < 30 & df$Fare >= 10] = 'f10t30';
    fare2[df$Fare < 10]  = 'less10';
    fare2 = as.factor(fare2);
    return (fare2);
}


#' Clean data frame "Embarked" information
#' @param The data frame.
#' @return The data frame.
clean.data.embarked <- function(df){
    df$Embarked[is.na(df$Embarked)] = "S";
    return (df);
}

#' Clean data frame "Fare" information
#' @param The data frame.
#' @return The data frame.
clean.data.fare <- function(df){
    df$Fare[df$Fare < 0.01] = 37.5;
    df$Fare[is.na(df$Fare)] = 37.5;
    return (df);
}

#' Clean data frame "Age" information
#' @param The data frame.
#' @return The data frame.
clean.data.age <- function(df){
    df$Age[is.na(df$Age) & df$Sex == "male"] = 29;
    df$Age[is.na(df$Age) & df$Sex == "female"] = 27;
    return (df);
}

#' Clean data frame "Survived" information
#' @param The data frame.
#' @return The data frame.
clean.data.survived <- function(df){
    df$Survived = revalue(df$Survived, c("1" = "Survive", "0" = "Perish"));
    return (df);
}

#' Clean data.
#' @return A cleaned data frame.
clean.data <-function(df){
    #df = ddply(df, .(Surname), transform, SurnameCount=length(Surname));
    df = df[with(df, order(df$PassengerId)),];
    rownames(df) = 1:nrow(df);
    df = clean.data.embarked(df);
    df = clean.data.fare(df);
    df = clean.data.age(df);
    df$Title = get.title.from(df$Name);
    df$Title2= get.title2.from(df$Name);
    df$Surname = get.surname.from(df$Name);
    df$FamilySize = get.familySize(df);
    df$FamilySize2 = get.familySize2(df);
    df$IsChild = get.isChild(df);
    df$SpecialName = get.specialName(df$Name);
    df$IsRichGirl = get.isRichGirl(df);
    df$IsPoorOld = get.isPoorOld(df);
    df$IsNoble = get.isNoble(df);
    df$IsNaCabin = get.isNaCabin(df);
    df$Fare2 = get.fare2(df);
    return (df);
}

#' Get test data.
#' @return Test data frame.
get.test.data <- function(){
    test.column.types <- c('integer',   # PassengerId
                            'factor',    # Pclass
                            'character', # Name
                            'factor',    # Sex
                            'numeric',   # Age
                            'integer',   # SibSp
                            'integer',   # Parch
                            'character', # Ticket
                            'numeric',   # Fare
                            'character', # Cabin
                            'factor'     # Embarked
    );    
    df = read.data("data/test.csv", test.column.types);
    return (clean.data(df));
}

#' Get train data.
#' @return Train data frame.
get.train.data <- function(){
    train.column.types <- c('integer',   # PassengerId
                            'factor',    # Survived 
                            'factor',    # Pclass
                            'character', # Name
                            'factor',    # Sex
                            'numeric',   # Age
                            'integer',   # SibSp
                            'integer',   # Parch
                            'character', # Ticket
                            'numeric',   # Fare
                            'character', # Cabin
                            'factor'     # Embarked
    );    
    df = read.data("data/train.csv", train.column.types);
    df = clean.data.survived(df);
    return (clean.data(df));
}