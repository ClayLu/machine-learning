library('dplyr')
library(rpart)
library(randomForest)

train <- read.csv('C:/R_titanic/train.csv', stringsAsFactors = F)
test  <- read.csv('C:/R_titanic/test.csv', stringsAsFactors = F)

#Combined data 
full  <- bind_rows(train, test)

#############data engineering#############
summary(full)
#Now we know Age, Fare has NA's

#Replace missing Fare cells with median of same (Pclass ,Embarked) group
full$Fare[which(is.na(full$Fare))] <-  median(full[full$Pclass == '3' & full$Embarked == 'S', ]$Fare, na.rm = TRUE)

#add variable: Title ,it uses Regular Expression to clean data.
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer' ,'Mlle' ,'Ms','Mme')
full$Title[full$Title %in% rare_title]  <- 'Rare Title'
table(full$Sex, full$Title)

#Find string type field for empty strings
for(x in c(1:ncol(full)) ){  
  if(is.character(full[,x])){
    if( (length(which(nchar(full[,x]) == 0)) > 0)   ){
      print(colnames(full)[x])
    }
  }  
}

#add variable: Fsize¡BFsizeD , We can collapse Fsize into three levels: singleton¡Bsmall¡Blarge
full$Fsize <- full$SibSp + full$Parch + 1
counts <- table(full$Survived, full$Fsize)
#barplot(counts , beside=TRUE)
full$FsizeD[full$Fsize == 1] <- 'singleton'
full$FsizeD[full$Fsize < 5 & full$Fsize > 1] <- 'small'
full$FsizeD[full$Fsize > 4] <- 'large'

#Use distribution of Boxplot with Fare and Embarked to estimate/Replace empty string in the Embarked feature.
full$Embarked <- factor(full$Embarked)
summary(full$Embarked)
#boxplot(Fare~Embarked,data=full)
#Replace with C¡Ait has more probability
full$Embarked[which( factor(full$Embarked) == '' )] <- "C"
full$Embarked <- factor(full$Embarked)

#Replace missing Age cells with model:CART
predicted_age <- rpart(Age ~ Pclass + Sex + SibSp + Parch + Fare + Embarked + Title + FsizeD,   data = full[!is.na(full$Age),], method = "anova")
full$Age[is.na(full$Age)] <- predict(predicted_age, full[is.na(full$Age),])

#full$Child <- NA
#full$Child[full$Age < 18] <- 'child'
#full$Child[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'mother'
#full$Child[is.na(full$Child)] <- 'adult'

#add variable: Child
full$Child[full$Age < 18] <- 'Child'
full$Child[full$Age >= 18] <- 'Adult'

#add variable: Mother , it has 4 rules: 1.female 2.Parch > 0 3.Age > 18 4.!= 'Miss'
full$Mother <- 'Not Mother'
full$Mother[full$Sex == 'female' & full$Parch > 0 & full$Age > 18 & full$Title != 'Miss'] <- 'Mother'

full$Child  <- factor(full$Child)
full$Mother <- factor(full$Mother)

#str(full)

factor_vars <- c('Pclass','Sex','Title','FsizeD','Child')
full[factor_vars] <- lapply(full[factor_vars], function(x) as.factor(x))

#add variable: FamilyId2 , it is made up of Surname and Fsize. And then regrouup level'Small'.
Surname<-sapply(full$Name,function(x) strsplit(x,'[.,]')[[1]][1])
FamilyId<-paste0(full$Fsize,Surname)
full$FamilyId<-factor(FamilyId)
Family<-data.frame(table(FamilyId))
SmallFamily<-Family$FamilyId[Family$Freq<=2]
FamilyId[FamilyId %in% SmallFamily]<-'Small'
full$FamilyId2<-factor(FamilyId)

#Divide data into train and test
train <- full[1:891,]
test <- full[892:1309,]

#############predict#############
library(party)
#use cforest (conditional inference tree) method ,it has the higgest score: 0.80861.
fit<-cforest(Survived~Pclass+Sex+Age+SibSp+Parch+Fare+Embarked+Title+FsizeD + Child + Mother + FamilyId2,data=train,controls=cforest_unbiased(ntree=20000, mtry=3))
prediction <- predict(fit,test,OOB=TRUE,type='response')
p1 <- prediction
p1[prediction >= 0.5] <- 1
p1[prediction < 0.5] <- 0
solution <- data.frame(PassengerID = test$PassengerId, Survived = p1)

#use randomForest model,but it is lower then cforest.
#rf_model <- randomForest(factor(Survived) ~ Pclass + Sex + Age + SibSp + Parch +  Fare + Embarked + Title  + FsizeD + Child ,data = train  , importance = TRUE , ntree = 1000)
#prediction <- predict(rf_model, test)
#solution <- data.frame(PassengerID = test$PassengerId, Survived = prediction)
write.csv(solution, file = 'C:/R_titanic/rf_mod_Solution.csv', row.names = F)
