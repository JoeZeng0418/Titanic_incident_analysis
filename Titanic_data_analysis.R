

# load data
trainData <- read.csv("train.csv", header = TRUE)
testData <- read.csv("test.csv", header = TRUE)

# add the survived column to the test data
test_survived <- data.frame(testData[,1], Survived = "None", testData[,2:(ncol(testData))])

# change the name of the first column
colnames(test_survived)[1] <- "PassengerId"

# combine train and test data
data_combined <- rbind(trainData, test_survived)

# make the type of Survived object a factor
# same for Pclass
data_combined$Survived <- as.factor(data_combined$Survived)
data_combined$Pclass <- as.factor(data_combined$Pclass)

# get the number for each actor
table(data_combined$Survived)
table(data_combined$Pclass)

#load library for plots (visualization)
library(ggplot2)

#plot a histogram with Pclass vs. Survival count and Death count
trainData$Survived <- as.factor(trainData$Survived)
ggplot(trainData, aes(x=Pclass, fill=factor(Survived))) +
  geom_histogram(binwidth=0.2)+
  xlab("Pclass")+
  ylab("Count")+
  labs(fill="Survived")

# look at the first few names
head(as.character(trainData$Name))

# the number of unique names
length(unique(as.character(data_combined$Name)))

# get the duplicated names in characters
dup_names <- as.character(data_combined[which(duplicated(as.character(data_combined$Name))),"Name"])
# take a look at the specific duplicated records
data_combined[which(data_combined$Name %in% dup_names),]


# examine the correlation between varables
library(stringr)
misses <- data_combined[which(str_detect(as.character(data_combined$Name),"Miss")),]
misses[1:5,]
males <- data_combined[which(data_combined$Sex=="male"),]
males[1:5,]

# extract title function
extractTitle <- function(name){
  if(str_detect(name, fixed("Mr."))){
    return("Mr.")
  } else if(str_detect(name, fixed("Miss."))){
    return("Miss.")
  } else if(str_detect(name, fixed("Mrs."))){
    return("Mrs.")
  } else if(str_detect(name, fixed("Rev."))){
    return("Rev.")
  } else if(str_detect(name, fixed("Master."))){
    return("Master.")
  } else {
    return("Other")
  }
}
titles <- NULL
for (i in 1:nrow(data_combined)) {
  titles <- c(titles, extractTitle(data_combined[i,"Name"]))
}
data_combined$Title <- as.factor(titles)
# plot the relationship among survival, pclass, and title
ggplot(data_combined[1:nrow(trainData),], aes(x = Title, fill=Survived))+
  geom_bar(width = 0.5)+ggtitle("Pclass")+facet_wrap(~Pclass)+
   xlab("Title")+ylab("Count")+labs(fill="Survived")

#plot the relationship among sex, pclass, and survival
ggplot(data_combined[1:nrow(trainData),], aes(x = Sex, fill=Survived))+
  geom_bar(width = 0.5)+ggtitle("Pclass")+facet_wrap(~Pclass)+
  xlab("Sex")+ylab("Count")+labs(fill="Survived")

summary(data_combined$Age)

#plot the relationship among sex, pclass, age and survival
ggplot(data_combined[1:nrow(trainData),], aes(x = Age, fill=Survived))+
  geom_histogram(binwidth = 10)+ggtitle("Pclass")+facet_wrap(~Sex+Pclass)+
  xlab("Age")+ylab("Count")+labs(fill="Survived")

# validate master as boys
boys <- data_combined[which(data_combined$Title=="Master."),]
summary(boys$Age)

misses <- data_combined[which(data_combined$Title=="Miss."),]
summary(misses$Age)

# survival vs. age of Miss by Pclass
ggplot(misses[misses$Survived!="None",], aes(x = Age, fill=Survived))+
  facet_wrap(~Pclass)+geom_histogram(binwidth = 5)+ggtitle("Age for 'Miss.' by Pclass")+xlab("Age")+ylab("count")

# female children situation
female_children = misses[which(misses$SibSp==0&misses$Parch==0),]
summary(female_children$Age)
length(which(female_children$Age<=14.5))

# examine sibsp varaible
summary(data_combined$SibSp)

# make the sibsp a factor
length(unique(data_combined$SibSp))
data_combined$SibSp <- as.factor(data_combined$SibSp)

ggplot(data_combined[1:nrow(trainData),], aes(x = SibSp, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("SibSp")+
  ylab("count")+
  ylim(0,300)+
  labs(fill="Survived")

# make the parch a factor
length(unique(data_combined$Parch))
data_combined$Parch <- as.factor(data_combined$Parch)

ggplot(data_combined[1:nrow(trainData),], aes(x = Parch, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("Parch")+
  ylab("count")+
  ylim(0,300)+
  labs(fill="Survived")

# add a family size feature
data_combined$FamilySize <- as.factor(c(trainData$SibSp,testData$SibSp)+c(trainData$Parch,testData$Parch)+1)
ggplot(data_combined[1:nrow(trainData),], aes(x = FamilySize, fill=Survived))+
  geom_bar()+
  facet_wrap(~Pclass+Title)+
  ggtitle("Pclass, Title")+
  xlab("FamilySize")+
  ylab("count")+
  ylim(0,300)+
  labs(fill="Survived")

# ticket variable
str(data_combined$Ticket)
data_combined$Ticket <- as.character(data_combined$Ticket)
str(data_combined$Ticket)
data_combined$Ticket[1:20]
# take a look at the first letter of Ticket variable
ticket_first_char <- ifelse(data_combined$Ticket==""," ",substr(data_combined$Ticket,1,1))
unique(ticket_first_char)
data_combined$TicketFirstChar <- as.factor(ticket_first_char)

ggplot(data_combined[1:nrow(trainData),],aes(x=TicketFirstChar, fill=Survived))+
  geom_bar()+
  labs(x="TicketFirstChar", y="Count", title="survival by ticket first char", fill="Survival")

ggplot(data_combined[1:nrow(trainData),],aes(x=TicketFirstChar, fill=Survived))+
  geom_bar()+facet_wrap(~Pclass)+ylim(0,150)+
  labs(x="TicketFirstChar", y="Count", title="survival by ticket first char in different Pclass", fill="Survival")

ggplot(data_combined[1:nrow(trainData),],aes(x=TicketFirstChar, fill=Survived))+
  geom_bar()+facet_wrap(~Pclass+Title)+ylim(0,200)+
  labs(x="TicketFirstChar", y="Count", title="survival by ticket first char in different Pclass&Title", fill="Survival")

# fare variable
summary(data_combined$Fare)
length(unique(data_combined$Fare))
ggplot(data_combined,aes(Fare))+
  geom_histogram(binwidth = 5,colour="black", fill="white")+
  labs(x="Fare",y="Count",title="Combined Fare Distribution")

ggplot(data_combined,aes(Fare))+
  geom_histogram(aes(y=..density..),      # Histogram with density instead of count on y-axis
                 binwidth=10,
                 colour="black", fill="white")+
  geom_density(alpha=.4, fill="#FF6666")+
  labs(x="Fare",y="Count",title="Combined Fare Distribution")

ggplot(data_combined[1:nrow(trainData),],aes(x=Fare,fill=Survived))+
  geom_histogram(binwidth = 5)+facet_wrap(~Pclass+Title)+ylim(0,50)+
  labs(x="Fare",y="Count",title="survival vs. fare in different pclass and title")

# cabin variable
str(data_combined$Cabin)
data_combined$Cabin <- as.character(data_combined$Cabin)
data_combined$Cabin[1:20]
data_combined$Cabin <- ifelse(data_combined$Cabin=="","U",data_combined$Cabin)
data_combined$Cabin[1:20]

cabin_first_char <- substr(data_combined$Cabin,1,1)
cabin_first_char <- as.factor(cabin_first_char)
levels(cabin_first_char)
data_combined$CabinFirstChar <- cabin_first_char

ggplot(data_combined[1:nrow(trainData),],aes(x = CabinFirstChar,fill=Survived))+
  labs(x="CabinFirstChar",y="Count",title="Survival vs. CabinFirstChar in different Pclass",fill="Survived")+
  geom_bar()+facet_wrap(~Pclass)

ggplot(data_combined[1:nrow(trainData),],aes(x = CabinFirstChar,fill=Survived))+
  labs(x="CabinFirstChar",y="Count",title="Survival vs. CabinFirstChar in different Pclass and title",fill="Survived")+
  geom_bar()+facet_wrap(~Pclass+Title)
# what about people in multiple cabins
data_combined$MultipleCabin <- as.factor(ifelse(str_detect(data_combined$Cabin," "),"Y","N"))
ggplot(data_combined[1:nrow(trainData),],aes(x = MultipleCabin,fill=Survived))+
  labs(x="MultipleCabin",y="Count",title="Survival vs. HasMultipleCabin in different Pclass and title",fill="Survived")+
  geom_bar()+facet_wrap(~Pclass+Title)

# ********************************
# exploratory modeling
# ********************************

 library(randomForest)

# train a random forest with pclass and title variables
rf_train_1 <- data_combined[1:nrow(trainData),c("Pclass","Title")]
rf_train_label <- as.factor(trainData$Survived)
set.seed(1234)
rf_1 <- randomForest(x=rf_train_1, y=rf_train_label, importance = TRUE, ntree = 1000)
rf_1
varImpPlot(rf_1)

# train a random forest with pclass, title, and sibsp variables
rf_train_2 <- rf_train_1
rf_train_2$SibSp <- as.factor(trainData$SibSp)
set.seed(1234)
rf_2 <- randomForest(x=rf_train_2, y=rf_train_label, importance = TRUE, ntree = 1000)
rf_2
varImpPlot(rf_2)

# train a random forest with pclass, title, and parch variables
rf_train_3 <- rf_train_1
rf_train_3$SibSp <- as.factor(trainData$Parch)
set.seed(1234)
rf_3 <- randomForest(x=rf_train_3, y=rf_train_label, importance = TRUE, ntree = 1000)
rf_3
varImpPlot(rf_3)

# train a random forest with pclass, title, sibsp, parch variables
rf_train_4 <- data_combined[1:nrow(trainData),c("Pclass","Title","SibSp","Parch")]
set.seed(1234)
rf_4 <- randomForest(x=rf_train_4, y=rf_train_label, importance = TRUE, ntree = 1000)
rf_4
varImpPlot(rf_4)

# train a random forest with pclass, title, family size variables
rf_train_5 <- data_combined[1:nrow(trainData),c("Pclass","Title","FamilySize")]
set.seed(1234)
rf_5 <- randomForest(x=rf_train_5, y=rf_train_label, importance = TRUE, ntree = 1000)
rf_5
varImpPlot(rf_5)

# submit the test result
test_predictors <- data_combined[(nrow(trainData)+1):nrow(data_combined),c("Pclass","Title","FamilySize")]
rf_5_predict <- predict(rf_5, test_predictors)
submit_df <- data.frame(PassengerId=rep((nrow(trainData)+1):nrow(data_combined)),Survived=rf_5_predict)
write.csv(submit_df, file="submit_rf_titanic.csv", row.names = FALSE)

# using cross validation with caret package to avoid overfit for more accurate prediction 
library(caret)
library(doSNOW)
# generate 10 folds for 10 times to have best result, 100 folds in total
# make sure that every fold have similar ratio as the overall trainging set,
# it is handled by the package function
set.seed(2348)
cv_10_folds <- createMultiFolds(rf_train_label, k=10, times=10)
table(rf_train_label)
table(rf_train_label[cv_10_folds[[33]]])
# set up caret's trainControl object
train_control_1 <- trainControl(method = "repeatedcv", number=10, repeats = 10, index = cv_10_folds)
#set up doSNOW package for multicore training(works for windows and mac)
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
# set seed and train
set.seed(34324)
rf_5_cv_2 <- train(x=rf_train_5,y=rf_train_label,method = "rf",tuneLength = 3,ntree=1000, trControl = train_control_1)
stopCluster(cl)

# with 5 folds to further prevent possible overfit
set.seed(5983)
cv_5_folds <- createMultiFolds(rf_train_label, k=5, times=10)
train_control_2 <- trainControl(method = "repeatedcv", number=5, repeats = 10, index = cv_5_folds)
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
set.seed(89472)
rf_5_cv_2 <- train(x=rf_train_5,y=rf_train_label,method = "rf",tuneLength = 3,ntree=1000, trControl = train_control_2)
stopCluster(cl)

# with 3 folds to further prevent possible overfit
set.seed(37596)
cv_3_folds <- createMultiFolds(rf_train_label, k=3, times=10)
train_control_3 <- trainControl(method = "repeatedcv", number=3, repeats = 10, index = cv_3_folds)
cl <- makeCluster(2, type = "SOCK")
registerDoSNOW(cl)
set.seed(94622)
rf_5_cv_3 <- train(x=rf_train_5,y=rf_train_label,method = "rf",tuneLength = 3,ntree=64, trControl = train_control_3)
stopCluster(cl)


