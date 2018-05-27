

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
  geom_bar(width = 0.5)+ggtitle("Pclass")+facet_wrap(~Pclass)
   xlab("Title")+ylab("Count")+labs(fill="Survived")















