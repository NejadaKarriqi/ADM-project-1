install.packages("GGally")
library(ggplot2)
library(dplyr)
library(GGally)
library(rpart)
library(rpart.plot)
#library(randomForest)

test <- read.csv('test.csv',stringsAsFactors = FALSE)
train <- read.csv('train.csv', stringsAsFactors = FALSE)

full <- bind_rows(train,test)
LT=dim(train)[1]

str(full)

colSums(is.na(full))
colSums(full=="")
# We have a lot of missing full in column name
#lets put instead a noname for the missing values
full$Name[full$Name==""]="Unnamed"

# Let's see how many features we can move to factors
apply(full,2, function(x) length(unique(x)))

## Let's move the features AnimalID, SexuponOutcome and ID to be factors
cols<-c("AnimalID", "SexuponOutcome", "ID")
for (i in cols){
  full[,i] <- as.factor(full[,i])
}

# First, let's look at the relationship between OutcomeType and age:

full$TimeValue <- sapply(full$AgeuponOutcome,
                         function(x) strsplit(x, split = ' ')[[1]][1])
full$UnitofTime <- sapply(full$AgeuponOutcome,
                          function(x) strsplit(x, split = ' ')[[1]][2])
full$UnitofTime <- gsub('s', '', full$UnitofTime)

full$TimeValue <- as.numeric(full$TimeValue)
full$UnitofTime <- as.factor(full$UnitofTime)
multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30,
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

AgeinDays <- as.numeric(full$TimeValue ) * multiplier
full$AgeinDays <- AgeinDays

Age_division <- ifelse(AgeinDays <= 185, 'Infants',
                            ifelse(AgeinDays > 185 & AgeinDays<=365,'child',
                                     ifelse(AgeinDays > 365 & AgeinDays<=1085,'Adult',
                                          ifelse(AgeinDays>1825,'Old', "Unknown"))))

full$Age_division <-Age_division


ggplot(data=full[1:LT,],aes(x=Age_division,fill=OutcomeType))+geom_bar()

# OutcomeType as a function of name:
full$HasName[full$Name!=""] <- "yes"
full$HasName[full$Name=="Unnamed"] <- "no"
ggplot(data = full[1:LT,],aes(x=HasName,fill=OutcomeType))+geom_bar(position="fill")+ylab("Frequency")
#lets see the % of outcometype as a function of name
t<-table(full[1:LT,]$HasName,full[1:LT,]$OutcomeType)
for (i in 1:dim(t)[1]){
  t[i,]<-t[i,]/sum(t[i,])*100
  }
  t

#correlation of outcomeType with animalType

ggplot(data = full[1:LT,],aes(x=AnimalType,fill=OutcomeType))+geom_bar(position="fill")+ylab("Frequency")
ggplot(data = full[1:LT,],aes(x=SexuponOutcome,fill=OutcomeType))+geom_bar(position="fill")+ylab("Frequency")

# The color of the animal can affect his outcometype:

full$colorNew <- full$Color
full$colorNew <- "Other"
full$colorNew[grepl("White",full$Color)] <- "White"
full$colorNew[grepl("Black",full$Color)] <- "Black"
full$colorNew[grepl("Yellow",full$Color)] <- "Yellow"
full$colorNew[grepl("Orange",full$Color)] <- "Orange"
full$colorNew[grepl("Red",full$Color)] <- "Red"
full$colorNew[grepl("Calico",full$Color)] <- "Calico"
full$colorNew[grepl("Brown",full$Color)] <- "Brown"
full$colorNew[grepl("Blue",full$Color)] <- "Blue"


full$colorNew<- as.factor(full$colorNew)

ggplot(data = full[1:LT,],aes(x=colorNew,fill=OutcomeType))+geom_bar(position="fill")+ylab("Frequency")
#not much affect on adoption but for sure affects transfer and return to owner.


train_im<- full[1:LT, c("OutcomeType","colorNew","Age_division","HasName")]
ind<-sample(1:dim(train_im)[1],13365) 
training<-train_im[ind,] # The train set of the model
testing<-train_im[-ind,] # The test set of the model
glm_model <- glm(factor(OutcomeType) ~ Age_division+HasName+colorNew,
                   data = training, family=binomial)

glm_model


prediction <- predict(glm_model, testing, type="response")
summary(glm_model)
# Let's look at the prediction of this model on the test set
pred.train <- predict(glm_model,testing)
pred.train <- ifelse(pred.train > 0.5,1,0)



#glm_model2 <- glm(factor(OutcomeType) ~ Age_division+HasName+colorNew,
                 #data = testing, family=binomial)
#prediction2 <- predict(glm_model2, testing, type="response")

#model_output <- cbind(training, prediction)
#head(model_output)

#pred.train <- predict(glm_model,testing,type = "response")


t1<-table(pred.train,testing$OutcomeType)
# Precision and recall of the model
precision<- t1[1,1]/(sum(t1[1,]))
recall<- t1[1,1]/(sum(t1[,1]))
precision
recall
recall

F1<- 2*precision*recall/(precision+recall)
F1

# F1 score on the initial test set is 0.609. This not good.

