install.packages("stringdist")
install.packages("ggthemes")
install.packages("caret")
install.packages("lattice")
install.packages("VGAM")
install.packages("wordcloud")
# Load packages
library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm
#library(stringdist) # algorithm to translate strings to phonetic codes


library(readr) # CSV file I/O, e.g. the read_csv function
library(plyr)
library(caret)
library(VGAM)

library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(randomForest) # classification algorithm


library(data.table)
library(wordcloud)
library(stringr)

library(stringdist) # algorithm to translate strings to phonetic codes

train <- read.csv("train.csv", stringsAsFactors = F)
test <- read.csv("test.csv", stringsAsFactors = F)
full <- bind_rows(train, test)


names(train)[1] = 'ID'
test$ID = as.character(test$ID)

multiplier <- ifelse(full$UnitofTime == 'day', 1,
                     ifelse(full$UnitofTime == 'week', 7,
                            ifelse(full$UnitofTime == 'month', 30, # Close enough
                                   ifelse(full$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
full$AgeinDays <- full$TimeValue * multiplier


summary(full$AgeinDays)

set.seed(731)

# Build the model
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+Hour+Weekday+TimeofDay+SimpleColor+IsMix+Sex+Month, 
                       data = train, 
                       ntree = 600, 
                       importance = TRUE)

# Show model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)


outcomes <- full[1:26729, ] %>%
  group_by(AnimalType, OutcomeType) %>%
  summarise(num_animals = n())
factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay','Lifestage')

full[factorVars] <- lapply(full[factorVars], function(x) as.factor(x))

#observe the most important factors on adoption

full = rbindlist(list(train, test), use.names = TRUE, fill=TRUE)
summary(full)
train <- full[1:26729, ]
test <- full[26729:nrow(full), ]
factor(full$AgeuponOutcome)[1:10]
full[ , c('Date', 'Time') := tstrsplit(DateTime, ' ', fixed=TRUE)]
full[ , c('Year', 'Month', 'Day') := tstrsplit(Date, '-', fixed=TRUE)]
full[ , c('Hour', 'Minute', 'Second') := tstrsplit(Time, ':', fixed=TRUE)]
full[, c('Date', 'Time', 'Minute', 'Second')] = NULL
full$AgeuponOutcome=as.character(full$AgeuponOutcome)
full[ , c('Duration', 'TimeUnits')] <- tstrsplit(full[ , AgeuponOutcome], " ", fixed = TRUE)

full$TimeUnits = str_replace(full$TimeUnits, "s$", "")
full[TimeUnits == 'year', TimeUnits := 365 ]
full[TimeUnits == 'month', TimeUnits := 30 ]
full[TimeUnits == 'week', TimeUnits := 7 ]
full[TimeUnits == 'day', TimeUnits := 1 ]
full[ ,AgeinDays:= as.numeric(full[ ,Duration])* as.numeric(full[,TimeUnits])]
full

#full$TimeValue <- sapply(full$AgeuponOutcome,  
                         #function(x) strsplit(x, ' ')[[1]][1])

set.seed(731)
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+Hour+Weekday+TimeofDay+SimpleColor+IsMix+Sex+Month,
                       data=train,
                       ntree = 100,
                       importance = TRUE)
#plot(rf_mod, ylim=c(0,1))
#legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)


#importance <- importance(rf_mod)
#varImportance <- data.frame(Variables = row.names(importance),
                            #Importance = round(importance[ , 'MeanDecreaseGini'], 2))

#rankI

ate(Rank = paste0('#', dense_rank(desc(Importance))))

#ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                          # y=Importance)) +
  #geom_bar(stat='identity', colour='black') +
 # geom_text(aes(x = Variables, y = 0.5, label = Rank),
            #hjust=0,vjust=0.55, size=4, colour='lavender',
            #fontface = 'bold') +
  #labs(x = 'Variables', title = 'Relative Variable Importance') +
 # coord_flip() +
 # theme_few()