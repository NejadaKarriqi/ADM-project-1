library(ggthemes)
library(dplyr)
library(lubridate)
library(rpart)
library(randomForest)
#https://www.kaggle.com/wasdee/quick-dirty-sdohkldsfhl
trainset <- read.csv('train.csv', stringsAsFactors = F)
testset <- read.csv('test.csv', stringsAsFactors = F)

names(trainset)[1] <- 'ID'
testset$ID <- as.character(testset$ID)
data <- bind_rows(trainset, testset)

data$TimeValue <- sapply(data$AgeuponOutcome,
                         function(x) strsplit(x, split = ' ')[[1]][1])
data$UnitofTime <- sapply(data$AgeuponOutcome,
                          function(x) strsplit(x, split = ' ')[[1]][2])
data$UnitofTime <- gsub('s', '', data$UnitofTime)

data$TimeValue <- as.numeric(data$TimeValue)
data$UnitofTime <- as.factor(data$UnitofTime)

head(data)

multiplier <- ifelse(data$UnitofTime == 'day', 1,
                     ifelse(data$UnitofTime == 'week', 7,
                            ifelse(data$UnitofTime == 'month', 30,
                                   ifelse(data$UnitofTime == 'year', 365, NA))))

data$AgeinDays <- data$TimeValue * multiplier

summary(data$AgeinDays)

data$Name <- ifelse(nchar(data$Name)==0, 'Nameless', data$Name)
data$HasName[data$Name == 'Nameless'] <- 0
data$HasName[data$Name != 'Nameless'] <- 1

data$SexuponOutcome <- ifelse(nchar(data$SexuponOutcome)==0,
                              'Spayed Female', data$SexuponOutcome)

data$Hour <- hour(data$DateTime)
data$Weekday <- wday(data$DateTime)
data$Month <- month(data$DateTime)
data$Year <- year(data$DateTime)

data$TimeofDay <- ifelse(data$Hour > 5 & data$Hour < 11, 'morning',
                         ifelse(data$Hour > 10 & data$Hour < 16, 'midday',
                                ifelse(data$Hour > 15 & data$Hour < 20, 'lateday', 'night')))

data$TimeofDay <- factor(data$TimeofDay, levels = c('morning', 'midday','lateday', 'night'))

daytimes <- data[1:26729, ] %>%
  group_by(AnimalType, TimeofDay, OutcomeType) %>%
  summarise(num_animals = n())
daytimes

ggplot(daytimes, aes(x = TimeofDay, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~AnimalType) +
  coord_flip() +
  labs(y = 'Proportion of Animals',
       x = 'Animal',
       title = 'Outcomes by Time of Day: Cats & Dogs') +
  theme_few()

levels(factor(data$Breed))[1:10]

data$IsMix <- ifelse(grepl('Mix', data$Breed), 1, 0)

data$SimpleBreed <- sapply(data$Breed,
                           function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))
length(unique(factor(data$SimpleBreed)))
length(unique(factor(data$Color)))

data$OneColor <- sapply(data$Color, 
                           function(x) strsplit(x, split = '/| ')[[1]][1])
length(unique(factor(data$OneColor)))

data$Intact <- ifelse(grepl('Intact', data$SexuponOutcome), 1,
                      ifelse(grepl('Unknown', data$SexuponOutcome), 'Unknown', 0))

data$Sex <- ifelse(grepl('Male', data$SexuponOutcome), 'Male',
                   ifelse(grepl('Unknown', data$Sex), 'Unknown', 'Female'))
head(data)

intact <- data[1:26729, ] %>%
  group_by(AnimalType, Intact, OutcomeType) %>%
  summarise(num_animals = n())
library(mice)
md.pattern(data)
nrow(data)

age_fit <- rpart(AgeinDays ~ AnimalType + Sex + Intact + SimpleBreed + HasName,
                 data = data[!is.na(data$AgeinDays), ],
                 method = 'anova')
data$AgeinDays[is.na(data$AgeinDays)] <- predict(age_fit, data[is.na(data$AgeinDays), ])

factorVars <- c('Name','OutcomeType','OutcomeSubtype','AnimalType',
                'SexuponOutcome','AgeuponOutcome','SimpleBreed','SimpleColor',
                'HasName','IsMix','Intact','Sex','TimeofDay')
data[factorVars] <- lapply(data[factorVars], function(x) as.factor(x))

trainset <- data[1:26729, ]
testset <- data[26729:nrow(data), ]

set.seed(731)
rf_mod <- randomForest(OutcomeType ~ AnimalType+AgeinDays+Intact+HasName+SimpleColor+IsMix+Sex,
                       data=trainset,
                       ntree = 600,
                       importance = TRUE)
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)
importance <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance),
                            Importance = round(importance[ , 'MeanDecreaseGini'], 2))

rankImportance <- varImportance %>%
  #mutate(Rank = paste0('#', dense_rank(desc(Importance))))

ggplot(rankImportance, aes(x = reorder(Variables, Importance),
                           y=Importance)) +
  geom_bar(stat='identity', colour='black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0,vjust=0.55, size=4, colour='lavender',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() +
  theme_few()
prediction <- predict(rf_mod, testset, type = 'vote')
prediction
solution <- data.frame('ID' = testset$ID, prediction)
sum(solution$Adoption)
