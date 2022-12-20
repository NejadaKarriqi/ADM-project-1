install.packages("ggplot2")
library(ggplot2)
library(dplyr)
train <- read.csv("train.csv")
test <- read.csv("test.csv")

entire_data <- bind_rows(train,test)
counts<-table(train$AnimalType)
barplot(counts,main="Dogs vs. Cats",beside = TRUE,col=c("dark green","dark red"),horiz=FALSE)

counts <- table(train$AnimalType,train$OutcomeType)
barplot(counts,main="Outcome Type vs Animal Type",legend=rownames(counts),beside = TRUE,col=c("red","yellow"),las=2)


##train$AgeuponOutcome <- as.factor(train$AgeuponOutcome)
Time_unit<-sapply(as.character(entire_data$AgeuponOutcome), function(x) strsplit(x, split = ' ')[[1]][1])
Multiplier <- sapply(as.character(entire_data$AgeuponOutcome), function(x) strsplit(x, split = ' ')[[1]][2])
Multiplier<-gsub('s','',Multiplier)
Multiplier_Conv <- ifelse(Multiplier=='day',1,
                          ifelse(Multiplier=='week',7,
                                 ifelse(Multiplier=='month',30,
                                        ifelse(Multiplier=='year',365,NA))))

AgeuponOutcomeInDays <- as.numeric(Time_unit)* Multiplier_Conv
entire_data$AgeuponOutcomeInDays<-AgeuponOutcomeInDays

Multiplier_age <- ifelse(AgeuponOutcomeInDays <= 185,'Infants',
                         ifelse(AgeuponOutcomeInDays > 185 & AgeuponOutcomeInDays<=365,'child',
                                ifelse(AgeuponOutcomeInDays > 365 & AgeuponOutcomeInDays<=1085,'Adult',
                                       ifelse(AgeuponOutcomeInDays>1825,'Aged',NA))))

entire_data$Multiplier_age <-Multiplier_age

counts <- table(entire_data[1:26729,]$Multiplier_age,entire_data[1:26729,]$Multiplier_age)
barplot(counts,main="Outcome Type vs Animal Type")

Infant <-filter(entire_data[1:26729,],Multiplier_age=='Infants')
Child <-filter(entire_data[1:26729,],Multiplier_age=='child')
Adult<-filter(entire_data[1:26729,],Multiplier_age=='Adult')
Aged<-filter(entire_data[1:26729,],Multiplier_age=='Aged')
#par(mfrow=c(2,2))
count_infant <- table(Infant$OutcomeType,Infant$SexuponOutcome)

barplot(count_infant,legend=row.names(count_infant),args.legend = list(title = "Outcometype", x = "topleft", cex = .5),col = c("red", "yellow", "grey","brown","dark green"),main = "Infant Distribution",las=2)

count_Child <- table(Child$OutcomeType,Child$SexuponOutcome)

barplot(count_Child,legend=row.names(count_Child),col = c("red", "yellow", "grey","brown","dark green"),args.legend = list(title = "Outcometype", x = "topleft", cex = .4),main = "Child Distribution",las=2)

count_Adult<-table(Adult$OutcomeType,Adult$SexuponOutcome)


barplot(count_Adult,legend=row.names(count_Adult),col = c("lightblue", "mistyrose", "lavender","lightgreen","orange"),args.legend = list(title = "Outcometype", x = "topleft", cex = .4),main = "Adult Distribution",las=2)

count_Aged<-table(Aged$OutcomeType,Aged$SexuponOutcome)

barplot(count_Aged,legend=row.names(count_Aged),col = c("red", "yellow", "grey","brown","dark green"),args.legend = list(title = "Outcometype", x = "topleft", cex = .4),main = "Aged Distribution",las=2)

#naming part
install.packages("stringdist")
install.packages("ggthemes")
library(stringdist)
library(dplyr) # data handling
library(stringdist) # algorithm to translate strings to phonetic codes
library(ggplot2) # visualization
library(ggthemes) # Visualization

train <- read.csv("train.csv", header=T)

#Make new variables

# Make new variable "HasName" indicating if animal has a name or not
train$HasName[train$Name!=""] <- "yes"
train$HasName[train$Name==""] <- "no"
 
# Make new variable Sex indicating if animal is female or male  
train$Sex <- "unknown"
train$Sex[train$SexuponOutcome=="Intact Female" | train$SexuponOutcome=="Spayed Female"] <- "Female"
train$Sex[train$SexuponOutcome=="Intact Male" | train$SexuponOutcome=="Neutered Male"] <- "Male"

# Make new variable phonetic, this variable groups names according to how they sound.
#With other words, similar sounding names get the same code. The method used is Soundex.
#For more information see the following link:http://www.archives.gov/research/census/soundex.html 
#The soundex algorithm is only meaningful for characters in the range a-z and A-Z, names with other characters are removed from the dataset

# Remove names with nonascii characters 
train$ascii <- printable_ascii(train$Name)
train <- train %>% filter(ascii==T)
# remove white spaces before and after every name
#train$Name <- str_trim(train$Name)

# remove names with numbers
train <- train[-grep("[0-9]", train$Name),] 
train$phonetic <- phonetic(train$Name, method = c("soundex"), useBytes = FALSE)

#.Is having a name associated with the OutcomeType?
# reshape  
counts <- train %>% group_by(HasName, OutcomeType) %>% summarise(num_animals = n())

# Plot
ggplot(counts, aes(x= HasName, y = num_animals, fill = OutcomeType)) +         
  geom_bar(stat = 'identity', position = 'fill', colour = 'white') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Name',
       title = 'Name vs. no Name') +
  theme_few()

#What are popular names and does having a popular names increases the chance of being adopted.
#Top10 name types for female cats
train %>% filter(HasName=="yes") %>% filter(AnimalType=="Cat"& Sex=="Female") %>% count(phonetic) %>% arrange(desc(n))
train$phonetic2 <- "other"
train$phonetic2[train$phonetic=="L400"] <- "L400"
train$phonetic2[train$phonetic=="M200"] <- "M200"
train$phonetic2[train$phonetic=="L200"] <- "L200"
train$phonetic2[train$phonetic=="B400"] <- "B400"
train$phonetic2[train$phonetic=="J500"] <- "J500"
train$phonetic2[train$phonetic=="C400"] <- "C400"
train$phonetic2[train$phonetic=="S100"] <- "S100"
train$phonetic2[train$phonetic=="S200"] <- "S200"
train$phonetic2[train$phonetic=="C200"] <- "C200"
train$phonetic2[train$phonetic=="K300"] <- "K300"

# reshape  
counts <- train %>% filter(HasName=="yes") %>% filter(AnimalType=="Cat" & Sex=="Female") %>% group_by(phonetic2, OutcomeType) %>% summarise(num_animals = n())

# Plot
ggplot(counts, aes(x= phonetic2, y = num_animals, fill = OutcomeType)) +         
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Name',
       title = 'Famale cats') +
  theme_few()

#Names that belong to the B400 group
train %>% filter(AnimalType=="Cat" & Sex=="Female" & train$phonetic=="B400") %>% count(Name) %>% arrange(desc(n))
#Names that belong to the L200 group
train %>% filter(AnimalType=="Cat" & Sex=="Female" & train$phonetic=="L200") %>% count(Name) %>% arrange(desc(n))
#Top10 names for Male cats
train %>% filter(HasName=="yes") %>% filter(AnimalType=="Cat" & Sex=="Male") %>% count(phonetic) %>% arrange(desc(n))

train$phonetic2 <- "other"
train$phonetic2[train$phonetic=="M200"] <- "M200"
train$phonetic2[train$phonetic=="R200"] <- "R200"
train$phonetic2[train$phonetic=="J200"] <- "J200"
train$phonetic2[train$phonetic=="T500"] <- "T500"
train$phonetic2[train$phonetic=="S500"] <- "S500"
train$phonetic2[train$phonetic=="L200"] <- "L200"
train$phonetic2[train$phonetic=="J500"] <- "J500"
train$phonetic2[train$phonetic=="0416"] <- "0416"
train$phonetic2[train$phonetic=="L000"] <- "L000"
train$phonetic2[train$phonetic=="C640"] <- "C640"


# reshape  
counts <- train %>% filter(HasName=="yes") %>% filter(AnimalType=="Cat" & Sex=="Male") %>% group_by(phonetic2, OutcomeType) %>% summarise(num_animals = n())

# Plot
ggplot(counts, aes(x= phonetic2, y = num_animals, fill = OutcomeType)) +         
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Name',
       title = 'Male cats') + theme_few()

#Names that belong to the L000 group
train %>% filter(AnimalType=="Cat" & Sex=="Male" & train$phonetic=="L000") %>% count(Name) %>% arrange(desc(n))
#Names that belong to the C640 group
train %>% filter(AnimalType=="Cat" & Sex=="Male" & train$phonetic=="C640") %>% count(Name) %>% arrange(desc(n))
#Top10 names for female dogs
train %>% filter(HasName=="yes") %>% filter(AnimalType=="Dog" & Sex=="Female") %>% count(phonetic) %>% arrange(desc(n))

train$phonetic2 <- "other"
train$phonetic2[train$phonetic=="L200"] <- "L200"
train$phonetic2[train$phonetic=="L400"] <- "L400"
train$phonetic2[train$phonetic=="B400"] <- "B400"
train$phonetic2[train$phonetic=="C200"] <- "C200"
train$phonetic2[train$phonetic=="R200"] <- "R200"
train$phonetic2[train$phonetic=="D200"] <- "D200"
train$phonetic2[train$phonetic=="M200"] <- "M200"
train$phonetic2[train$phonetic=="S200"] <- "S200"
train$phonetic2[train$phonetic=="S300"] <- "S300"
train$phonetic2[train$phonetic=="P652"] <- "P652"


# reshape  
counts <- train %>% filter(HasName=="yes") %>% filter(AnimalType=="Dog" & Sex=="Female") %>% group_by(phonetic2, OutcomeType) %>% summarise(num_animals = n())
# Plot
ggplot(counts, aes(x= phonetic2, y = num_animals, fill = OutcomeType)) +         
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Name',
       title = 'Female dogs') +
  theme_few()

train %>% filter(AnimalType=="Dog" & Sex=="Female" & train$phonetic=="M200") %>% count(Name) %>% arrange(desc(n))
train %>% filter(AnimalType=="Dog" & Sex=="Female" & train$phonetic=="R200") %>% count(Name) %>% arrange(desc(n))

#Top10 names for Male dogs
train %>% filter(HasName=="yes") %>% filter(AnimalType=="Dog" & Sex=="Male") %>% count(phonetic) %>% arrange(desc(n))
train$phonetic2 <- "other"
train$phonetic2[train$phonetic=="R200"] <- "R200"
train$phonetic2[train$phonetic=="M200"] <- "M200"
train$phonetic2[train$phonetic=="C200"] <- "C200"
train$phonetic2[train$phonetic=="J200"] <- "J200"
train$phonetic2[train$phonetic=="B300"] <- "B300"
train$phonetic2[train$phonetic=="D200"] <- "D200"
train$phonetic2[train$phonetic=="L200"] <- "L200"
train$phonetic2[train$phonetic=="C640"] <- "C640"
train$phonetic2[train$phonetic=="B400"] <- "B400"
train$phonetic2[train$phonetic=="B650"] <- "B650"


# reshape  
counts <- train %>% filter(HasName=="yes") %>% filter(AnimalType=="Dog" & Sex=="Male") %>% group_by(phonetic2, OutcomeType) %>% summarise(num_animals = n())
# Plot
ggplot(counts, aes(x= phonetic2, y = num_animals, fill = OutcomeType)) +         
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  coord_flip() +
  labs(y = 'Proportion of Animals', 
       x = 'Name',
       title = 'Male dogs') +
  theme_few()

train %>% filter(AnimalType=="Dog" & Sex=="Male" & train$phonetic=="B300") %>% count(Name) %>% arrange(desc(n))
train %>% filter(AnimalType=="Dog" & Sex=="Male" & train$phonetic=="C640") %>% count(Name) %>% arrange(desc(n))

