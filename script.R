library('ggplot2') # visualization
library('dplyr') # data manipulation
library('tidyr') # data manipulation

train <- read.csv('./Input/train.csv', stringsAsFactors = F)

# Make some values factors instead of ints
train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

test  <- read.csv('./Input/test.csv', stringsAsFactors = F)

# Make some values factors instead of ints
test <- test %>% mutate(
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

full  <- bind_rows(train, test) # bind training & test data

# put all the different titles in a new variable
full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)

# Titles with very low cell counts to be combined to "rare" level
rare_title <- c('Dona', 'Lady', 'the Countess','Capt', 'Col', 'Don', 
                'Dr', 'Major', 'Rev', 'Sir', 'Jonkheer')

# Also reassign mlle, ms, and mme accordingly
full$Title[full$Title == 'Mlle']        <- 'Miss' 
full$Title[full$Title == 'Ms']          <- 'Miss'
full$Title[full$Title == 'Mme']         <- 'Mrs' 
full$Title[full$Title %in% rare_title]  <- 'Rare Title'

# Show title counts by sex again
table(full$Sex, full$Title)

# Finally, grab surname from passenger name
full$Surname <- sapply(full$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])

# Create a family size variable including the passenger themselves
full$Fsize <- full$SibSp + full$Parch + 1

# Put the family names in the table and add how big they are
full$Family <- paste(full$Surname, full$Fsize, sep='_')

# check data
str(full)
summary(full)

aggr(combine, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

# Check how old the passengers were and divide them by gender
ggplot(data = full, mapping = aes(x = Age, color = Sex)) +
  geom_bar(binwidth = 1) +
  theme_few()

# Use ggplot2 to visualize the relationship between family size & survival
# 0 = died
# 1 = survived
ggplot(full[1:891,], aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size') +
  theme_few()

ggplot(full, aes(x = Fare)) +
  geom_freqpoly()

# Show the density of if people survived or died by in which class they were and how much they paid
train %>%
  ggplot(aes(Fare, fill=Pclass)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~ Survived, ncol = 1)

train %>%
  filter(Embarked %in% c("S","C","Q")) %>%
  ggplot() +
  geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
  facet_grid(~ Survived)
