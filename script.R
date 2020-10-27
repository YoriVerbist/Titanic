library('ggplot2') # visualization
library('dplyr') # data manipulation
library('tidyr') # data manipulation
library('stringr') # string manipulation


train <- read.csv('./Input/train.csv', stringsAsFactors = F)

# Make some values factors instead of ints
train <- train %>% mutate(
  Survived = factor(Survived),
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex),
)

test  <- read.csv('./Input/test.csv', stringsAsFactors = F)

# Make some values factors instead of ints
test <- test %>% mutate(
  Pclass = factor(Pclass),
  Embarked = factor(Embarked),
  Sex = factor(Sex)
)

combine  <- bind_rows(train, test) # bind training & test data

# Show title counts by sex again
table(train$Sex, train$Title)

# Finally, grab surname from passenger name
train$Surname <- sapply(train$Name,  
                       function(x) strsplit(x, split = '[,.]')[[1]][1])
 
# Create a family size variable including the passenger themselves
train$Fsize <- train$SibSp + train$Parch + 1

# Put the family names in the table and add how big they are
train$Family <- paste(train$Surname, train$Fsize, sep='_')

# check data
str(train)
summary(train)

aggr(combine, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

# Use ggplot2 to visualize the relationship between family size & survival
# 0 = died
# 1 = survived
ggplot(train, aes(x = Fsize, fill = Survived)) +
  geom_bar(stat='count', position='dodge') +
  scale_x_continuous(breaks=c(1:11)) +
  labs(x = 'Family Size')

ggplot(full, aes(x = Fare)) +
  geom_freqpoly()

# Show the density of if people survived or died by in which class they were and how much they paid
train %>%
  ggplot(aes(Fare, fill=Pclass)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  facet_wrap(~ Survived, ncol = 1)

# Show survival rate filtered on where they boarded
train %>%
  filter(Embarked %in% c("S","C","Q")) %>%
  ggplot() +
  geom_bar(aes(Embarked, fill = Pclass), position = "dodge") +
  facet_grid(~ Survived)

# Show the how the density of people's survival rate odered by age and sex
ggplot(train, aes(x=Age)) +
  geom_density(aes(fill = Survived), alpha = 0.5) +
  facet_wrap(~Sex)

# Show how many family members each sex has on board
train %>%
  ggplot() +
  geom_bar(aes(Parch, fill = Sex), position = "dodge") +
  scale_y_log10() +
  labs(x = '# of family members')

train %>%
  mutate(SibSp = factor(SibSp)) %>%
  ggplot(aes(x=Age, color = SibSp)) +
  geom_density(size = 1.5)

combine <- mutate(combine,
                  fclass = factor(log10(Fare+1) %/% 1),
                  age_known = factor(!is.na(Age)),
                  cabin_known = factor(!is.na(Cabin)),
                  title_orig = factor(str_extract(Name, "[A-Z][a-z]*\\.")), # Re done by https://www.kaggle.com/nad136
                  
                  # Young people are people younger than 30
                  young = factor( if_else(Age<=30, 1, 0, missing = 0) | (title_orig %in% c('Master.','Miss.','Mlle.')) ),
                  child = Age<10,
                  family = SibSp + Parch,
                  alone = (SibSp == 0) & (Parch == 0),
                  large_family = (SibSp > 2) | (Parch > 3),
                  deck = if_else(is.na(Cabin),"U",str_sub(Cabin,1,1)),
                  ttype = str_sub(Ticket,1,1),
                  bad_ticket = ttype %in% c('1', '5', '6', '7', '8', 'A', 'F', 'W')
)

tgroup <- combine %>%
  group_by(Ticket) %>%
  summarise(ticket_group = n()) %>%
  ungroup

combine <- left_join(combine, tgroup, by = "Ticket") %>%
  mutate(shared_ticket = ticket_group > 1)

combine <- combine %>%
  mutate(fare_eff = Fare/ticket_group,
         title = fct_lump(title_orig, n=4),
  )

# Put the correct data back in the correct dataset
train <- combine %>% filter(!is.na(Survived))
test <- combine %>% filter(is.na(Survived))

# Function to show some survival rates
plot_bar_fill_grid <- function(barx, filly, gridx, gridy){
  train %>%
    ggplot(aes_string(barx, fill = filly)) +
    geom_bar(position = "fill") +
    facet_grid(reformulate(gridy,gridx))
}

# Show the survival rate of young people ordered by price class
plot_bar_fill_grid("young", "Survived", "Sex", "Pclass")

# Show the survival rate of the age of the person is known ordered by price class
plot_bar_fill_grid("age_known", "Survived", "Sex", "Pclass")
