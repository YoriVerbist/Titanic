library('ggplot2') 
library('ggthemes') 
library('scales') 
library('dplyr') 

train <- read.csv('../Documents/Jaar3/Data science/train.csv', stringsAsFactors = F)
test  <- read.csv('../Documents/Jaar3/Data science/test.csv', stringsAsFactors = F)
full  <- bind_rows(train, test) 

str(full)

full$Title <- gsub('(.*, )|(\\..*)', '', full$Name)
table(full$Sex, full$Title)
