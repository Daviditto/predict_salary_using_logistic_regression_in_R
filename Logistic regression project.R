df <- read.csv('adult_sal.csv')
head(df)
library(dplyr)
df <- select(df, -X)
head(df)
summary(df)
str(df)

#data cleaning
# type_employer column
table(df$type_employer)

cus_func <- function(x){
  if(x %in% c('Never-worked', 'Without-pay')){
    x <- 'Unemployed'
  }else{
    x <- x
  }
  return(x)
}

df$type_employer <- sapply(df$type_employer, cus_func)
table(df$type_employer)

cus_func2 <- function(x){
  if(x %in% c('Local-gov', 'State-gov')){
    x <- 'SL-gov'
  }else{
    x <- x
  }
  return(x)
}


df$type_employer <- sapply(df$type_employer, cus_func2)
table(df$type_employer)


cus_func3<- function(x){
  if(x %in% c('Self-emp-inc', 'Self-emp-not-inc')){
    x <- 'self-emp'
  }else{
    x <- x
  }
  return(x)
}

df$type_employer <- sapply(df$type_employer, cus_func3)
table(df$type_employer)

# marital column
table(df$marital)

cus_func4<- function(x){
  if(x %in% c('Married-civ-spouse', 'Married-AF-spouse', 'Married-spouse-absent')){
    x <- 'Married'
  }else if(x %in% c('Divorced', 'Separated', 'Widowed')){
    x <- 'Not-Married'
  }else{
    x <- x
  }
  return(x)
}

df$marital <- sapply(df$marital, cus_func4)
table(df$marital)

# country column
table(df$country)

Asia <- c('China','Hong','India','Iran','Cambodia','Japan', 'Laos' ,
          'Philippines' ,'Vietnam' ,'Taiwan', 'Thailand')

North.America <- c('Canada','United-States','Puerto-Rico' )

Europe <- c('England' ,'France', 'Germany' ,'Greece','Holand-Netherlands','Hungary',
            'Ireland','Italy','Poland','Portugal','Scotland','Yugoslavia')

Latin.and.South.America <- c('Columbia','Cuba','Dominican-Republic','Ecuador',
                             'El-Salvador','Guatemala','Haiti','Honduras',
                             'Mexico','Nicaragua','Outlying-US(Guam-USVI-etc)','Peru',
                             'Jamaica','Trinadad&Tobago')
Other <- c('South')

group_country <- function(ctry){
  if (ctry %in% Asia){
    return('Asia')
  }else if (ctry %in% North.America){
    return('North.America')
  }else if (ctry %in% Europe){
    return('Europe')
  }else if (ctry %in% Latin.and.South.America){
    return('Latin.and.South.America')
  }else{
    return('Other')      
  }
}

df$country <- sapply(df$country,group_country)
table(df$country)


#missing data
library(Amelia)
df[df=='?'] <- NA

table(df$type_employer)

df$type_employer <- factor(df$type_employer)
df$marital<- factor(df$marital)
df$country <- factor(df$country)
df$occupation <- factor(df$occupation)

missmap(df)
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

df <- na.omit(df)
missmap(df,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#EDA
str(df)
library(ggplot2)
ggplot(df, aes(age)) + geom_histogram(aes(fill=income), color='black', bins=30)
ggplot(df, aes(hr_per_week)) + geom_histogram(color='black', bins=30)
colnames(df)

df<- rename(df, region=country)
colnames(df)

ggplot(df, aes(region)) +geom_bar(aes(fill=income)) + scale_x_discrete(guide = guide_axis(angle = 90))

# train the model
head(df)
library(caTools)
split <- sample.split(df$income, SplitRatio = 0.7)
train <- subset(df, split=T)
test <- subset(df, split=F)

help(glm)
train$income <- factor(train$income)

log.model <- glm(income~., family=binomial(logit), data = train)

summary(log.model)
help(step)
new.model <- step(log.model)
summary(new.model)
test$income <- factor(test$income)

# make predictions
pred <- predict(new.model,test, type='response')
pred.class <- ifelse(pred>0.5, '>50K', '<=50K')
table(test$income, pred.class)

# evaluate the result
miserror <- mean(test$income != pred.class)
print(1-miserror)

recall <- (21401/(21401+1667))
print(recall)

precision <- (21401/(21401+3017))
print(precision)





