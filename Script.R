#Read CSV
adult<-read.csv('adult_sal.csv')
head(adult)

#Cleaning Data
library(dplyr)
adult<-select(adult,-X)

head(adult)
str(adult)
summary(adult)

colnames(adult)

table(adult$type_employer)

##Function to merge Never-worked and Without pay = Unemployed
unemp<-function(job)
{
  job<-as.character(job)
  if(job=="Never-worked" | job=="Without-pay")
  {
    return('Unemployed') 
  }
  else
  {
    return(job)
  }
}

#Apply
adult$type_employer<-sapply(adult$type_employer,unemp)

table(adult$type_employer)

##Function to State and Local gov jobs into a category called SL-gov

unemp1<-function(job)
{
  job<-as.character(job)
  if(job=="Local-gov" | job=="State-gov")
  {
    return('SL-gov') 
  }
  else
  {
    return(job)
  }
}

#Apply
adult$type_employer<-sapply(adult$type_employer,unemp1)

table(adult$type_employer)


##Function to  Self-emp-inc and Self-emp-not-inc into self-emp
unemp2<-function(job)
{
  job<-as.character(job)
  if(job=="Self-emp-inc" | job=="Self-emp-not-inc")
  {
    return('self-emp') 
  }
  else
  {
    return(job)
  }
}

#Apply
adult$type_employer<-sapply(adult$type_employer,unemp2)

table(adult$type_employer)

#Marital Coloum
table(adult$marital)


##Function to Married COloum
Marr1<-function(marr)
{
  marr<-as.character(marr)
  if(marr=="Married-AF-spouse" | marr=="Married-civ-spouse" | marr=="Married-spouse-absent")
  {
    return('Married') 
  }
  
  if(marr=="Divorced" | marr=="Widowed" | marr=="Separated")
  {
    return("Not-Married")
  }
  else
  {
    return(marr)
  }
}

#Apply
adult$marital<-sapply(adult$marital,Marr1)

table(adult$marital)

## Grouping Country
table(adult$country)
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

adult$country<-sapply(adult$country,group_country)
table(adult$country)


str(adult)


adult$education <- sapply(adult$education,factor)
adult$occupation <- sapply(adult$occupation,factor)
adult$relationship <- sapply(adult$relationship,factor)
adult$race <- sapply(adult$race,factor)
adult$sex <- sapply(adult$sex,factor)
adult$income <- sapply(adult$income,factor)

str(adult)

#Missing Data
library(Amelia)
adult[adult=='?']<-NA

adult$type_employer <- sapply(adult$type_employer,factor)
adult$country <- sapply(adult$country,factor)
adult$marital <- sapply(adult$marital,factor)

table(adult$type_employer)

missmap(adult)
missmap(adult,y.at=c(1),y.labels = c(''),col=c('yellow','black'))

#Drop NA values
adult<-na.omit(adult)
missmap(adult)


str(adult)

#Plotting AGe
library(ggplot2)
pl<-ggplot(adult,aes(age))+geom_histogram(aes(fill=income),binwidth = 1,color="black")
print(pl)

#Plotting HOurs
pl2<-ggplot(adult,aes(hr_per_week))+geom_histogram()
print(pl2)

#changine name country to region
names(adult)[names(adult)=="country"] <- "region"
str(adult)

##barplot of region with the fill color defined by income class
pl3<-ggplot(adult,aes(region))+geom_bar(aes(fill=income))+theme(axis.text.x = element_text(angle = 90))
print(pl3)



#Building MOdel
head(adult)


library(caTools)
set.seed(101)
sample <- sample.split(adult$income, SplitRatio = 0.70)
train = subset(adult, sample == TRUE)
test = subset(adult, sample == FALSE)


model<-glm(income~.,family = binomial(logit),data = train)
summary(model)


new.step.model<-step(model)

test$predicted.income<-predict(model,newdata = test,type = 'response')
table(test$income, test$predicted.income > 0.5)
(6372+1423)/(6372+1423+548+872)


#recall
6732/(6372+548)


#precision
6732/(6372+872)
