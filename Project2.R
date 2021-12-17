rm(mm2)
library(imputeTS)
library(ggplot2)

mm<- read.csv('c:/users/lewis/documents/mammographic_masses.data')

ggplot(data=mm,aes(x=X67))+
  geom_histogram(binwidth = 1,stat = 'count')

test1<-na_interpolation(mm$X67)


#observation   4 of the rows are not in the right class, they are characters, they should be integers

class(mm$BIRADS) = 'integer'
class(mm$Age) = 'integer'
class(mm$Shape) = 'integer'
class(mm$Margin) = 'integer'
class(mm$Density) = 'integer'
class(mm$Severity) = 'integer'

install.packages('mice')
install.packages('VIM')
library(mice)
library(VIM)
summary(mm)


#cleaned up the columns from character to integer,
# now summary will show useful information to begin data cleaning.
# i have decided to use imputation by interpolation to fill the NA's because some columns
# seem to have values correlated to other columns.

#rename columns to sensible descriptions

names(mm)<- c('BIRADS','Age','Shape','Margin','Density','Severity')

#for BI-RADS, i will replace the 2 missing values with mean
#In the summary 4 is the mean value.

#There is what i assume to be a typographical error for a firld in
#BI-RADS it has 55, but  it is out of the range
#i assume it was supposed to be 5
mm$BIRADS[340]=5

#replaced BIrad column missing values using mean
mm$BIRADS[is.na(mm$BIRADS)]<-sprintf(mean(mm$BIRADS,na.rm = TRUE),fmt='%#.0f')
class(mm$BIRADS) = 'integer'


#test block

mmb<-mm
mmb$BIRADS[is.na(mmb$BIRADS)]<-sprintf(mean(mmb$BIRADS,na.rm = TRUE),fmt='%#.0f')
summary(mmb)

md.pattern(mmb)
test_plot <- aggr(mmb,col=c('green','red'),numbers=TRUE,sortVars=TRUE,
                  labels=names(mmb),cex.axis=.7,
                  gap=1,ylab=c('Missing data','Pattern'))

imputemmb<-mice(mmb,m=5,maxit = 50,method = 'pmm',seed = 500)
summary(imputemmb)
completeimp<-complete(imputemmb,2)
#test block ^^^^

#i used mice to impute the missing data for the 
#remaining columns. I used the Predictive mean matching
#because the values are numeric and non binary
imputedmm <- mice(mm,m=5,maxit = 50,method = 'pmm',seed=500)

model1 <- complete(imputedmm,1)
model2 <- complete(imputedmm,2)
model3 <- complete(imputedmm,3)
model4 <- complete(imputedmm,4)
model5 <- complete(imputedmm,5)

summary(model2)
summary(mm)

#visualizations
library(ggplot2)

ggplot(data=model1,aes(x=Age,fill=Severity==1))+
  geom_bar()

ggplot(data=model1,aes(x=Age,fill=Severity==1))+
  geom_histogram(bins = 7,col='black',alpha=0.5)+
  theme_classic()+
  labs(title= 'Histogram showing age ranges and Cancer Diagnosis',
       x='Age',y='Number of Cases')

#relationship between density and severity
ggplot(data=model1,aes(x=Density,fill=Severity==1))+
  geom_histogram()

ggplot(data = model1,aes(x=Severity==1,fill=Density))+
  geom_histogram()
