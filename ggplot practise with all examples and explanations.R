library(ggplot2)
data(iris)
library(dplyr)
library(imputeTS)
View(iris)
table(iris$Species)


#scatterplot
plot(iris$Sepal.Length~iris$Petal.Length,ylab="Petal Length",xlab='Sepal Length',
     main='Sepal Length vs Petal Length',col='blue',pch=16)

hist(iris$Sepal.Width)
hist(iris$Sepal.Width,xlab='Sepal Width',main='Distribution of sepal width',col='aquamarine3')

boxplot(data =iris, Sepal.Length~Species,xlab = 'species',col='brown',ylab='Sepal Length',main = 'Sepal Length of Different Species')
################################################
scatter<-ggplot(iris,aes(x=Sepal.Length,y=Petal.Length,col=Species))+
  geom_point(aes(shape=Species))+
  theme_classic()

box<-ggplot(iris,aes(x=Species, y=Sepal.Length))+
  geom_boxplot(aes(fill=Species))+
  labs(fill='Species',x='Species',y='Sepal Length',xticks=TRUE,title='Species vs Sepal Length',
       caption= 'Data Source: iris')+
  theme_classic()+
  theme(plot.title = element_text(hjust=0.5,face='bold',size=(15),color='steelblue'),
        legend.title = element_text(colour = 'aquamarine3'),
        axis.text = element_text(colour = 'cornflowerblue'),
        axis.title = element_text(colour = 'aquamarine3'))

line<- ggplot(iris,aes(x=Petal.Length,y=Sepal.Length,group=Species))+
  geom_line(aes(col=Species))+
  geom_point(aes(shape=Species),size=0.5)+
  geom_text(aes(label=Sepal.Length))+
  theme_classic()
#############
install.packages('gridxtra')
library(gridExtra)
##########

#Fill refers to the bar fill or inner colour
#fill makes no sense for scatter plot
#color refers to the line and point colour

grid.arrange(box,scatter,line,ncol=2)

#house data set

data(houserates)

house <- read.csv('c:/users/lewis/downloads/housingdata.csv')

house <- data.frame(house)
head(house)
summary(house)

colnames(house) <- c('CRIM','ZN','INDUS','CHAS','NOX','RM','AGE','DIS','RAD','TAX','PTRATIO','B','LSTAT','MEDV')

ggplot(data=house,aes(x=MEDV))+
  geom_histogram(col='white',fill='red',bins = 50,aes(y=..density..))+
  geom_density()+
  labs(x='Median Cost Per house in 1000\'s')+
  theme_classic()

#frequency polygon
ggplot(data=house,aes(x=MEDV))+
  geom_freqpoly()

###
# if colour and fill are not matching up to the parameters,
# remove it from aes

ggplot(data=iris,aes(x=Petal.Length,fill=Species))+
  geom_bar(alpha=0.5,position='fill')+
  theme_classic()
#box plot

ggplot(data = iris,aes(x=Species,y=Sepal.Width))+
  geom_boxplot(aes(fill=Species))+
  labs(title='Distribution of Sepal width per Species',
       y='Sepal Width',fill='Key'    )+
  theme_classic()+
  theme(panel.background = element_rect(fill='white'),
    plot.title = element_text(hjust=0.5,color='aquamarine3',size=15,face='bold'),
        legend.title = element_text(colour='aquamarine3'),
        legend.text = element_text(colour = 'black'),
        axis.title = element_text(colour='aquamarine3',size=12),
        axis.text = element_text(colour = 'black',size=10)
        )
# scale_y_continuous(labels= dollar) this changes the y values to dollar


#smooth line

ggplot(house,aes(y=TAX,x=AGE,col=factor(CHAS)))+
  geom_point()+
  geom_smooth(se=F,method='lm')+
  facet_grid(~CHAS)

#facet grid splits the chart into categories depending on the amount
# of categorical variables available

#se determines the wings

#use factor keyword inside aes(), to convert a variable to a categorical format
