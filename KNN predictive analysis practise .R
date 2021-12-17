bc <- read.csv('c:/users/lewis/downloads/breast-cancer-wisconsin.data')
bc <- data.frame(bc)
head(bc)
colnames(bc)<-c('id','Clump Thickness','Uniformity of Cell Size','Uniformity of Cell shape','Marginal Adhesion','Single Epithelial Cell Size','Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses','Class')

#str to see the structure of the data including the data types
str(bc)
bc$id<-NULL 
#to remove unwanted column

class(bc$`Bare Nuclei`) = 'integer'
#identify complete cases
bc <- bc[complete.cases(bc),]
summary(bc)
#transform binary classifiers from numbers to meaningful descriptions

bc$class <- factor(ifelse(bc$Class==2,'benign','malignant'))


#build the model splice the data into train and test set 80/20

trainingset <- bc[1:477,1:9]
testset <- bc[478:682,1:9]
# do not include the output variable in the training data and testing data

trainingoutcomes <- bc[1:477,10]
testoutcomes <- bc[478:682,10]

#apply ml algorithm
# knn algorithm
library(class)
predictions <- knn(train = trainingset, cl= trainingoutcomes,k=21,
                   test=testset)

# k = 21 is the square root of the amount of variables in the training set
predictions

#model evaluation
table(testoutcomes,predictions)
RMSE = mean((testoutcomes - predictions)^2)+sqrt()

# try knn on iris data

str(iris)
summary(iris)

iristrainingset <- iris[1:100,1:4]
iristestingset <- iris[101:150,1:4]

iristrainingoutcome <- iris[1:100,5]
iristestoutcome <- iris[101:150,5]

irispredict <- knn(train = iristrainingset, cl=iristrainingoutcome, k=5,
                  test=iristestingset)
irispredict

table(iristestoutcome,irispredict)

