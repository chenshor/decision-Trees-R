# part 1
#reading a file to my data (data.frame) while changing all the 
# empty cells to NA
mydata = read.csv("Ex-2_dataset.csv", header=T, na.strings=c("","NA"))
mydata
library(zoo)
# all the NA values in the numeric type will become avarage
mydata$Loan_Amount=na.aggregate(mydata$Loan_Amount)
mydata$Monthly_Profit=na.aggregate(mydata$Monthly_Profit)
mydata$Payment_Terms=na.aggregate(mydata$Payment_Terms)
mydata$Spouse_Income=na.aggregate(mydata$Spouse_Income)

# all the NA values in the catagorial type will become 
# the most frequent value
  
mydata[, sapply(mydata, function(x) !is.numeric(x))] <-
  apply(mydata[, sapply(mydata, function(x) !is.numeric(x))], 2,
        function(x) {x[is.na(x)] <- names(sort(table(x), decreasing = TRUE)[1]); x})

maxCredit=Mode(mydata$Credit_History)
i=0
for(x in mydata$Credit_History){
  i=i+1
  if(is.na(x)==TRUE){
    mydata[i,"Credit_History"]=maxCredit
  }
}


# Discretization by equal frequency
# We used the function "classIntervals" to find the length of the intervals.
# We used the output length in the function "cut"

library(classInt)
monthlyProfitIntervals= classIntervals(mydata$Monthly_Profit, 5, style = 'quantile')


mydata$Monthly_Profit <- cut(mydata$Monthly_Profit, c(-Inf, 2624, 3418, 4336.6,6250, Inf), 
                             labels=c('low', 'mediumLow', 'mediumHigh','High', 'ExtraHigh'))

income=classIntervals(mydata$Spouse_Income, 4, style = 'quantile')


mydata$Spouse_Income <- cut(mydata$Spouse_Income, c(-Inf, 0, 1213, 2333,41667, Inf), 
                             labels=c('low', 'mediumLow', 'mediumHigh', 'High','ExtraHigh'))


LoanMountInterval=classIntervals(mydata$Loan_Amount, 3, style = 'quantile')


mydata$Loan_Amount <- cut(mydata$Loan_Amount, c(minLoan, 112, 151,  maxLoan), 
                            labels=c('low', 'medium', 'High'))



# random partition for training set and test set
library(caret)
inTrain<-createDataPartition(y=mydata$Request_Approved,p=0.7 ,list=FALSE)
training<-mydata[inTrain,]
testing<-mydata[-inTrain,]



# part 2
# building the model

training$Request_Number<-NULL
library(rpart)
library(rpart.plot)
library(rattle)
library(RColorBrewer)

gini50 <- rpart(
  Request_Approved ~ .,
  data = training, 
  method = "class",
  minsplit=50,
  minbucket=1,
  cp = -1
  
)
fancyRpartPlot(gini50, caption = NULL, tweak=0.9,cex=0.2)

gini80 <- rpart(
  Request_Approved ~ .,
  data = training, 
  method = "class",
  minsplit=80,
  minbucket=1,
  cp = -1
  
)
fancyRpartPlot(gini80, caption = NULL, tweak=0.9,cex=0.2)

information50 <- rpart(
  Request_Approved ~ .,
  data = training, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit=50,
  minbucket=1,
  cp = -1
  
)
fancyRpartPlot(information50, caption = NULL, tweak=0.9,cex=0.2)

information80 <- rpart(
  Request_Approved ~ .,
  data = training, 
  method = "class",
  parms = list(split = 'information'), 
  minsplit=80,
  minbucket=1,
  cp = -1
  
)
fancyRpartPlot(information80, caption = NULL, tweak=0.9,cex=0.2)


# part 3

predictgini50<- predict(gini50, newdata=testing)
predigini50<-(predictgini50[,"Y"] > predictgini50[,"N"])

i=0
for(x in predigini50){
  i=i+1
  if(x==TRUE){
    predigini50[i]="Y"
  }else{
    predigini50[i]="N"
  }
}


CMGini50<-confusionMatrix(table(predigini50,testing$Request_Approved))
CMGini50



# for information50

predictinfo50<- predict(information50, newdata=testing)
prediinformation50<-(predictinfo50[,"Y"] > predictinfo50[,"N"])
i=0
for(x in prediinformation50){
  i=i+1
  if(x==TRUE){
    prediinformation50[i]="Y"
  }else{
    prediinformation50[i]="N"
  }
}
CMInfo50<-confusionMatrix(table(prediinformation50,testing$Request_Approved))
CMInfo50

# for gini 80

predictgini80<- predict(gini80, newdata=testing)
predigini80<-(predictgini80[,"Y"] > predictgini80[,"N"])

i=0
for(x in predigini80){
  i=i+1
  if(x==TRUE){
    predigini80[i]="Y"
  }else{
    predigini80[i]="N"
  }
}


CMGini80<-confusionMatrix(table(predigini80,testing$Request_Approved))
CMGini80

# for information80

predictinfo80<- predict(information80, newdata=testing)
prediinformation80<-(predictinfo80[,"Y"] > predictinfo80[,"N"])
i=0
for(x in prediinformation80){
  i=i+1
  if(x==TRUE){
    prediinformation80[i]="Y"
  }else{
    prediinformation80[i]="N"
  }
}
CMInfo80<-confusionMatrix(table(prediinformation80,testing$Request_Approved))
CMInfo80