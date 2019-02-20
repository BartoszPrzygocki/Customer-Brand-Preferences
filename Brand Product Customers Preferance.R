#Customer Brand Preferance Prediction 
#Bartosz Przygocki 
#Ubiqum Code Acadmy November 2018 




###########################################
library(doParallel)
# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)
registerDoParallel(cl)
on.exit(stopCluster(cl))
getDoParWorkers()
#####Librarys##########################
library(readxl)#loading library that will help to open Excel file
library(rattle)
library(rpart.plot)
library(caret)
library(esquisse)
install.packages("ROSE")
library(ROSE)
library(rpart)
library(dplyr)
library(plotly)
library(rpart)
#################Load file 

#load csv excel file / use second sheet
SurveyComplete <- read_excel("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task2/Survey_Key_and_Complete_Responses_excel.xlsx", sheet = "Survey Results Complete")

#data exploration
attributes(SurveyComplete)   #lists attibutes
summary(SurveyComplete)      #prints min, max, median, mean, etc. each attribute
str(SurveyComplete)          #displays the structure of the data set
names(SurveyComplete)        #names your attributes
dim(SurveyComplete)          #dimensions


#labels for survey according to the key  

SurveyComplete$elevel <- factor(SurveyComplete$elevel,
                        levels = c(0,1,2,3,4),
                        labels = c("less than high school","high school","some college","college dregree","Master's,doctorate or professional degree"),
                        ordered = TRUE)
table(SurveyComplete$elevel)
min(SurveyComplete$elevel)
SurveyComplete$car<-factor(SurveyComplete$car,
                            levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                            labels=c("BMW","Buick","Cadillac","Chevrolet","Chrysler","Dodge","Ford","Honda","Hyundai",
                                     "Jeep","Kia","Lincoln","Mazda","Mercedes Benz","Mitsubishi","Nissan","Ram","Subaru","Toyota","None of the above"))
table(SurveyComplete$car)
SurveyComplete$zipcode<- factor(SurveyComplete$zipcode,
                                 levels=c(0,1,2,3,4,5,6,7,8),
                                 labels=c("New England","Mid-Atlantic","East North Central","West North Central","South Atlantic",
                                          "East South Central","West South Central","Mountain","Pacific"))

table(SurveyComplete$zipcode)
SurveyComplete$brand<- factor(SurveyComplete$brand,
                              levels = c(0,1),
                              labels = c("Acer","Sony"))

table(SurveyComplete$brand)





########Bins for the numerical attributes #using bins lowers accuracy and kappa in this example 

# summary(SurveyComplete$salary)
# summary(SurveyComplete$age)
# 
# b <- c(-Inf, 52109, 84969,117168,Inf)
# names <- c("20k-50k","50k-80k","80k-100k","100k-150K")
# salaryBins <- cut(SurveyComplete$salary, breaks = b, labels = names)
# 
# a<-c(-Inf,35, 50,65,Inf)
# namesAge <- c("20y-35y","35y-50y","50y-65y","65y-80y")
# ageBins<-cut(SurveyComplete$age,breaks = a,labels = namesAge)
# table(ageBins)
# 
# summary(SurveyComplete$credit)
# 
# c<-c(-Inf,121155,250607,374872,Inf)
# namesCredit<-c("0k-120k","120k-250k","250k-370k","370k-500k")
# creditBins<- cut(SurveyComplete$credit,breaks=c,labels=namesCredit)
# table(creditBins)


# str(SurveyComplete)
# SurveyCompleteBins<-SurveyComplete
# SurveyCompleteBins$salary<-salaryBins
# SurveyCompleteBins$credit<-creditBins
# SurveyCompleteBins$age<-ageBins

sampleNoBins<-SurveyComplete[sample(1:nrow(SurveyComplete), 1000, replace=FALSE),]
# sampleBINS <-SurveyCompleteBins[sample(1:nrow(SurveyCompleteBins), 1000, replace=FALSE),]


set.seed(123)



#Recursive Feature Selection to choose best atributes 
ctrl <- rfeControl(functions = rfFuncs, # control for RFE only 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)
ncol(SurveyCompleteBins)

#using bins lowers accuracy and kappa in this example 
# rfeBrandBINS <- rfe(sampleBINS[,1:6], 
#                             sampleBINS$brand, 
#                             sizes=(1:6), 
#                             rfeControl=ctrl)
rfeBrand <- rfe(sampleNoBins[,1:6], 
                    sampleNoBins$brand, 
                    sizes=(1:6), 
                    rfeControl=ctrl)
plot(rfeBrand,type=c("g", "o"))
#best atributes for the model are salary, age



#Test and Training for models 

#checking if data is balanced 
table(SurveyComplete$brand)
prop.table(table(SurveyComplete$brand))
table(training$brand)
prop.table(table(training$brand))

treeimb <- rpart(brand ~ ., data = training)
pred.treeimb <- predict(treeimb, newdata = testing)
accuracy.meas(testing$brand, pred.treeimb[,2])
roc.curve(testing$brand, pred.treeimb[,2], plotit = F)
# precision: 0.939 
# recall: 0.944
# Area under the curve (AUC): 0.948
# all the values are high hence no need to balance the data 


inTraining <- createDataPartition(SurveyComplete$brand, p = .80, list = FALSE)
training <- SurveyComplete[ inTraining,]
testing  <- SurveyComplete[-inTraining,]





#10-fold cross validation, repeated 3 times
Control <- trainControl(method = "repeatedcv", number = 10, repeats = 3,
                           classProbs=TRUE) 



#######models using train to get best parameters with tuneLength set to 3.

#C50 
C50AcerSonyALL<- train(brand~., data=training, method= "C5.0",tuneLength = 3, trControl = Control,preProc = c("center", "scale"))
C5IAcerSonyALL_predict<-predict(C50AcerSonyALL, testing)
confusionMatrix(testing$brand, C5IAcerSonyALL_predict)
plot(C50AcerSonyALL)
summary(C50AcerSonyALL$finalModel)
varImp(C50AcerSonyALL)
# Confusion Matrix and Statistics
# Prediction Acer Sony
# Acer       683   73
# Sony        74 1169
# 
# Accuracy : 0.9265          
# Kappa : 0.8437    

C50AcerSonySA<- train(brand~+salary+age, data=training, method= "C5.0",tuneLength = 3, trControl = Control,preProc = c("center", "scale"))
C5IAcerSonySA_predict<-predict(C50AcerSonySA, testing)
confusionMatrix(testing$brand, C5IAcerSonySA_predict)
plot(C50AcerSonySA)
summary(C50AcerSonySA$finalModel)
# Confusion Matrix and Statistics
# Prediction Acer Sony
# Acer        676   80
# Sony         60 1183
# 
# Accuracy : 0.93            
# Kappa : 0.8503          


RFAcerSonyALL<- train(brand~., data=training, method= "rf", tuneLength = 3,trControl = Control,preProc = c("center", "scale"))
RFAcerSonyALL_predict<-predict(RFAcerSonyALL, testing)
confusionMatrix(testing$brand, RFAcerSonyALL_predict)
plot(RFAcerSonyALL)
summary(RFAcerSonyALL$finalModel)
varImp(RFAcerSonyALL)
# Confusion Matrix and Statistics
# Prediction Acer Sony
# Acer  688   68
# Sony   73 1170
# Accuracy : 0.9295          
# Kappa : 0.8502       


RFAcerSonySA<- train(brand~ +salary+age, data=training, method= "rf",tuneLength = 3, trControl = Control,preProc = c("center", "scale"))
RFAcerSonySA_predict<-predict(RFAcerSonySA, testing)
confusionMatrix(testing$brand, RFAcerSonySA_predict)
plot(RFAcerSonySA)
summary(RFAcerSonySA$finalModel)
varImp(RFAcerSonySA)
# Confusion Matrix and Statistics

# Reference
# Prediction Acer Sony
# Acer  674   82
# Sony   77 1166
# 
# Accuracy : 0.9205          
# Kappa : 0.8307   




#KNN
KNN_SonyAcerALL <- train(brand~., data=training, method= "kknn", trControl = Control,preProc = c("center", "scale"))
KNNAcerSonyALL_predict<-predict(KNN_SonyAcerALL, testing)
confusionMatrix(testing$brand, KNNAcerSonyALL_predict)
plot(KNN_SonyAcerALL)
summary(KNN_SonyAcerALL$finalModel)

# Confusion Matrix and Statistics
# Prediction Acer Sony
# Acer  384  372
# Sony  424  819
# Accuracy : 0.6018        
# Kappa : 0.1646        

KNN_SonyAcerSA <- train(brand~+salary+age, data=training, method= "kknn", trControl = Control,preProc = c("center", "scale"))
KNNAcerSonySA_predict<-predict(KNN_SonyAcerSA, testing)
confusionMatrix(testing$brand,KNNAcerSonySA_predict)
plot(KNN_SonyAcerSA)
summary(KNN_SonyAcerSA$finalModel)

# Confusion Matrix and Statistics
# Prediction Acer Sony
# Acer        685   71
# Sony         83 1160
# Accuracy : 0.923           
# Kappa : 0.8367   



########Using tuneGrid for manual tuning 
grid <- expand.grid(.mtry=c(1,2,3,4,5))



#Randome Forest 

RFAcerSonyALLM<- train(brand~., data=training, method= "rf", tuneGrid=grid,trControl = Control,preProc = c("center", "scale"))
RFAcerSonyALLM_predict<-predict(RFAcerSonyALLM, testing)
confusionMatrix(testing$brand, RFAcerSonyALLM_predict)
plot(RFAcerSonyALLM)
summary(RFAcerSonyALLM$finalModel)
varImp(RFAcerSonyALLM)

RFAcerSonySAM<- train(brand~+salary+age, data=training, method= "rf", tuneGrid=grid,trControl = Control,preProc = c("center", "scale"))
RFAcerSonySAM_predict<-predict(RFAcerSonySAM, testing)
confusionMatrix(testing$brand, RFAcerSonySAM_predict)
plot(RFAcerSonySAM)
summary(RFAcerSonySAM$finalModel)
varImp(RFAcerSonySAM)






######compare the models 


resampsAll <- resamples(list(
  C50AllTlenght=C50AcerSonyALL,
  C50SalaryAgeTlenght=C50AcerSonySA,
  RFAllTlenght=RFAcerSonyALL,
  RFSalaryAgeTlenght=RFAcerSonySA,
  KNNALLTlenght=KNN_SonyAcerALL,
  KNNSalaryAgeTlenght=KNN_SonyAcerSA
  RFAllTgrid=RFAcerSonyALLM,
  RFSalaryAgeTgrid=RFAcerSonySAM
  ))
summary(resampsAll)

bwplot(resampsAll, layout = c(3, 1))
dotplot(resampsAll, metric = "Kappa")
dotplot(resampsAll, metric = "Accuracy")


differance <- diff(resampsAll)
summary(differance)

##########################Using model to complete the survey 

SurveyINComplete <- read.delim("/Users/BP/desktop/Ubiqium/Data Analytics/2.Data Analytics Predicting Customer Preferences/Task2/SurveyIncomplete.csv",",",header=TRUE)

#data exploration
attributes(SurveyINComplete)   #lists attibutes
summary(SurveyINComplete)      #prints min, max, median, mean, etc. each attribute
str(SurveyINComplete)          #displays the structure of the data set
names(SurveyINComplete)        #names your attributes
dim(SurveyINComplete)          #dimensions

SurveyINComplete$elevel <- factor(SurveyINComplete$elevel,
                                levels = c(0,1,2,3,4),
                                labels = c("less than high school","high school","some college","college dregree","Master's,doctorate or professional degree"),
                                ordered = TRUE)
table(SurveyINComplete$elevel)
min(SurveyINComplete$elevel)
SurveyINComplete$car<-factor(SurveyINComplete$car,
                           levels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                           labels=c("BMW","Buick","Cadillac","Chevrolet","Chrysler","Dodge","Ford","Honda","Hyundai",
                                    "Jeep","Kia","Lincoln","Mazda","Mercedes Benz","Mitsubishi","Nissan","Ram","Subaru","Toyota","None of the above"))
table(SurveyINComplete$car)
SurveyINComplete$zipcode<- factor(SurveyINComplete$zipcode,
                                levels=c(0,1,2,3,4,5,6,7,8),
                                labels=c("New England","Mid-Atlantic","East North Central","West North Central","South Atlantic",
                                         "East South Central","West South Central","Mountain","Pacific"))

table(SurveyINComplete$zipcode)



#SurveyINComplete$brand<- factor(SurveyINComplete$brand,
                             # levels = c(0,1),
                             # labels = c("Acer","Sony"))


prediction_Survey<-predict(C50AcerSonySA, SurveyINComplete)
table(prediction_Survey)
SurveyINComplete$brand<-prediction_Survey
table(SurveyINComplete$brand)

ggplot(SurveyComplete, aes(salary, age, color = brand)) + 
  geom_jitter() +
  scale_color_manual(breaks = c("Acer", "Sony"),
                     values=c("lightgreen", "steelblue"))+
  labs(x="Salary",
       y="Age",
       title = "Brand Preferences",
       subtitle="Complete Survey",
       color="Brand")

#predicted
ggplot(SurveyINComplete, aes(salary, age, color = brand)) + 
  geom_jitter() +
  scale_color_manual(breaks = c("Acer", "Sony"),
                     values=c("lightgreen", "steelblue"))+
  labs(x="Salary",
       y="Age",
       title = "Predicted brand preferences",
       subtitle="Incomplete Survey",
       color="Predicted Brand")


PREDICTED <- table(SurveyINComplete$brand)
KNOWN<-table(SurveyComplete$brand)
ALL<-PREDICTED+KNOWN

# Pie Chart with Percentages
pie(PREDICTED, labels = c("Acer", "Sony"), main="Brand preferences")
pie(KNOWN,labels = c("Acer", "Sony"), main="Brand preferences")

barchart(PREDICTED)



pieData <- data.frame(COM = c("Acer", "Sony"), 
                      values=c(ALL))

colors <- c('rgb(211,94,96)', 'rgb(128,133,133)')


# create pie chart
plot_ly(pieData, labels = ~COM, values = ~ values, type = "pie",
        textposition = 'inside',
        textinfo = 'label+percent',
        insidetextfont = list(color = '#FFFFFF'),
        hoverinfo = 'text',
        text = ~paste( values),
        marker = list(colors = colors,
                      line = list(color = '#FFFFFF', width = 1)),
        showlegend = F) %>%
  layout(title = 'Brand Preferance', 
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))














