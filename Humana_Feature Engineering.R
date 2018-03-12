library(mlbench)
data = read.table(file="/Users/yangshenyang/Desktop/humanna/Data_Release/test.csv", header=TRUE, sep=",")

######################### Recode missing values to mode of some features ################
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

data$Est_Net_worth[is.na(data$Est_Net_worth)] <- getmode(data$Est_Net_worth)
data$Est_BMI_decile[is.na(data$Est_BMI_decile)] <- median(data$Est_BMI_decile, na.rm = TRUE)

####################### remove the rest of the rows with missing value ##################
data <- na.omit(data)

#################### convert categorical variables into factor variables #################
# ESRD_IND
data$esrd_IND[data$ESRD_IND=='Y'] <- 1
data$esrd_IND[data$ESRD_IND=='N'] <- 2
data <- na.omit(data)
data$ESRD_IND <- data$esrd_IND
data$esrd_IND <- NULL

# HOSPICE_IND
data$hospice_IND[data$HOSPICE_IND=='Y'] <- 1
data$hospice_IND[data$HOSPICE_IND=='N'] <- 2
data$HOSPICE_IND <- data$hospice_IND
data$hospice_IND <- NULL

# DUAL
data$dual[data$DUAL=='Y'] <- 1
data$dual[data$DUAL=='N'] <- 2
data$DUAL <- data$dual
data$dual <- NULL

# INSTITUTIONAL
data$institutional[data$INSTITUTIONAL=='Y'] <- 1
data$institutional[data$INSTITUTIONAL=='N'] <- 2
data$INSTITUTIONAL <- data$institutional
data$institutional <- NULL
# LIS
data$lis[data$LIS=='Y'] <- 1
data$lis[data$LIS=='N'] <- 2
data$LIS <- data$lis
data$lis <- NULL

####### Create new columns Convert the values of admission and readmission into 0 and 1########
data$ADMISSIONSnew[data$ADMISSIONS!="0"] <- 1
data$READMISSIONSnew[data$READMISSIONS!="0"] <- 1

##AGE
# par(mfrow=c(1,2))
# boxplot(data$AGE~data$ADMISSIONSnew,data=data, main="Age by Admission", 
#         xlab="Admission", ylab="Age")
# boxplot(data$AGE~data$READMISSIONSnew,data=data, main="Age by Readmission", 
#         xlab="Readmission", ylab="Age")



# add sum of all the flags
data$sum_con <- rowSums (data[107:362], na.rm = FALSE, dims = 1)
data$sum_pot <- rowSums (data[363:652],na.rm = FALSE, dims = 1)
data$sum_rx <- rowSums (data[653:921],na.rm = FALSE, dims = 1)


data$rxPerVist <- data$sum_rx / data$sum_con

# add selected sums of POT/CON/RX

selectConImp <- cbind(data[207],data[208],data[254],data[259],data[294],data[298],data[302],
                      data[310],data[322:326],data[342:344],data[331:334],data[351:362])
data$select_conImp <- rowSums (selectConImp,na.rm = FALSE, dims = 1)

selectPotImp <- cbind(data[558],data[579],data[601:604],data[610],data[616],data[617],data[621],data[637],data[597:599],
                      data[606],data[622],data[642],data[643],data[535],data[536],data[566],data[567],data[583],data[584],
                      data[593],data[594],data[532],data[550],data[551],data[570],data[581],data[633],data[484],data[503],
                      data[545],data[565],data[471],data[541],data[552],data[553],data[577],data[613],data[614],data[556],
                      data[561],data[562],data[571:575])
data$select_potImp <- rowSums (selectPotImp,na.rm = FALSE, dims = 1)

selectRxImp <- cbind(data[887],data[896],data[858],data[897],data[899],data[908],data[868:870],
                      data[909],data[917],data[919])
data$select_rxImp <- rowSums (selectRxImp,na.rm = FALSE, dims = 1)

# add sum of flags
data$flags2015 <- rowSums (data[31:57], na.rm = FALSE, dims = 1)
data$flags2014 <- rowSums (data[58:86], na.rm = FALSE, dims = 1)
data$flag1514 <- data$flags2015 - data$flags2014
data$flags <- data$flags2015 + data$flags2014
#data$flagsIncre <- data$flags2015 - data$flags2014

#select important
flags_all <- colSums (cbind(data[31:57],data[58:86]), na.rm = FALSE, dims = 1)
adm <- data[data[1]!=0, ]
flags_adm <- colSums (cbind(adm[31:57],adm[58:86]), na.rm = FALSE, dims = 1)
compare <- flags_adm / flags_all
result <- cbind(flags_all,flags_adm,compare)

data$flags2015Imp <- rowSums (data[31:57], na.rm = FALSE, dims = 1)
data$flags2014Imp <- rowSums (data[58:86], na.rm = FALSE, dims = 1)

##histograms##
hist(data$flags[data[1]!=0], col=rgb(1,0,0,0.5), ylim=c(0,10000),main='Overlapping Histogram')

hist(data$flags[data[1]==0], col=rgb(0,0,1,0.5), add=T)

summary(data$flags[data[1]!=0])
summary(data$flags[data[1]==0])

hist(data$flag1514[data[1]!=0], col=rgb(1,0,0,0.5), ylim=c(0,10000),main='Overlapping Histogram')
hist(data$flag1514[data[1]==0],col=rgb(0,0,1,0.5), add=T)

bbb<-data$flag1514[data[1]!=0]
aaa<-data$flag1514[data[1]==0]

bbb<-data$Est_BMI_decile[data[1]!=0]
aaa<-data$Est_BMI_decile[data[1]==0]

boxplot(aaa, bbb, horizontal = TRUE, main = "Ages of Oscar Winning Actors", xlab = "Age")

hist(data$Est_BMI_decile[data[1]!=0])
(data$Est_BMI_decile[data[1]==0])

hist(aaa, col=rgb(1,0,0,0.5), ylim=c(0,8000),main='Overlapping Histogram')
hist(bbb,col=rgb(0,0,1,0.5), add=T)


sum(data$flags2015)
sum(data$flags2014)
summary(data$flags2014[data[1]!=0])
summary(data$flags2014[data[1]==0])
#gender
summary(data$SEX_CD[data[1]!=0])
summary(data$SEX_CD[data[1]==0])

#MONTHS_2016
summary(data$MONTHS_2016[data[1]!=0])
summary(data$MONTHS_2016[data[1]==0])
table(data$MONTHS_2016)

##ESRD
summary(data$ESRD_IND[data[1]!=0])
summary(data$ESRD_IND[data[1]==0])


##HOSPICE_IND
summary(data$HOSPICE_IND[data[1]!=0])
summary(data$HOSPICE_IND[data[1]==0])


#ORIG_REAS_ENTITLE_CD
table(data$ORIG_REAS_ENTITLE_CD[data[1]!=0])
table(data$ORIG_REAS_ENTITLE_CD[data[1]==0])

##PCP_ASSIGNMENT
table(data$PCP_ASSIGNMENT[data[1]!=0])
table(data$PCP_ASSIGNMENT[data[1]==0])

##DUAL
table(data$DUAL[data[1]!=0])
table(data$DUAL[data[1]==0])

#INSTITUTIONAL
table(data$INSTITUTIONAL[data[1]!=0])
table(data$INSTITUTIONAL[data[1]==0])

#LIS
table(data$LIS[data[1]!=0])
table(data$LIS[data[1]==0])

#MAJOR_GEOGRAPHY
summary(data$MAJOR_GEOGRAPHY[data[1]!=0])
summary(data$MAJOR_GEOGRAPHY[data[1]==0])

summary(data$MINOR_GEOGRAPHY)

# MCO_HLVL_PLAN_CD

summary(data$MCO_HLVL_PLAN_CD)
summary(data$MCO_HLVL_PLAN_CD[data[1]!=0])
summary(data$MCO_HLVL_PLAN_CD[data[1]==0])

#MCO_PROD_TYPE_CD
summary(data$MCO_PROD_TYPE_CD)
summary(data$MCO_PROD_TYPE_CD[data[1]!=0])
summary(data$MCO_PROD_TYPE_CD[data[1]==0])

#ACE_ELIG_2014/15
table(data$ACE_ELIG_2014)
table(data$ACE_ELIG_2014[data[1]!=0])
table(data$ACE_ELIG_2014[data[1]==0])

table(data$ACE_ELIG_2015[data[1]!=0])
table(data$ACE_ELIG_2015[data[1]==0])

table(data$ACE_PASS_2014[data[1]!=0])
table(data$ACE_PASS_2014[data[1]==0])

table(data$ACE_PASS_2015[data[1]!=0])
table(data$ACE_PASS_2015[data[1]==0])

table(data$DIAB_ELIG_2014[data[1]!=0])
table(data$DIAB_ELIG_2014[data[1]==0])

table(data$DIAB_PASS_2014[data[1]!=0])
table(data$DIAB_PASS_2014[data[1]==0])

# Find who dont adhere
data$adhHT2014 <- data$ACE_ELIG_2014 - data$ACE_PASS_2014
data$adhHT2015 <- data$ACE_ELIG_2015 - data$ACE_PASS_2015

data$adhDI2014 <- data$DIAB_ELIG_2014 - data$DIAB_PASS_2014
data$adhDI2015 <- data$DIAB_ELIG_2015 - data$DIAB_PASS_2015

data$adhHL2014 <- data$STATIN_ELIG_2014 - data$STATIN_PASS_2014
data$adhHL2015 <- data$STATIN_ELIG_2015 - data$STATIN_PASS_2015

data$totalNotAdh <- data$adhHT2014 + data$adhHT2015 + data$adhDI2014 + data$adhDI2015 + data$adhHL2014 +data$adhHL2015



table(data$totalNotAdh[data[1]!=0])
table(data$totalNotAdh[data[1]==0])

summary(data$totalNotAdh[data[1]!=0])
summary(data$totalNotAdh[data[1]==0])

#income
table(data$Est_income[data[1]!=0])
table(data$Est_income[data[1]==0])

hist(data$Est_income[data[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,20000))
hist(data$Est_income[data[1]==0],col=rgb(0,0,1,0.5),add=T)

summary(data$Est_Net_worth[data[1]!=0])
summary(data$Est_Net_worth[data[1]==0])

hist(data$Est_Net_worth[data[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,20000))
hist(data$Est_Net_worth[data[1]==0],col=rgb(0,0,1,0.5),add=T)


hist(data$Pct_above_poverty_line[data[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,8000))
hist(data$Pct_above_poverty_line[data[1]==0],col=rgb(0,0,1,0.5),add=T)

summary(data$Home_value[data[1]!=0])
summary(data$Home_value[data[1]==0])

#education

table(data$Education_level[data[1]!=0])
table(data$Education_level[data[1]==0])

hist(data$Education_level[data[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,30000))
hist(data$Education_level[data[1]==0],col=rgb(0,0,1,0.5),add=T)

count(data$College[data[1]!=0])
count(data$College[data[1]==0])

#Decile_struggle_Med_lang
summary(data$Decile_struggle_Med_lang[data[1]!=0])
summary(data$Decile_struggle_Med_lang[data[1]==0])

#Index_Health_ins_engage

summary(data$Index_Health_ins_engage[data[1]!=0])
summary(data$Index_Health_ins_engage[data[1]==0])

#Index_Health_ins_influence
summary(data$Index_Health_ins_influence[data[1]!=0])
summary(data$Index_Health_ins_influence[data[1]==0])

#sum_con
summary(data$select_con[data[1]!=0])
summary(data$select_con[data[1]==0])

summary(data$select_potImp[data[1]!=0])
summary(data$select_potImp[data[1]==0])

summary(data$select_rxImp[data[1]!=0])
summary(data$select_rxImp[data[1]==0])

summary(data$select_conImp[data[1]!=0])
summary(data$select_conImp[data[1]==0])

summary(data$rxPerVist[data[1]!=0])
summary(data$rxPerVist[data[1]==0])

hist(data$rxPerVist[data[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,30000))
hist(data$rxPerVist[data[1]==0],col=rgb(0,0,1,0.5),add=T)

data$rxPerVist[data$rxPerVist==Inf] <- 0
summary(data$rxPerVist)

#CDC_2014
count(data$sum_cdc[data[1]!=0])
summary(data$sum_cdc[data[1]==0])
data$sum_cdc <- rowSums (data[85:106],na.rm = FALSE, dims = 1)

cdc_Im <- cbind(data[87:89],data[98],data[99])
data$sum_cdcIm <- rowSums (cdc_Im,na.rm = FALSE, dims = 1)

 

cdc2014 <- rowSums (data[85:93],na.rm = FALSE, dims = 1)
cdc2015 <- rowSums (data[94:106],na.rm = FALSE, dims = 1)
sum(cdc2014)
sum(cdc2015)

summary(data$sum_cdcIm[data[1]!=0])
summary(data$sum_cdcIm[data[1]==0])

new <- data[940:946]

# names
aa <- names(data)
as.data.frame(aa)

write.table(aa, "/Users/yangshenyang/Desktop/names111.csv", sep=",")

## delete columns

trail2 <- data

trail2[960] <- NULL
trail2[951:958] <- NULL
trail2[947:949] <- NULL
trail2[943] <-NULL
trail2[938] <- NULL
trail2[939] <- NULL
trail2[924:936] <- NULL
trail2[15:922] <- NULL
trail2[11] <- NULL
trail2[12] <- NULL
trail2[7] <- NULL
trail2[2:5] <- NULL

####### Create new columns Convert the values of admission and readmission into 0 and 1########
data$ADMISSIONSnew[data$ADMISSIONS!="0"] <- 1
data$READMISSIONSnew[data$READMISSIONS!="0"] <- 1

trail2$ADMISSIONSnew[trail2$ADMISSIONS!="0"] <- 1
trail2$ADMISSIONSnew[is.na(trail2$ADMISSIONSnew)] <- 0
table(trail2$ADMISSIONSnew)

trail2$ADMISSIONS[trail2$ADMISSIONS!="0"] <- 1
trail2$ADMISSIONSnew<- NULL

train$signal <- ifelse(train$signal == 1, "X", "Y")
test$signal <- ifelse(test$signal == 1, "X", "Y")
# trail2$READMISSIONSnew[trail2$READMISSIONS!="0"] <- 1

## model
library(caret)
library(plyr)
library(ipred)
library(e1071)
library(pROC)
library(gbm)
library(survival)
library(splines)
library(parallel)
library(randomForest)
library(MASS)
library(e1071)
library(ggplot2)


set.seed(2016)
split <- createDataPartition(trail2$ADMISSIONS, p=0.7, list=F)
traindf <- trail2[split,]
testdf <-  trail2[-split,]

traindf$ADMISSIONS <- ifelse(traindf$ADMISSIONS == 1, "X", "Y")
testdf$ADMISSIONS <- ifelse(testdf$ADMISSIONS == 1, "X", "Y")

############################   SVM RBF #######################
# Training SVM Models
library(dplyr)         # Used by caret
library(kernlab)       # support vector machine 
library(pROC)	       # plot the ROC curves


# Setup for cross validation
trainX=traindf[,2:18]

ctrlSVM <- trainControl(method="repeatedcv",   # 10fold cross validation
                        repeats=3,		    # do 5 repititions of cv
                        summaryFunction=twoClassSummary,	# Use AUC to pick the best model
                        classProbs=TRUE)

svcvmod <- train(x=trainX,
                 y= traindf$ADMISSIONS,
                 method = "svmRadial",   # Radial kernel svmRadialWeights
                 tuneLength = 9,					# 9 values of the cost function
                 preProc = c("center","scale"),  # Center and scale data
                 metric="ROC",
                 trControl=ctrlSVM)

#Predict on training data

svcvProbs1 <- predict(svcvmod, newdata=trainX, type="prob")[,1]
svcvClasses1 <- predict(svcvmod, newdata=trainX)
confusionMatrix(data=svcvClasses1, traindf$ADMISSIONS)

#Predict on testing data
testX=testdf[,2:18]
svcvProbs <- predict(svcvmod, newdata=testX, type="prob")[,1]
svcvClasses <- predict(svcvmod, newdata=testX)
confusionMatrix(data=svcvClasses, testdf$ADMISSIONS)

#find the roc curve on testing data

# rocCurvesvcv1 <- roc (response = traindf$Made.Donation.in.March.2007,
#                       predictor = svcvProbs1, levels = rev(levels(traindf$Made.Donation.in.March.2007)))
# par(mfrow=c(1,1))
# plot(rocCurvesvcv1, legacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white", bty= "n",
#        cex = .8, legend = c("SVM(RBF)"), fill = c("red"))
# auc(rocCurvesvcv1)


#find the roc curve on testing data
# rocCurvesvcv <- roc (response = testdf$Made.Donation.in.March.2007, 
#                      predictor = svcvProbs, levels = rev(levels(testdf$Made.Donation.in.March.2007)))
# par(mfrow=c(1,1))
# plot(rocCurvesvcv, legacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white", bty= "n", 
#        cex = .8, legend = c("SVM(RBF)"), fill = c("red"))
# auc(rocCurvesvcv)

###################################  ANN MLP ######################################
install.packages("RSNNS")
library(RSNNS)

ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary)

mlpmod1 <- train(ADMISSIONS ~.,
                 data=traindf,
                 method = "mlp",
                 preProc = c("center","scale"),  # Center and scale data
                 tuneLength = c(1:5),           # Porf. add this argument ,need to changed the classifiction type
                 metric="ROC",                   
                 trControl=ctrl)

myGird <- expand.grid(size = c(5,10,20),decay = c(0,.10,.15))

ann <- train(ADMISSIONS ~ .,
             data = traindf,
             method = "nnet",
             trControl = ctrl,
             tuneGrid= myGird,
             metric = "ROC", 
             preProc = c("center","scale"),
             maxint=250
)

#finding consusion matrix on training data

annProbs1 <- predict(ann, newdata=traindf, type="prob")[,1]
annClasses1 <- predict(ann, newdata=traindf)
confusionMatrix(data=annClasses1, traindf$ADMISSIONS)

#inding consusion matrix on testing data

bagProbs <- predict(bagmod, newdata=testdf, type="prob")[,1]
bagClasses <- predict(bagmod, newdata=testdf)
confusionMatrix(data=bagClasses, testdf$ADMISSIONS)


##########################  logistic bagged ######################### 
#Bagging

bagmod <- train(ADMISSIONS~.,
             data=traindf,
             method="treebag",
             preProc = c("center","scale"),
             trControl=trainControl(method="cv",number=30))

#finding consusion matrix on training data

bagProbs1 <- predict(bagmod, newdata=traindf, type="prob")[,1]
bagClasses1 <- predict(bagmod, newdata=traindf)
confusionMatrix(data=bagClasses1, traindf$ADMISSIONS)

#inding consusion matrix on testing data

bagProbs <- predict(bagmod, newdata=testdf, type="prob")[,1]
bagClasses <- predict(bagmod, newdata=testdf)
confusionMatrix(data=bagClasses, testdf$ADMISSIONS)

#find the roc curve on training data

# rocCurvebag1 <- roc (response = traindf$ADMISSIONS,
#                      predictor = bagProbs1,
#                      levels = rev(levels(traindf$ADMISSIONS)))
# par(mfrow=c(1,1))
# plot(rocCurvebag1, le1gacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white",
#        bty= "n", cex = .8, legend = c("Bagging"), fill = c("red"))
# auc(rocCurvebag1)



#find the roc curve on testing data

# rocCurvebag <- roc (response = testdf$Made.Donation.in.March.2007,
#                     predictor = bagProbs, levels = rev(levels(testdf$Made.Donation.in.March.2007)))
# par(mfrow=c(1,1))
# plot(rocCurvebag, le1gacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white", 
#        bty= "n", cex = .8, legend = c("Bagging"), fill = c("red"))
# auc(rocCurvebag)

####################################  c5.0  ########################################
install.packages("c50")
library(C50)
c5mod1 <- train(ADMISSIONs~.,
                traindf,
                method = "C5.0",
                preProc = c("center","scale"),  # Center and scale data
                metric="ROC",
                trControl=ctrl)
summary(c5mod1)

#Predict on training data

C5Probs1 <- predict(c5mod1, newdata=traindf, type="prob")[,1]
c5Classes1 <- predict(c5mod1, newdata=traindf)
confusionMatrix(c5Classes1, traindf$ADMISSIONS)

#find the roc curve on testing data

# rocCurvec51 <- roc (response = traindf$Made.Donation.in.March.2007,
#                     predictor = C5Probs1, levels = rev(levels(traindf$Made.Donation.in.March.2007)))
# par(mfrow=c(1,1))
# plot(rocCurvec51, legacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white", bty= "n", cex = .8, legend = c("C5"), fill = c("red"))
# auc(rocCurvec51)

#Predict on testing data

c5Probs <- predict(c5mod1, newdata=testdf, type="prob")[,1]
c5Classes <- predict(c5mod1, newdata=testdf)
confusionMatrix(c5Classes, testdf$ADMISSIONS)

#find the roc curve on testing data
# 
# rocCurvec5 <- roc (response = testdf$Made.Donation.in.March.2007,
#                    predictor = c5Probs, levels = rev(levels(testdf$Made.Donation.in.March.2007)))
# par(mfrow=c(1,1))
# plot(rocCurvec5, legacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white", bty= "n", cex = .8, legend = c("C5"), fill = c("red"))
# auc(rocCurvec5)

