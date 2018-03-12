###############################################################################################################
#####                                    Load the libraries and data                                      #####
###############################################################################################################
#----

### load the library
library(caret)
library(mlbench)
library(ggplot2)
library(dplyr)        # Used by caret
library(kernlab)      # support vector machine 
library(pROC)	        # plot the ROC curves

### Define some funtions 
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#### load the dataset
dataset <- read.table(file="/Users/yangshenyang/Desktop/humanna/Data_Release/TAMU_FINAL_SUBSET.csv",
                      header=T, sep = ',')

#----
###############################################################################################################
#####                                           Clean the data                                            #####
###############################################################################################################
#----

### Record missing values to median or mode of some features 
# These features include: Est_income, Est_Net_worth, Home_value, Pct_above_poverty_line
# , Pct_below_poverty_line, Education_level, College, Dwelling_Type, Num_person_household
# , Length_residence, Population_density_centile_ST, Population_density_centile_US, Online_User
# , Online_purchaser, Decile_struggle_Med_lang, Est_BMI_decile, Index_Health_ins_engage
# , Index_Health_ins_influence
dataset$Est_income[is.na(dataset$Est_income)] <- getmode(dataset$Est_income)
dataset$Est_Net_worth[is.na(dataset$Est_Net_worth)] <- getmode(dataset$Est_Net_worth)
dataset$Home_value[is.na(dataset$Home_value)] <- getmode(dataset$Home_value)
dataset$Pct_above_poverty_line[is.na(dataset$Pct_above_poverty_line)] <- median(dataset$Pct_above_poverty_line, na.rm = TRUE)
dataset$Pct_below_poverty_line[is.na(dataset$Pct_below_poverty_line)] <- median(dataset$Pct_below_poverty_line, na.rm = TRUE)
dataset$Education_level[is.na(dataset$Education_level)] <- getmode(dataset$Education_level)
dataset$College[is.na(dataset$College)] <- getmode(dataset$College)
dataset$Dwelling_Type[is.na(dataset$Dwelling_Type)] <- getmode(dataset$Dwelling_Type)
dataset$Num_person_household[is.na(dataset$Num_person_household)] <- getmode(dataset$Num_person_household)
dataset$Length_residence[is.na(dataset$Length_residence)] <- median(dataset$Length_residence, na.rm = TRUE)
dataset$Population_density_centile_ST[is.na(dataset$Population_density_centile_ST)] <- median(dataset$Population_density_centile_ST, na.rm = TRUE)
dataset$Population_density_centile_US[is.na(dataset$Population_density_centile_US)] <- median(dataset$Population_density_centile_US, na.rm = TRUE)
dataset$Online_User[is.na(dataset$Online_User)] <- getmode(dataset$Online_User)
dataset$Online_purchaser[is.na(dataset$Online_purchaser)] <- getmode(dataset$Online_purchaser)
dataset$Decile_struggle_Med_lang[is.na(dataset$Decile_struggle_Med_lang)] <- median(dataset$Decile_struggle_Med_lang, na.rm = TRUE)
dataset$Est_BMI_decile[is.na(dataset$Est_BMI_decile)] <- median(dataset$Est_BMI_decile, na.rm = TRUE)
dataset$Index_Health_ins_engage[is.na(dataset$Index_Health_ins_engage)] <- median(dataset$Index_Health_ins_engage, na.rm = TRUE)
dataset$Index_Health_ins_influence[is.na(dataset$Index_Health_ins_influence)] <- getmode(dataset$Index_Health_ins_influence)


### Convert categorical variables into factor variables
# SEX_CD
dataset$sex_CD[dataset$SEX_CD=='M'] <- 1
dataset$sex_CD[dataset$SEX_CD=='F'] <- 2
dataset$SEX_CD <- dataset$sex_CD
dataset$sex_CD <- NULL
# ESRD_IND (still have one missing value)
dataset$esrd_IND[dataset$ESRD_IND=='Y'] <- 1
dataset$esrd_IND[dataset$ESRD_IND=='N'] <- 2
dataset <- na.omit(dataset)
dataset$ESRD_IND <- dataset$esrd_IND
dataset$esrd_IND <- NULL
# HOSPICE_IND
dataset$hospice_IND[dataset$HOSPICE_IND=='Y'] <- 1
dataset$hospice_IND[dataset$HOSPICE_IND=='N'] <- 2
dataset$HOSPICE_IND <- dataset$hospice_IND
dataset$hospice_IND <- NULL
# PCP_ASSIGNMENT (still have 48 missing values)
dataset$pcp_ASSIGNMENT[dataset$PCP_ASSIGNMENT=='ATTRIBUTED'] <- 1
dataset$pcp_ASSIGNMENT[dataset$PCP_ASSIGNMENT=='MEMBER SELECTED'] <- 2
dataset$pcp_ASSIGNMENT[dataset$PCP_ASSIGNMENT=='UNATTRIBUTED'] <- 3
dataset <- na.omit(dataset)
dataset$PCP_ASSIGNMENT <- dataset$pcp_ASSIGNMENT
dataset$pcp_ASSIGNMENT <- NULL
# DUAL
dataset$dual[dataset$DUAL=='Y'] <- 1
dataset$dual[dataset$DUAL=='N'] <- 2
dataset$DUAL <- dataset$dual
dataset$dual <- NULL
# INSTITUTIONAL
dataset$institutional[dataset$INSTITUTIONAL=='Y'] <- 1
dataset$institutional[dataset$INSTITUTIONAL=='N'] <- 2
dataset$INSTITUTIONAL <- dataset$institutional
dataset$institutional <- NULL
# LIS
dataset$lis[dataset$LIS=='Y'] <- 1
dataset$lis[dataset$LIS=='N'] <- 2
dataset$LIS <- dataset$lis
dataset$lis <- NULL
# MAJOR_GEOGRAPHY
dataset$major_GEOGRAPHY[dataset$MAJOR_GEOGRAPHY=='Central'] <- 1
dataset$major_GEOGRAPHY[dataset$MAJOR_GEOGRAPHY=='Florida'] <- 2
dataset$major_GEOGRAPHY[dataset$MAJOR_GEOGRAPHY=='Northern'] <- 3
dataset$major_GEOGRAPHY[dataset$MAJOR_GEOGRAPHY=='Southeastern'] <- 4
dataset$major_GEOGRAPHY[dataset$MAJOR_GEOGRAPHY=='Western'] <- 5
dataset$MAJOR_GEOGRAPHY <- dataset$major_GEOGRAPHY
dataset$major_GEOGRAPHY <- NULL
# MINOR_GEOGRAPHY
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='California'] <- 1
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Central'] <- 2
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Central West'] <- 3
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='East'] <- 4
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='East Central'] <- 5
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='FL Non HMO'] <- 6
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Gulf States'] <- 7
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Intermountain'] <- 8
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Mid-Atlantic'] <- 9
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Mid-South'] <- 10
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='North Central'] <- 11
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='North Florida'] <- 12
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Northeast'] <- 13
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Pacific'] <- 14
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='South Florida'] <- 15
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Southeast: GA-SC'] <- 16
dataset$minor_GEOGRAPHY[dataset$MINOR_GEOGRAPHY=='Texas'] <- 17
dataset$MINOR_GEOGRAPHY <- dataset$minor_GEOGRAPHY
dataset$minor_GEOGRAPHY <- NULL
# MCO_HLVL_PLAN_CD
dataset$mco_HLVL_PLAN_CD[dataset$MCO_HLVL_PLAN_CD=='MA'] <- 1
dataset$mco_HLVL_PLAN_CD[dataset$MCO_HLVL_PLAN_CD=='MAPD'] <- 2
dataset$MCO_HLVL_PLAN_CD <- dataset$mco_HLVL_PLAN_CD
dataset$mco_HLVL_PLAN_CD <- NULL
# MCO_PROD_TYPE_CD
dataset$mco_PROD_TYPE_CD[dataset$MCO_PROD_TYPE_CD=='HMO'] <- 1
dataset$mco_PROD_TYPE_CD[dataset$MCO_PROD_TYPE_CD=='LPPO'] <- 2
dataset$mco_PROD_TYPE_CD[dataset$MCO_PROD_TYPE_CD=='PFFS'] <- 3
dataset$mco_PROD_TYPE_CD[dataset$MCO_PROD_TYPE_CD=='RPPO'] <- 4
dataset$MCO_PROD_TYPE_CD <- dataset$mco_PROD_TYPE_CD
dataset$mco_PROD_TYPE_CD <- NULL
# Dwelling_Type
dataset$dwelling_Type[dataset$Dwelling_Type=='A'] <- 1
dataset$dwelling_Type[dataset$Dwelling_Type=='B'] <- 2
dataset$dwelling_Type[dataset$Dwelling_Type=='C'] <- 3
dataset$dwelling_Type[dataset$Dwelling_Type=='N'] <- 4
dataset$dwelling_Type[dataset$Dwelling_Type=='P'] <- 5
dataset$dwelling_Type[dataset$Dwelling_Type=='S'] <- 6
dataset$dwelling_Type[dataset$Dwelling_Type=='T'] <- 7
dataset$Dwelling_Type <- dataset$dwelling_Type
dataset$dwelling_Type <- NULL
# After recording missing values and removing the remianing missing values
# , the dataset is 44942 * 939.

#----
###############################################################################################################
#####                                Explore the data & Variables selection                              #####
###############################################################################################################
#----

### Get to know percentages of Admissions and Readmissions
table(dataset$ADMISSIONS)
table(dataset$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# Admissions:
#     0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    16    19 
# 36259  4711  2147   798   462   235   147    74    47    27    14     8     7     1     3     1     1    
#
# Readmissions:
#     0     1     2     3     4     5     6     7     8     9    10    11    12 
# 42993  1304   378   151    51    30    16    10     3     3     1     1     1 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# We can see that the classes of ADMISSIONS and READMISSIONS are unbalanced.
# So we need to pay attention to the effect affected by unbalance. 


### Add potential useful data: sum of all the flags
# Add sum of all the flags 
# CON_VISIT_ starts from CON_VISIT_04_Q04 and ends to CON_VISIT_13_Q08
# POT_VISIT_ starts from POT_VISIT_11_Q03 and ends to POT_VISIT_55_Q01
# RX_THER_ starts from RX_THER_27_YR2015 and ends to RX_THER_20_YR2015
dataset$sum_con <- rowSums (dataset[which(colnames(dataset)=="CON_VISIT_04_Q04"):which(colnames(dataset)=="CON_VISIT_13_Q08")], 
                            na.rm = FALSE, dims = 1)
dataset$sum_pot <- rowSums (dataset[which(colnames(dataset)=="POT_VISIT_11_Q03"):which(colnames(dataset)=="POT_VISIT_55_Q01")],
                            na.rm = FALSE, dims = 1)
dataset$sum_rx <- rowSums (dataset[which(colnames(dataset)=="RX_THER_27_YR2015"):which(colnames(dataset)=="RX_THER_20_YR2015")],
                           na.rm = FALSE, dims = 1)
dataset$rxPerVist <- dataset$sum_rx / dataset$sum_con


### Add selected sums of POT/CON/RX
# Add selected sum of CON
selectConImp <- cbind(dataset[207],dataset[208],dataset[254],dataset[259],dataset[294],dataset[298],dataset[302],
                      dataset[310],dataset[322:326],dataset[342:344],dataset[331:334],dataset[351:362])
dataset$select_conImp <- rowSums (selectConImp,na.rm = FALSE, dims = 1)
# Add selected sum of POT
selectPotImp <- cbind(dataset[558],dataset[579],dataset[601:604],dataset[610],dataset[616],dataset[617],dataset[621],dataset[637],dataset[597:599],
                      dataset[606],dataset[622],dataset[642],dataset[643],dataset[535],dataset[536],dataset[566],dataset[567],dataset[583],dataset[584],
                      dataset[593],dataset[594],dataset[532],dataset[550],dataset[551],dataset[570],dataset[581],dataset[633],dataset[484],dataset[503],
                      dataset[545],dataset[565],dataset[471],dataset[541],dataset[552],dataset[553],dataset[577],dataset[613],dataset[614],dataset[556],
                      dataset[561],dataset[562],dataset[571:575])
dataset$select_potImp <- rowSums (selectPotImp,na.rm = FALSE, dims = 1)
# Add selected sum of RX
selectRxImp <- cbind(dataset[887],dataset[896],dataset[858],dataset[897],dataset[899],dataset[908],dataset[868:870],
                     dataset[909],dataset[917],dataset[919])
dataset$select_rxImp <- rowSums (selectRxImp,na.rm = FALSE, dims = 1)

# add sum of flags
dataset$flags2015 <- rowSums (dataset[31:57], na.rm = FALSE, dims = 1)
dataset$flags2014 <- rowSums (dataset[58:86], na.rm = FALSE, dims = 1)
dataset$flag1514 <- dataset$flags2015 - dataset$flags2014
dataset$flags <- dataset$flags2015 + dataset$flags2014
#dataset$flagsIncre <- dataset$flags2015 - dataset$flags2014

#select important
flags_all <- colSums (cbind(dataset[31:57],dataset[58:86]), na.rm = FALSE, dims = 1)
adm <- dataset[dataset[1]!=0, ]
flags_adm <- colSums (cbind(adm[31:57],adm[58:86]), na.rm = FALSE, dims = 1)
compare <- flags_adm / flags_all
result <- cbind(flags_all,flags_adm,compare)

dataset$flags2015Imp <- rowSums (dataset[31:57], na.rm = FALSE, dims = 1)
dataset$flags2014Imp <- rowSums (dataset[58:86], na.rm = FALSE, dims = 1)

##hist
hist(dataset$flags[dataset[1]!=0], col=rgb(1,0,0,0.5), ylim=c(0,10000),main='Overlapping Histogram')

hist(dataset$flags[dataset[1]==0], col=rgb(0,0,1,0.5), add=T)

summary(dataset$flags[dataset[1]!=0])
summary(dataset$flags[dataset[1]==0])

hist(dataset$flag1514[dataset[1]!=0], col=rgb(1,0,0,0.5), ylim=c(0,10000),main='Overlapping Histogram')
hist(dataset$flag1514[dataset[1]==0],col=rgb(0,0,1,0.5), add=T)

bbb<-dataset$flag1514[dataset[1]!=0]
aaa<-dataset$flag1514[dataset[1]==0]

bbb<-dataset$Est_BMI_decile[dataset[1]!=0]
aaa<-dataset$Est_BMI_decile[dataset[1]==0]

boxplot(aaa, bbb, horizontal = TRUE, main = "Ages of Oscar Winning Actors", xlab = "Age")

hist(dataset$Est_BMI_decile[dataset[1]!=0])
(dataset$Est_BMI_decile[dataset[1]==0])

hist(aaa, col=rgb(1,0,0,0.5), ylim=c(0,8000),main='Overlapping Histogram')
hist(bbb,col=rgb(0,0,1,0.5), add=T)


sum(dataset$flags2015)
sum(dataset$flags2014)
summary(dataset$flags2014[dataset[1]!=0])
summary(dataset$flags2014[dataset[1]==0])
#gender
summary(dataset$SEX_CD[dataset[1]!=0])
summary(dataset$SEX_CD[dataset[1]==0])

#MONTHS_2016
summary(dataset$MONTHS_2016[dataset[1]!=0])
summary(dataset$MONTHS_2016[dataset[1]==0])
table(dataset$MONTHS_2016)

##ESRD
summary(dataset$ESRD_IND[dataset[1]!=0])
summary(dataset$ESRD_IND[dataset[1]==0])


##HOSPICE_IND
summary(dataset$HOSPICE_IND[dataset[1]!=0])
summary(dataset$HOSPICE_IND[dataset[1]==0])


#ORIG_REAS_ENTITLE_CD
table(dataset$ORIG_REAS_ENTITLE_CD[dataset[1]!=0])
table(dataset$ORIG_REAS_ENTITLE_CD[dataset[1]==0])

##PCP_ASSIGNMENT
table(dataset$PCP_ASSIGNMENT[dataset[1]!=0])
table(dataset$PCP_ASSIGNMENT[dataset[1]==0])

##DUAL
table(dataset$DUAL[dataset[1]!=0])
table(dataset$DUAL[dataset[1]==0])

#INSTITUTIONAL
table(dataset$INSTITUTIONAL[dataset[1]!=0])
table(dataset$INSTITUTIONAL[dataset[1]==0])

#LIS
table(dataset$LIS[dataset[1]!=0])
table(dataset$LIS[dataset[1]==0])

#MAJOR_GEOGRAPHY
summary(dataset$MAJOR_GEOGRAPHY[dataset[1]!=0])
summary(dataset$MAJOR_GEOGRAPHY[dataset[1]==0])

summary(dataset$MINOR_GEOGRAPHY)

# MCO_HLVL_PLAN_CD

summary(dataset$MCO_HLVL_PLAN_CD)
summary(dataset$MCO_HLVL_PLAN_CD[dataset[1]!=0])
summary(dataset$MCO_HLVL_PLAN_CD[dataset[1]==0])

#MCO_PROD_TYPE_CD
summary(dataset$MCO_PROD_TYPE_CD)
summary(dataset$MCO_PROD_TYPE_CD[dataset[1]!=0])
summary(dataset$MCO_PROD_TYPE_CD[dataset[1]==0])

#ACE_ELIG_2014/15
table(dataset$ACE_ELIG_2014)
table(dataset$ACE_ELIG_2014[dataset[1]!=0])
table(dataset$ACE_ELIG_2014[dataset[1]==0])

table(dataset$ACE_ELIG_2015[dataset[1]!=0])
table(dataset$ACE_ELIG_2015[dataset[1]==0])

table(dataset$ACE_PASS_2014[dataset[1]!=0])
table(dataset$ACE_PASS_2014[dataset[1]==0])

table(dataset$ACE_PASS_2015[dataset[1]!=0])
table(dataset$ACE_PASS_2015[dataset[1]==0])

table(dataset$DIAB_ELIG_2014[dataset[1]!=0])
table(dataset$DIAB_ELIG_2014[dataset[1]==0])

table(dataset$DIAB_PASS_2014[dataset[1]!=0])
table(dataset$DIAB_PASS_2014[dataset[1]==0])

# Find who dont adhere
dataset$adhHT2014 <- dataset$ACE_ELIG_2014 - dataset$ACE_PASS_2014
dataset$adhHT2015 <- dataset$ACE_ELIG_2015 - dataset$ACE_PASS_2015

dataset$adhDI2014 <- dataset$DIAB_ELIG_2014 - dataset$DIAB_PASS_2014
dataset$adhDI2015 <- dataset$DIAB_ELIG_2015 - dataset$DIAB_PASS_2015

dataset$adhHL2014 <- dataset$STATIN_ELIG_2014 - dataset$STATIN_PASS_2014
dataset$adhHL2015 <- dataset$STATIN_ELIG_2015 - dataset$STATIN_PASS_2015

dataset$totalNotAdh <- dataset$adhHT2014 + dataset$adhHT2015 + dataset$adhDI2014 + dataset$adhDI2015 + dataset$adhHL2014 +dataset$adhHL2015



table(dataset$totalNotAdh[dataset[1]!=0])
table(dataset$totalNotAdh[dataset[1]==0])

summary(dataset$totalNotAdh[dataset[1]!=0])
summary(dataset$totalNotAdh[dataset[1]==0])

#income
table(dataset$Est_income[dataset[1]!=0])
table(dataset$Est_income[dataset[1]==0])

hist(dataset$Est_income[dataset[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,20000))
hist(dataset$Est_income[dataset[1]==0],col=rgb(0,0,1,0.5),add=T)

summary(dataset$Est_Net_worth[dataset[1]!=0])
summary(dataset$Est_Net_worth[dataset[1]==0])

hist(dataset$Est_Net_worth[dataset[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,20000))
hist(dataset$Est_Net_worth[dataset[1]==0],col=rgb(0,0,1,0.5),add=T)


hist(dataset$Pct_above_poverty_line[dataset[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,8000))
hist(dataset$Pct_above_poverty_line[dataset[1]==0],col=rgb(0,0,1,0.5),add=T)

summary(dataset$Home_value[dataset[1]!=0])
summary(dataset$Home_value[dataset[1]==0])

#education

table(dataset$Education_level[dataset[1]!=0])
table(dataset$Education_level[dataset[1]==0])

hist(dataset$Education_level[dataset[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,30000))
hist(dataset$Education_level[dataset[1]==0],col=rgb(0,0,1,0.5),add=T)

count(dataset$College[dataset[1]!=0])
count(dataset$College[dataset[1]==0])

#Decile_struggle_Med_lang
summary(dataset$Decile_struggle_Med_lang[dataset[1]!=0])
summary(dataset$Decile_struggle_Med_lang[dataset[1]==0])

#Index_Health_ins_engage

summary(dataset$Index_Health_ins_engage[dataset[1]!=0])
summary(dataset$Index_Health_ins_engage[dataset[1]==0])

#Index_Health_ins_influence
summary(dataset$Index_Health_ins_influence[dataset[1]!=0])
summary(dataset$Index_Health_ins_influence[dataset[1]==0])

#sum_con
summary(dataset$select_con[dataset[1]!=0])
summary(dataset$select_con[dataset[1]==0])

summary(dataset$select_potImp[dataset[1]!=0])
summary(dataset$select_potImp[dataset[1]==0])

summary(dataset$select_rxImp[dataset[1]!=0])
summary(dataset$select_rxImp[dataset[1]==0])

summary(dataset$select_conImp[dataset[1]!=0])
summary(dataset$select_conImp[dataset[1]==0])

summary(dataset$rxPerVist[dataset[1]!=0])
summary(dataset$rxPerVist[dataset[1]==0])

hist(dataset$rxPerVist[dataset[1]!=0],col=rgb(1,0,0,0.5), ylim=c(0,30000))
hist(dataset$rxPerVist[dataset[1]==0],col=rgb(0,0,1,0.5),add=T)

dataset$rxPerVist[dataset$rxPerVist==Inf] <- 0
summary(dataset$rxPerVist)

#CDC_2014
count(dataset$sum_cdc[dataset[1]!=0])
summary(dataset$sum_cdc[dataset[1]==0])
dataset$sum_cdc <- rowSums (dataset[85:106],na.rm = FALSE, dims = 1)

cdc_Im <- cbind(dataset[87:89],dataset[98],dataset[99])
dataset$sum_cdcIm <- rowSums (cdc_Im,na.rm = FALSE, dims = 1)



cdc2014 <- rowSums (dataset[85:93],na.rm = FALSE, dims = 1)
cdc2015 <- rowSums (dataset[94:106],na.rm = FALSE, dims = 1)
sum(cdc2014)
sum(cdc2015)

summary(dataset$sum_cdcIm[dataset[1]!=0])
summary(dataset$sum_cdcIm[dataset[1]==0])

new <- dataset[940:946]

### delete columns
newdataset <- dataset

newdataset[960] <- NULL
newdataset[951:958] <- NULL
newdataset[947:949] <- NULL
newdataset[943] <-NULL
newdataset[938] <- NULL
newdataset[939] <- NULL
newdataset[924:936] <- NULL
newdataset[15:922] <- NULL
newdataset[11] <- NULL
newdataset[12] <- NULL
newdataset[7] <- NULL
newdataset[3:5] <- NULL


### remove rows with missing value
newdataset <- na.omit(newdataset)
# The new dataset is 44942*19

#----
###############################################################################################################
#####                                Preprocess and Split the data                                        #####
###############################################################################################################
#----

### Create the dataset for Admission Prediction Model
# drop the column Readmission
datasetA <- within(newdataset,rm("READMISSIONS"))


### Create the dataset for Readmission Prediction Model
# only use the data 
datasetR <- newdataset[which(newdataset$ADMISSIONS != 0),]
# drop the column Admission
datasetR <- within(datasetR,rm("ADMISSIONS"))


### split the data into training and testing dataset
# 80% of the sample size
sample_sizeA <- floor(0.8 * nrow(datasetA))
sample_sizeR <- floor(0.8 * nrow(datasetR))
# set the seed to make the partition reproductible
set.seed(2017)
# Split the datasetA
sampleA <- sample(seq_len(nrow(datasetA)), size=sample_sizeA)
trainingA <- datasetA[sampleA,]
testingA <- datasetA[-sampleA,]
# Split the datasetR
sampleR <- sample(seq_len(nrow(datasetR)), size=sample_sizeR)
trainingR <- datasetR[sampleR,]
testingR <- datasetR[-sampleR,]
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# traingA$ADMISSIONS:
#     0     1     2     3     4     5     6     7     8     9    10    11    12    13    14    19 
# 29021  3768  1693   636   387   185   116    60    35    22    10     8     7     1     3     1 
#
# testingA$ADMISSIONS:
#    0    1    2    3    4    5    6    7    8    9   10   16 
# 7238  943  454  162   75   50   31   14   12    5    4    1 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# traingR$READMISSIONS:
#    0    1    2    3    4    5    6    7    8    9   10   11   12 
# 5396 1028  301  127   43   24   12    7    2    3    1    1    1 
#
# testingR$READMISSIONS:
#    0    1    2    3    4    5    6    7    8 
# 1338  276   77   24    8    6    4    3    1 
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---
# We can see that the classes of ADMISSIONS and READMISSIONS are unbalanced.
# So we decide to over-sampling the small class. 

### Deal with imbalance issue of traingA
# Replicate the records based on the number of Admissions (!=0,1)
replicatePartA <- trainingA[which(trainingA$ADMISSIONS!=0 & trainingA$ADMISSIONS!=1),]
replicatePartA <- replicatePartA[rep(row.names(replicatePartA),replicatePartA$ADMISSIONS),]
# Combine replicate part (!=0,1) with other (=0,1)
trainingA <- rbind(replicatePartA, trainingA[which(trainingA$ADMISSIONS == 0 | trainingA$ADMISSIONS == 1),])
# Convert the values of Admissions (!=0,1) into 1
trainingA$ADMISSIONS[trainingA$ADMISSIONS!=0] <- 1
# After replicate the admission class (!=0,1), we get the trainingA .


### Deal with imbalance issue of trainingR
# Replicate the records based on the number of Readmission (!=0,1)
replicatePartR <- trainingR[which(trainingR$READMISSIONS!=0 & trainingR$READMISSIONS!=1),]
replicatePartR <- replicatePartR[rep(row.names(replicatePartR),replicatePartR$READMISSIONS),]
# Combine replicate part (!=0,1) with other (=0,1)
trainingR <- rbind(replicatePartR, trainingR[which(trainingR$READMISSIONS == 0 | trainingR$READMISSIONS == 1),])
# Convert the values of Admissions (!=0,1) into 1
trainingR$READMISSIONS[trainingR$READMISSIONS!=0] <- 1
# After replicate the admission class (!=0,1), we get the trainingR .


### balancedTestingA
balancedTestingA <- testingA
# Replicate the records based on the number of Admissions (!=0,1)
replicatePartA <- balancedTestingA[which(balancedTestingA$ADMISSIONS!=0 & balancedTestingA$ADMISSIONS!=1),]
replicatePartA <- replicatePartA[rep(row.names(replicatePartA),replicatePartA$ADMISSIONS),]
# Combine replicate part (!=0,1) with other (=0,1)
balancedTestingA <- rbind(replicatePartA, balancedTestingA[which(balancedTestingA$ADMISSIONS == 0 | balancedTestingA$ADMISSIONS == 1),])
# Convert the values of Admissions (!=0,1) into 1
balancedTestingA$ADMISSIONS[balancedTestingA$ADMISSIONS!=0] <- 1
# After replicate the admission class (!=0,1), we get the balancedTestingA

### UnbalancedTestingA (Actual dataset)
UnbalancedTestingA <- testingA
# Convert UnblancedTestingA$ADMISSIONS 0 & 1. 
UnbalancedTestingA$ADMISSIONS[UnbalancedTestingA$ADMISSIONS!=0] <- 1


### balancedTestingR
balancedTestingR <- testingR
# Replicate the records based on the number of Admissions (!=0,1)
replicatePartA <- balancedTestingR[which(balancedTestingR$READMISSIONS!=0 & balancedTestingR$READMISSIONS!=1),]
replicatePartA <- replicatePartA[rep(row.names(replicatePartA),replicatePartA$READMISSIONS),]
# Combine replicate part (!=0,1) with other (=0,1)
balancedTestingR <- rbind(replicatePartA, balancedTestingR[which(balancedTestingR$READMISSIONS == 0 | balancedTestingR$READMISSIONS == 1),])
# Convert the values of Admissions (!=0,1) into 1
balancedTestingR$READMISSIONS[balancedTestingR$READMISSIONS!=0] <- 1
# After replicate the admission class (!=0,1), we get the balancedTestingR

### UnbalancedTestingR (Actual dataset)
UnbalancedTestingR <- testingR
# Convert UnbalancedTestingR$READMISSIONS 0 & 1. 
UnbalancedTestingR$READMISSIONS[UnbalancedTestingR$READMISSIONS!=0] <- 1

### Set Admission and Readmission as ordered factor
# trainingA$ADMISSIONS
trainingA$ADMISSIONS <- ifelse(trainingA$ADMISSIONS == 1, "Y", "N")
trainingA$ADMISSIONS <- as.factor(trainingA$ADMISSIONS)
trainingA$ADMISSIONS <- relevel(trainingA$ADMISSIONS,"Y")
# balancedTestingA$ADMISSIONS
balancedTestingA$ADMISSIONS <- ifelse(balancedTestingA$ADMISSIONS == 1, "Y", "N")
balancedTestingA$ADMISSIONS <- as.factor(balancedTestingA$ADMISSIONS)
balancedTestingA$ADMISSIONS <- relevel(balancedTestingA$ADMISSIONS,"Y")
# UnbalancedTestingA$ADMISSIONS
UnbalancedTestingA$ADMISSIONS <- ifelse(UnbalancedTestingA$ADMISSIONS == 1, "Y", "N")
UnbalancedTestingA$ADMISSIONS <- as.factor(UnbalancedTestingA$ADMISSIONS)
UnbalancedTestingA$ADMISSIONS <- relevel(UnbalancedTestingA$ADMISSIONS,"Y")
# trainingR$READMISSIONS
trainingR$READMISSIONS <- ifelse(trainingR$READMISSIONS == 1, "Y", "N")
trainingR$READMISSIONS <- as.factor(trainingR$READMISSIONS)
trainingR$READMISSIONS <- relevel(trainingR$READMISSIONS,"Y")
# balancedTestingR$READMISSIONS
balancedTestingR$READMISSIONS <- ifelse(balancedTestingR$READMISSIONS == 1, "Y", "N")
balancedTestingR$READMISSIONS <- as.factor(balancedTestingR$READMISSIONS)
balancedTestingR$READMISSIONS <- relevel(balancedTestingR$READMISSIONS,"Y")
# UnbalancedTestingR$READMISSIONS
UnbalancedTestingR$READMISSIONS <- ifelse(UnbalancedTestingR$READMISSIONS == 1, "Y", "N")
UnbalancedTestingR$READMISSIONS <- as.factor(UnbalancedTestingR$READMISSIONS)
UnbalancedTestingR$READMISSIONS <- relevel(UnbalancedTestingR$READMISSIONS,"Y")

#----
###############################################################################################################
#####                         Build the model for Admission - Logistic                                    #####
###############################################################################################################
#----

### Build model
ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary,verboseIter = TRUE)
logit <- train(ADMISSIONS~.,
               data = trainingA,
               method = "glm",
               family = "binomial",
               metric = "ROC", 
               trControl = ctrl,
               na.action=na.exclude)

### Find Confusion Matrix for training data
logitProbsTrain <- predict(logit, newdata=trainingA, type="prob")[,1]
logitClassesTrain <- predict(logit, newdata=trainingA)
confusionMatrix(data=logitClassesTrain, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  4688  2097
#          N  8787 26924
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7439
# Sensitivity : 0.3479          
# Specificity : 0.9277
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitTrain <- roc (response = trainingA$ADMISSIONS,
                     predictor = logitProbsTrain,
                     levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveLogitTrain)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7516
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitProbsTest1 <- predict(logit, newdata=balancedTestingA, type="prob")[,1]
logitClassesTest1 <- predict(logit, newdata=balancedTestingA)
confusionMatrix(data=logitClassesTest1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y 1085  507
#          N 2283 6731
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7369
# Sensitivity : 0.3221          
# Specificity : 0.9300
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitTest1 <- roc (response = balancedTestingA$ADMISSIONS,
                           predictor = logitProbsTest1,
                           levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(rocCurveLogitTest1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7419
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
logitProbsTest2 <- predict(logit, newdata=UnbalancedTestingA, type="prob")[,1]
logitClassesTest2 <- predict(logit, newdata=UnbalancedTestingA)
confusionMatrix(data=logitClassesTest2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  462  507
#          N 1289 6731
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.8002
# Sensitivity : 0.2638          
# Specificity : 0.9300
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitTest2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                          predictor = logitProbsTest2,
                          levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(rocCurveLogitTest2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.709
# --- --- --- --- --- --- --- --- --- ---




#----
###############################################################################################################
#####                    Build the model for Admission - Logistic Boosted                                 #####
###############################################################################################################
#----

### Build model
logitBoosted <- train(ADMISSIONS~.,
               data = trainingA,
               method = "gbm",
               verbose=F,
               trControl=trainControl(method="cv",number=20))

### Find Confusion Matrix for training data
logitBoostedProbsTrain <- predict(logitBoosted, newdata=trainingA, type="prob")[,1]
logitBoostedClassesTrain <- predict(logitBoosted, newdata=trainingA)
confusionMatrix(data=logitBoostedClassesTrain, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  5304  2334    
#          N  8171 26687  
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7528
# Sensitivity :  0.3936         
# Specificity :  0.9196
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitBoostedTrain <- roc (response = trainingA$ADMISSIONS,
                           predictor = logitBoostedProbsTrain,
                           levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveLogitBoostedTrain)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7711
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitBoostedProbsTest1 <- predict(logitBoosted, newdata=balancedTestingA, type="prob")[,1]
logitBoostedClassesTest1 <- predict(logitBoosted, newdata=balancedTestingA)
confusionMatrix(data=logitBoostedClassesTest1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y 1255  590
#          N 2113 6648
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7451
# Sensitivity : 0.3726          
# Specificity : 0.9185
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitBoostedTest1 <- roc (response = balancedTestingA$ADMISSIONS,
                           predictor = logitBoostedProbsTest1,
                           levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(rocCurveLogitBoostedTest1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7506
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
logitBoostedProbsTest2 <- predict(logitBoosted, newdata=UnbalancedTestingA, type="prob")[,1]
logitBoostedClassesTest2 <- predict(logitBoosted, newdata=UnbalancedTestingA)
confusionMatrix(data=logitBoostedClassesTest2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  543  590
#          N 1208 6648
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.8
# Sensitivity : 0.31011          
# Specificity : 0.91849
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitBoostedTest2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                           predictor = logitBoostedProbsTest2,
                           levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(rocCurveLogitBoostedTest2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7156
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                    Build the model for Admission - Logistic Bagged                                  #####
###############################################################################################################
#----

### Build model
logitBagged <- train(ADMISSIONS~.,
                      data = trainingA,
                      method = "treebag",
                      trControl=trainControl(method="cv",number=20))

### Find Confusion Matrix for training data
logitBaggedProbsTrain <- predict(logitBagged, newdata=trainingA, type="prob")[,1]
logitBaggedClassesTrain <- predict(logitBagged, newdata=trainingA)
confusionMatrix(data=logitBaggedClassesTrain, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y 13410     5    
#          N    65 29016  
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.9984
# Sensitivity :  0.9952         
# Specificity :  0.9998
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitBaggedTrain<- roc (response = trainingA$ADMISSIONS,
                                  predictor = logitBaggedProbsTrain,
                                  levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveLogitBaggedTrain)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 1
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitBaggedProbsTest1 <- predict(logitBagged, newdata=balancedTestingA, type="prob")[,1]
logitBaggedClassesTest1 <- predict(logitBagged, newdata=balancedTestingA)
confusionMatrix(data=logitBaggedClassesTest1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  830  420
#          N 2538 6818
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7211
# Sensitivity : 0.24644          
# Specificity : 0.94197
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitBaggedTest1 <- roc (response = balancedTestingA$ADMISSIONS,
                                  predictor = logitBaggedProbsTest1,
                                  levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(rocCurveLogitBaggedTest1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7048
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
logitBaggedProbsTest2 <- predict(logitBagged, newdata=UnbalancedTestingA, type="prob")[,1]
logitBaggedClassesTest2 <- predict(logitBagged, newdata=UnbalancedTestingA)
confusionMatrix(data=logitBaggedClassesTest2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  354  420
#          N 1397 6818
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7979
# Sensitivity : 0.20217          
# Specificity : 0.94197
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveLogitBaggedTest2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                                  predictor = logitBaggedProbsTest2,
                                  levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(rocCurveLogitBaggedTest2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6791
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                             Build the model for Admission - CART                                    #####
###############################################################################################################
#----

### Build model
cartmod1 <- train(ADMISSIONS~.,
                  data = trainingA,
                  method = "rpart",
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

### Find Confusion Matrix for training data
cartProbsTrain <- predict(cartmod1, newdata=trainingA, type="prob")[,1]
cartClassesTrain <- predict(cartmod1, newdata=trainingA)
confusionMatrix(cartClassesTrain, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  4345  1865
#          N  9130 27156  
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7413
# Sensitivity : 0.3224          
# Specificity : 0.9357
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveCartTrain <- roc (response = trainingA$ADMISSIONS,
                          predictor = cartProbsTrain,
                          levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveCartTrain)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6574
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
CartProbsTest1 <- predict(cartmod1, newdata=balancedTestingA, type="prob")[,1]
CartClassesTest1 <- predict(cartmod1, newdata=balancedTestingA)
confusionMatrix(data=CartClassesTest1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y 1029  476
#          N 2339 6762
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7346
# Sensitivity : 0.30552         
# Specificity : 0.93424
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveCartTest1 <- roc (response = balancedTestingA$ADMISSIONS,
                         predictor = CartProbsTest1,
                         levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(rocCurveCartTest1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6524
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
CartProbsTest2 <- predict(cartmod1, newdata=UnbalancedTestingA, type="prob")[,1]
CartClassesTest2 <- predict(cartmod1, newdata=UnbalancedTestingA)
confusionMatrix(data=CartClassesTest2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  433  476
#          N 1318 6762
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.8004
# Sensitivity : 0.24729         
# Specificity : 0.93424
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveCartTest2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                         predictor = CartProbsTest2,
                         levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(rocCurveCartTest2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6201
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                       Build the model for Admission - Random Forest                                 #####
###############################################################################################################
#----

### Build model
RFmod=train(ADMISSIONS~.,
            data = trainingA,
            method="rf",
            importance=T,
            trControl=trainControl(method="cv",number=20,verboseIter = TRUE))


### Find Confusion Matrix for training data
rfProbsTrain <- predict(RFmod, newdata=trainingA, type="prob")[,1]
rfClassesTrain <- predict(RFmod, newdata=trainingA)
confusionMatrix(data=rfClassesTrain, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y 13475     0
#          N     0 29021  
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 1
# Sensitivity : 1.000         
# Specificity : 1.000
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveRfTrain <- roc (response = trainingA$ADMISSIONS,
                           predictor = rfProbsTrain,
                           levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveRfTrain)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 1
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
rfProbsTest1 <- predict(RFmod, newdata=balancedTestingA, type="prob")[,1]
rfClassesTest1 <- predict(RFmod, newdata=balancedTestingA)
confusionMatrix(data=rfClassesTest1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  733  277
#          N 2635 6961
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7254
# Sensitivity :  0.21764         
# Specificity :  0.96173
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveRfTest1 <- roc (response = balancedTestingA$ADMISSIONS,
                          predictor = rfProbsTest1,
                          levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(rocCurveRfTest1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7287
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
rfProbsTest2 <- predict(RFmod, newdata=UnbalancedTestingA, type="prob")[,1]
rfClassesTest2 <- predict(RFmod, newdata=UnbalancedTestingA)
confusionMatrix(data=rfClassesTest2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  293  273
#          N 1458 6965
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.8074
# Sensitivity :  0.16733         
# Specificity :  0.96228
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveRfTest2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                       predictor = rfProbsTest2,
                       levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(rocCurveRfTest2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.698
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                              Build the model for Admission - LDA                                    #####
###############################################################################################################
#----

### Build model
ldamodA <- train(ADMISSIONS~.,
                 data = trainingA,
                 method = "lda",
                 family = "binomial",
                 trControl = ctrl, 
                 metric = "ROC")


### Find Confusion Matrix for training data
ldaProbsTrainA <- predict(ldamodA, newdata=trainingA, type="prob")[,1]
ldaClassesTrainA <- predict(ldamodA, newdata=trainingA)
confusionMatrix(ldaClassesTrainA, trainingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  4411  1951
#          N  9064 27070
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7408
# Sensitivity : 0.3273          
# Specificity : 0.9328
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLdaTrainA <- roc (response = trainingA$ADMISSIONS,
                          predictor = ldaProbsTrainA,
                          levels = rev(levels(trainingA$ADMISSIONS)))
auc(rocCurveLdaTrainA)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7506
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
ldaProbsTestA1 <- predict(ldamodA, newdata=balancedTestingA, type="prob")[,1]
ldaClassesTestA1 <- predict(ldamodA, newdata=balancedTestingA)
confusionMatrix(data=ldaClassesTestA1, balancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y 1047  458
#          N 2321 6780
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.738
# Sensitivity : 0.31087         
# Specificity : 0.93672
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
ldaCurveCartTestA1 <- roc (response = balancedTestingA$ADMISSIONS,
                           predictor = ldaProbsTestA1,
                           levels = rev(levels(balancedTestingA$ADMISSIONS)))
auc(ldaCurveCartTestA1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.741
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
ldaProbsTestA2 <- predict(ldamodA, newdata=UnbalancedTestingA, type="prob")[,1]
ldaClassesTestA2 <- predict(ldamodA, newdata=UnbalancedTestingA)
confusionMatrix(data=ldaClassesTestA2, UnbalancedTestingA$ADMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  444  458
#          N 1307 6780
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.8036
# Sensitivity : 0.25357         
# Specificity : 0.93672
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
ldaCurveCartTestA2 <- roc (response = UnbalancedTestingA$ADMISSIONS,
                           predictor = ldaProbsTestA2,
                           levels = rev(levels(UnbalancedTestingA$ADMISSIONS)))
auc(ldaCurveCartTestA2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7085
# --- --- --- --- --- --- --- --- --- ---
#----
###############################################################################################################
#####                             Readmission model Building - Logistic                                   #####
###############################################################################################################
#----

### Build model
ctrl <- trainControl(classProbs = TRUE, summaryFunction = twoClassSummary,verboseIter = TRUE)
logitR <- train(READMISSIONS~.,
               data = trainingR,
               method = "glm",
               family = "binomial",
               metric = "ROC", 
               trControl = ctrl,
               na.action=na.exclude)

### Find Confusion Matrix for training data
logitProbsTrainR <- predict(logitR, newdata=trainingR, type="prob")[,1]
logitClassesTrainR <- predict(logitR, newdata=trainingR)
confusionMatrix(data=logitClassesTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  524  342
#          N 1976 5054
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7064
# Sensitivity :  0.20960         
# Specificity :  0.93662
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitTrainR <- roc (response = trainingR$READMISSIONS,
                           predictor = logitProbsTrainR,
                           levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveLogitTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6804
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitProbsTestR1 <- predict(logitR, newdata=balancedTestingR, type="prob")[,1]
logitClassesTestR1 <- predict(logitR, newdata=balancedTestingR)
confusionMatrix(data=logitClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  161   75
#          N  456 1263
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7284
# Sensitivity : 0.26094          
# Specificity : 0.94395
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveLogitTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                          predictor = logitProbsTestR1,
                          levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(rocCurveLogitTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6904
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
logitProbsTestR2 <- predict(logitR, newdata=UnbalancedTestingR, type="prob")[,1]
logitClassesTestR2 <- predict(logitR, newdata=UnbalancedTestingR)
confusionMatrix(data=logitClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   79   75
#          N  320 1263
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7726
# Sensitivity : 0.19799          
# Specificity : 0.94395
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveLogitTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                           predictor = logitProbsTestR2,
                           levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(rocCurveLogitTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6457
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                        Readmission model Building - Logistic Boosted                                #####
###############################################################################################################
#----

### Build model
logitBoostedR <- train(READMISSIONS~.,
                data = trainingR,
                method = "gbm",
                verbose=F,
                trControl=trainControl(method="cv",number=20))

### Find Confusion Matrix for training data
logitProbsBoostedTrainR <- predict(logitBoostedR, newdata=trainingR, type="prob")[,1]
logitClassesBoostedTrainR <- predict(logitBoostedR, newdata=trainingR)
confusionMatrix(data=logitClassesBoostedTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  794  337
#          N 1706 5059
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7413
# Sensitivity :  0.3176         
# Specificity :  0.9375
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitBoostedTrainR <- roc (response = trainingR$READMISSIONS,
                            predictor = logitProbsBoostedTrainR,
                            levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveLogitBoostedTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.7418
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitBoostedProbsTestR1 <- predict(logitBoostedR, newdata=balancedTestingR, type="prob")[,1]
logitBoostedClassesTestR1 <- predict(logitBoostedR, newdata=balancedTestingR)
confusionMatrix(data=logitBoostedClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  123   87
#          N  494 1251
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7028
# Sensitivity : 0.19935          
# Specificity : 0.93498
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveLogitBoostedTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                            predictor = logitBoostedProbsTestR1,
                            levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(rocCurveLogitBoostedTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6852
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
logitBoostedProbsTestR2 <- predict(logitBoostedR, newdata=UnbalancedTestingR, type="prob")[,1]
logitBoostedClassesTestR2 <- predict(logitBoostedR, newdata=UnbalancedTestingR)
confusionMatrix(data=logitBoostedClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   61   87
#          N  338 1251
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7553
# Sensitivity : 0.15288          
# Specificity : 0.93498
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveLogitBoostedTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                            predictor = logitBoostedProbsTestR2,
                            levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(rocCurveLogitBoostedTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6482
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                        Readmission model Building - Logistic Bagged                                #####
###############################################################################################################
#----

### Build model
logitBaggedR <- train(READMISSIONS~.,
                       data = trainingR,
                       method = "treebag",
                       trControl=trainControl(method="cv",number=20))

### Find Confusion Matrix for training data
logitProbsBaggedTrainR <- predict(logitBaggedR, newdata=trainingR, type="prob")[,1]
logitClassesBaggedTrainR <- predict(logitBaggedR, newdata=trainingR)
confusionMatrix(data=logitClassesBaggedTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y 2490    1
#          N   10 5395
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.9986
# Sensitivity :  0.9960         
# Specificity :  0.9998
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLogitBaggedTrainR <- roc (response = trainingR$READMISSIONS,
                                   predictor = logitProbsBaggedTrainR,
                                   levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveLogitBaggedTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 1
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
logitBaggedProbsTestR1 <- predict(logitBaggedR, newdata=balancedTestingR, type="prob")[,1]
logitBaggedClassesTestR1 <- predict(logitBaggedR, newdata=balancedTestingR)
confusionMatrix(data=logitBaggedClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   85   70
#          N  532 1268
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.6921
# Sensitivity : 0.13776          
# Specificity : 0.94768
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveLogitBaggedTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                                   predictor = logitBaggedProbsTestR1,
                                   levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(rocCurveLogitBaggedTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.626
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
logitBaggedProbsTestR2 <- predict(logitBaggedR, newdata=UnbalancedTestingR, type="prob")[,1]
logitBaggedClassesTestR2 <- predict(logitBaggedR, newdata=UnbalancedTestingR)
confusionMatrix(data=logitBaggedClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   47   70
#          N  352 1268
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7571
# Sensitivity : 0.11779          
# Specificity : 0.94768
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveLogitBaggedTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                                   predictor = logitBaggedProbsTestR2,
                                   levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(rocCurveLogitBaggedTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6035
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                             Readmission model Building - CART                                       #####
###############################################################################################################
#----

### Build model
cartmod1R <- train(READMISSIONS~.,
                  data = trainingR,
                  method = "rpart",
                  preProc = c("center","scale"),  # Center and scale data
                  metric="ROC",
                  trControl=ctrl)

### Find Confusion Matrix for training data
cartProbsTrainR <- predict(cartmod1R, newdata=trainingR, type="prob")[,1]
cartClassesTrainR <- predict(cartmod1R, newdata=trainingR)
confusionMatrix(cartClassesTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  195   68
#          N 2305 5328
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.6995
# Sensitivity : 0.07800          
# Specificity : 0.98740
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveCartTrainR <- roc (response = trainingR$READMISSIONS,
                          predictor = cartProbsTrainR,
                          levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveCartTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.612
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
CartProbsTestR1 <- predict(cartmod1R, newdata=balancedTestingR, type="prob")[,1]
CartClassesTestR1 <- predict(cartmod1R, newdata=balancedTestingR)
confusionMatrix(data=CartClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   59   20
#          N  558 1318
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7043
# Sensitivity : 0.09562         
# Specificity : 0.98505
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveCartTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                         predictor = CartProbsTestR1,
                         levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(rocCurveCartTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.636
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
CartProbsTestR2 <- predict(cartmod1R, newdata=UnbalancedTestingR, type="prob")[,1]
CartClassesTestR2 <- predict(cartmod1R, newdata=UnbalancedTestingR)
confusionMatrix(data=CartClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   23   20
#          N  376 1318
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.772
# Sensitivity : 0.05764         
# Specificity : 0.98505
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
rocCurveCartTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                          predictor = CartProbsTestR2,
                          levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(rocCurveCartTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6004
# --- --- --- --- --- --- --- --- --- ---


#----
###############################################################################################################
#####                        Readmission model Building - Random Forest                                   #####
###############################################################################################################
#----

### Build model
RFmodR=train(READMISSIONS~.,
            data = trainingR,
            method="rf",
            importance=T,
            trControl=trainControl(method="cv",number=40,verboseIter = TRUE))


### Find Confusion Matrix for training data
rfProbsTrainR <- predict(RFmodR, newdata=trainingR, type="prob")[,1]
rfClassesTrainR <- predict(RFmodR, newdata=trainingR)
confusionMatrix(data=rfClassesTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y 2500      0
#          N    0   5396 
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 1
# Sensitivity : 1.000         
# Specificity : 1.000
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveRfTrainR <- roc (response = trainingR$READMISSIONS,
                        predictor = rfProbsTrainR,
                        levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveRfTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 1
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
rfProbsTestR1 <- predict(RFmodR, newdata=balancedTestingR, type="prob")[,1]
rfClassesTestR1 <- predict(RFmodR, newdata=balancedTestingR)
confusionMatrix(data=rfClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   82   38
#          N  535 1300
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7069
# Sensitivity :  0.13290         
# Specificity :  0.97160
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
rocCurveRfTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                       predictor = rfProbsTestR1,
                       levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(rocCurveRfTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6651
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for unbalanced testing data
rfProbsTestR2 <- predict(RFmodR, newdata=UnbalancedTestingR, type="prob")[,1]
rfClassesTestR2 <- predict(RFmodR, newdata=UnbalancedTestingR)
confusionMatrix(data=rfClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   39   39
#          N  360 1299
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7703
# Sensitivity :  0.09774         
# Specificity :  0.97085
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on unbalanced testing data
rocCurveRfTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                        predictor = rfProbsTestR2,
                        levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(rocCurveRfTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6363
# --- --- --- --- --- --- --- --- --- ---



#----
###############################################################################################################
#####                              Readmission model Building - LDA                                       #####
###############################################################################################################
#----

### Build model
ldamodR <- train(READMISSIONS~.,
                   data = trainingR,
                   method = "lda",
                   family = "binomial",
                   trControl = ctrl, 
                   metric = "ROC")


### Find Confusion Matrix for training data
ldaProbsTrainR <- predict(ldamodR, newdata=trainingR, type="prob")[,1]
ldaClassesTrainR <- predict(ldamodR, newdata=trainingR)
confusionMatrix(ldaClassesTrainR, trainingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#                Reference
# Prediction     Y     N
#          Y  529   348
#          N 1971 5048
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7063
# Sensitivity : 0.2116          
# Specificity : 0.9355
# --- --- --- --- --- --- --- --- --- ---


### Find the roc curve on training data
rocCurveLdaTrainR <- roc (response = trainingR$READMISSIONS,
                           predictor = ldaProbsTrainR,
                           levels = rev(levels(trainingR$READMISSIONS)))
auc(rocCurveLdaTrainR)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6801
# --- --- --- --- --- --- --- --- --- ---

### Find Confusion Matrix for balanced testing data
ldaProbsTestR1 <- predict(ldamodR, newdata=balancedTestingR, type="prob")[,1]
ldaClassesTestR1 <- predict(ldamodR, newdata=balancedTestingR)
confusionMatrix(data=ldaClassesTestR1, balancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y  153   76
#          N  464 1262
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7238
# Sensitivity : 0.24797         
# Specificity : 0.94320
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on balanced testing data
ldaCurveCartTestR1 <- roc (response = balancedTestingR$READMISSIONS,
                           predictor = ldaProbsTestR1,
                           levels = rev(levels(balancedTestingR$READMISSIONS)))
auc(ldaCurveCartTestR1)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6901
# --- --- --- --- --- --- --- --- --- ---


### Find Confusion Matrix for unbalanced testing data
ldaProbsTestR2 <- predict(ldamodR, newdata=UnbalancedTestingR, type="prob")[,1]
ldaClassesTestR2 <- predict(ldamodR, newdata=UnbalancedTestingR)
confusionMatrix(data=ldaClassesTestR2, UnbalancedTestingR$READMISSIONS)
# --- --- --- --- --- --- --- --- --- --- 
#             Reference
# Prediction    Y    N
#          Y   77   76
#          N  322 1262
# --- --- --- --- --- --- --- --- --- ---
# Accuracy : 0.7709
# Sensitivity : 0.19298         
# Specificity : 0.94320
# --- --- --- --- --- --- --- --- --- ---

### Find the roc curve on testing data
ldaCurveCartTestR2 <- roc (response = UnbalancedTestingR$READMISSIONS,
                           predictor = ldaProbsTestR2,
                           levels = rev(levels(UnbalancedTestingR$READMISSIONS)))
auc(ldaCurveCartTestR2)
# --- --- --- --- --- --- --- --- --- ---
# Area under the curve: 0.6457
# --- --- --- --- --- --- --- --- --- ---

#----
###############################################################################################################
#####                                  Define the cost Matrix - Admission                                 #####
###############################################################################################################
#----

TPCostA <- 0.65
TNCostA <- 0
FPCostA <- 0.25
FNCostA <- 2

#----
###############################################################################################################
#####                                 Cost _ cutoff for Admission Models                                  #####
###############################################################################################################
#----

### get the total cost of each cutoff for Logit model 
cutoff <- 0.07
logit_costA <- NULL
while( cutoff < 0.99){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(logitProbsTest2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "Logistic"
  logit_costA <-rbind(logit_costA,data.frame(cutoff,cost,type))
}


### get the total cost of each cutoff for Logit Boosted model 
cutoff <- 0.07
logitBoosted_costA <- NULL
while( cutoff < 0.92){
  cutoff = cutoff + 0.01
  logitBoosted_result_Approved <- ifelse(logitBoostedProbsTest2 >= cutoff , "T", "F")
  logitBoosted_result_Approved <- relevel(as.factor(logitBoosted_result_Approved),"T")
  cm_logit <- table(logitBoosted_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "Logistic Boosted"
  logitBoosted_costA <-rbind(logitBoosted_costA,data.frame(cutoff,cost,type))
}

### get the total cost of each cutoff for Logit Bagged model 
cutoff <- 0.07
logitBagged_costA <- NULL
while( cutoff < 0.92){
  cutoff = cutoff + 0.01
  logitBagged_result_Approved <- ifelse(logitBaggedProbsTest2 >= cutoff , "T", "F")
  logitBagged_result_Approved <- relevel(as.factor(logitBagged_result_Approved),"T")
  cm_logit <- table(logitBagged_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "Logistic Bagged"
  logitBagged_costA <-rbind(logitBagged_costA,data.frame(cutoff,cost,type))
}


### get the total cost of each cutoff for CART model 
cutoff <- 0.23
CART_costA <- NULL
while( cutoff < 0.71){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(CartProbsTest2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset)+ cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "CART"
  CART_costA <-rbind(CART_costA,data.frame(cutoff,cost,type))
}



### get the total cost of each cutoff for Random Forest model 
cutoff <- 0.1
RF_costA <- NULL
while( cutoff < 0.9){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(rfProbsTest2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "Random Forest"
  RF_costA <-rbind(RF_costA,data.frame(cutoff,cost,type))
}


### get the total cost of each cutoff for LDA model 
cutoff <- 0.07
lda_costA <- NULL
while( cutoff < 0.99){
  cutoff = cutoff + 0.01
  lda_result_Approved <- ifelse(ldaProbsTestA2 >= cutoff , "T", "F")
  lda_result_Approved <- relevel(as.factor(lda_result_Approved),"T")
  cm_logit <- table(lda_result_Approved,UnbalancedTestingA$ADMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCost/nrow(UnbalancedTestingA)*nrow(dataset)+ cm_logit["F","N"]*TNCost/nrow(UnbalancedTestingA)*nrow(dataset)
          + cm_logit["T","N"]*FPCost/nrow(UnbalancedTestingA)*nrow(dataset) + cm_logit["F","Y"]*FNCost/nrow(UnbalancedTestingA)*nrow(dataset))
  # cost<- (cm_logit["T","Y"]*TPCostA + cm_logit["F","N"]*TNCostA
  #         + cm_logit["T","N"]*FPCostA + cm_logit["F","Y"]*FNCostA)
  type <- "LDA"
  lda_costA <-rbind(lda_costA,data.frame(cutoff,cost,type))
}

### plot the cost graph
ggplot( ) +
  geom_line(data=logit_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=logitBoosted_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=logitBagged_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=CART_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=RF_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=lda_costA,aes(x=cutoff, y=cost, color = type), size=1) +
  xlab("cutoff") +
  ggtitle("Cost Based on Cutoff for Admission Models" )


#----
###############################################################################################################
#####                                  Define the cost Matrix - Readmission                               #####
###############################################################################################################
#----

TPCostR <- 1
TNCostR <- 0
FPCostR <- 0.25
FNCostR <- 2

#----
###############################################################################################################
#####                                 Cost _ cutoff for Readmission Models                                #####
###############################################################################################################
#----

### get the total cost of each cutoff for Logit model 
cutoff <- 0.1
logit_costR <- NULL
while( cutoff < 0.9){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(logitProbsTestR2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "Logistic"
  logit_costR <-rbind(logit_costR,data.frame(cutoff,cost,type))
}

### get the total cost of each cutoff for Logit Boosted model 
cutoff <- 0.1
logitBoosted_costR <- NULL
while( cutoff < 0.85){
  cutoff = cutoff + 0.01
  logitBoosted_result_Approved <- ifelse(logitBoostedProbsTestR2 >= cutoff , "T", "F")
  logitBoosted_result_Approved <- relevel(as.factor(logitBoosted_result_Approved),"T")
  cm_logit <- table(logitBoosted_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "Logistic Boosted"
  logitBoosted_costR <-rbind(logitBoosted_costR,data.frame(cutoff,cost,type))
}

### get the total cost of each cutoff for Logit Bagged model 
cutoff <- 0.1
logitBagged_costR <- NULL
while( cutoff < 0.9){
  cutoff = cutoff + 0.01
  logitBagged_result_Approved <- ifelse(logitBaggedProbsTestR2 >= cutoff , "T", "F")
  logitBagged_result_Approved <- relevel(as.factor(logitBagged_result_Approved),"T")
  cm_logit <- table(logitBagged_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "Logistic Bagged"
  logitBagged_costR <-rbind(logitBagged_costR,data.frame(cutoff,cost,type))
}


### get the total cost of each cutoff for CART model 
cutoff <- 0.28
CART_costR <- NULL
while( cutoff < 0.72){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(CartProbsTestR2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "CART"
  CART_costR <-rbind(CART_costR,data.frame(cutoff,cost,type))
}



### get the total cost of each cutoff for Random Forest model 
cutoff <- 0.1
RF_costR <- NULL
while( cutoff < 0.9){
  cutoff = cutoff + 0.01
  logit_result_Approved <- ifelse(rfProbsTestR2 >= cutoff , "T", "F")
  logit_result_Approved <- relevel(as.factor(logit_result_Approved),"T")
  cm_logit <- table(logit_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "Random Forest"
  RF_costR <-rbind(RF_costR,data.frame(cutoff,cost,type))
}



### get the total cost of each cutoff for LDA model 
cutoff <- 0.1
lda_costR <- NULL
while( cutoff < 0.9){
  cutoff = cutoff + 0.01
  lda_result_Approved <- ifelse(ldaProbsTestR2 >= cutoff , "T", "F")
  lda_result_Approved <- relevel(as.factor(lda_result_Approved),"T")
  cm_logit <- table(lda_result_Approved,UnbalancedTestingR$READMISSIONS, dnn = c("predict", "actual"))
  cost<- (cm_logit["T","Y"]*TPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","N"]*TNCostR/nrow(UnbalancedTestingR)*nrow(dataset)
          + cm_logit["T","N"]*FPCostR/nrow(UnbalancedTestingR)*nrow(dataset) + cm_logit["F","Y"]*FNCostR/nrow(UnbalancedTestingR)*nrow(dataset))
  type <- "LDA"
  lda_costR <-rbind(lda_costR,data.frame(cutoff,cost,type))
}


### plot the cost graph
ggplot() +
  geom_line(data=logit_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=logitBoosted_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=logitBagged_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=CART_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=RF_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  geom_line(data=lda_costR,aes(x=cutoff, y=cost, color = type), size=1) +
  xlab("cutoff") +
  ggtitle("Cost Based on Cutoff for Readmission Models" )
  

# ------------------------------------------------------------------------------------------------------------------------------
#     waiting for edit       waiting for edit       waiting for edit       waiting for edit       waiting for edit    
# ------------------------------------------------------------------------------------------------------------------------------
#----
#----
###############################################################################################################
#####                                             Old codes                                               #####
###############################################################################################################
#----
### Scale (standardize and normalize) all the features
# dataset[3:ncol(dataset)] <- scale(dataset[3:ncol(dataset)], center=TRUE, scale=TRUE)
# dataset[3:ncol(dataset)] <- with(dataaset[3:ncol(dataset)], center=TRUE, scale=TRUE)
# #~~with(a, (var2 - min(var2)) / (max(var2) - min(var2)))
# str(dataset$Est_income)

## draw roc 
# par(mfrow=c(1,1))
# plot(rocCurveLogitTrain, le1gacy.axes=T, col = "red", main = "Receiver Operating Characteristics (ROC) Curve")
# legend("bottomright", inset=0, title="Model", border="white",
#        bty= "n", cex = .8, legend = c("Logistic"), fill = c("red"))

### Draw some boxplot of some continues variables
# # boxplot of AGE
# par(mfrow=c(1,2))
# boxplot(dataset$AGE~dataset$ADMISSIONS,data=dataset, main="Age by Admission", 
#         xlab="Admission", ylab="Age")
# boxplot(dataset$AGE~dataset$READMISSIONS,data=dataset, main="Age by Readmission", 
#         xlab="Readmission", ylab="Age")
# # historgram of MONTHS_2016
# par(mfrow=c(1,2))
# int.hist(dataset[which(dataset$ADMISSIONS==1),]$MONTHS_2016)
# title("MONTHS_2016 by Admitted")
# int.hist(dataset[which(dataset$ADMISSIONS==0),]$MONTHS_2016)
# title("MONTHS_2016 by Unadmitted")
# 
# 
# a <- barplot(table(datasetAdmitted$AGE))
# a + barplot(table(datasetUnadmitted$AGE))
# 
# 
# 
# s <- ggplot(dataset, aes(dataset$ADMISSIONS, fill="drv"))
# s + geom_bar(position="dodge")
# 
# str(dataset)
# 
# ### make the dataset balanced for Admission and convert the values of admission to 0 and 1
# NonAdmissionTemp <- subset(dataset, dataset$ADMISSIONS=="0")
# NonAdmissionTemp <- NonAdmissionTemp[sample(nrow(subset(dataset, dataset$ADMISSIONS=="0")),nrow(subset(dataset,dataset$ADMISSIONS!= "0"))), ]
# DatasetForAdmission <- rbind(subset(dataset,dataset$ADMISSIONS!= "0"),NonAdmissionTemp)
# DatasetForAdmission$ADMISSIONS[DatasetForAdmission$ADMISSIONS!="0"] <- 1
# ### make the dataset balanced for Readmission and convert the values of readmission to 0 and 1
# NonReadmissionTemp <- subset(dataset, dataset$READMISSIONS=="0")
# NonReadmissionTemp <- NonReadmissionTemp[sample(nrow(subset(dataset, dataset$READMISSIONS=="0")),nrow(subset(dataset,dataset$READMISSIONS!= "0"))), ]
# DatasetForReadmission <- rbind(subset(dataset,dataset$READMISSIONS!="0"),NonReadmissionTemp)
# DatasetForReadmission$READMISSIONS[DatasetForReadmission$READMISSIONS!="0"] <- 1
# 
# 
# 
# 
# ## Choose the variables for clustering
# 
# 
# 
# 
# 
# 
# # calculate correlation matrix
# correlationMatrix <- cor(dataset)
# 
# # summarize the correlation matrix
# print(correlationMatrix)
# # find attributes that are highly corrected (ideally >0.75)
# highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.5)
# # print indexes of highly correlated attributes
# print(highlyCorrelated)






