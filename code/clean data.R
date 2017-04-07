#######################################################################
### clean data ###
setwd("C:/Users/timdai/Desktop/Stat602/kaggle")
library(rpart)
train <- read.csv('train.csv',header = T, stringsAsFactors = F)
test <- read.csv('test.csv',header = T, stringsAsFactors = F)
dim(test)
dim(train)
test$SalePrice <- rep(NA, dim(test)[1])
total_data <- rbind(train,test)
cha_var <- names(total_data)[which(sapply(total_data, is.character))]
num_var <- names(total_data)[which(sapply(total_data, is.numeric))]
##  check missing values ##
colSums(sapply(total_data, is.na))
colSums(sapply(total_data[,cha_var], is.na))
colSums(sapply(total_data[,num_var], is.na))
## graph missing value ##
#library(Amelia)
#missmap(total_data[,1:80], main = "Missing values in AmesHousing Data", y.labels = NULL, y.at = NULL)

## Alley
total_data$Alley[is.na(total_data$Alley)] <- 'None' 
## Fence
total_data$Fence[is.na(total_data$Fence)] <- 'None'
## MiscFeature
total_data$MiscFeature[is.na(total_data$MiscFeature)] <- 'None'
## FireplaceQu
table(is.na(total_data$FireplaceQu), total_data$Fireplaces == 0)
total_data$FireplaceQu[is.na(total_data$FireplaceQu)] <- 'None'
## Garage
g_names <- c("GarageType", "GarageYrBlt","GarageFinish", "GarageQual","GarageCond")
g1_names <- c("GarageType", "GarageYrBlt","GarageCars","GarageArea", "GarageQual","GarageCond")
table(is.na(total_data$GarageArea))
total_data[is.na(total_data$GarageArea),][59:65]
total_data$GarageArea[is.na(total_data$GarageArea)] <- 1
table(total_data$GarageArea == 0)
total_data[total_data$GarageArea == 0,g_names] <- 'None'
table(is.na(total_data$GarageYrBlt))
total_data$GarageArea[total_data$GarageArea == 1] <- as.character(NA)
total_data[is.na(total_data$GarageYrBlt),59:65]
total_data[c(2127,2577), 59:65]
## predict GarageYrBlt
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
GarageYrBlt_rpart <- rpart(as.factor(GarageYrBlt) ~.,
                           data = total_data[!is.na(total_data$GarageYrBlt),g_names],
                           method = "class",
                           na.action = na.omit)
total_data$GarageYrBlt[is.na(total_data$GarageYrBlt)] <- as.numeric(as.character(predict(GarageYrBlt_rpart,
                                                                                         total_data[is.na(total_data$GarageYrBlt),g_names],
                                                                                         type = "class")))
## predict GarageCars
GarageCars_rpart <- rpart(GarageCars ~.,
                          data = total_data[!is.na(total_data$GarageCars),g1_names],
                          method = "anova",
                          na.action = na.omit)
total_data$GarageCars[is.na(total_data$GarageCars)] <- round(as.numeric(as.character(predict(GarageCars_rpart,
                                                                                       total_data[is.na(total_data$GarageCars),g1_names]))))

## predict GarageArea
GarageArea_rpart <- rpart(GarageArea ~.,
                          data = total_data[!is.na(total_data$GarageArea),g1_names],
                          method = "anova",
                          na.action = na.omit)
total_data$GarageArea[is.na(total_data$GarageArea)] <- round(as.numeric(as.character(predict(GarageArea_rpart,
                                                                                             total_data[is.na(total_data$GarageArea),g1_names]))))

summary(total_data[total_data$GarageType == 'Detchd',59:65])
## predict GarageFinish
total_data[,cha_var] <- lapply(total_data[,cha_var], as.character)
total_data$GarageFinish[is.na(total_data$GarageFinish)] <- 'Unf'
## predict GarageQual
total_data$GarageQual[is.na(total_data$GarageQual)] <- 'TA'
## predict GarageCond
total_data$GarageCond[is.na(total_data$GarageCond)] <- 'TA'
summary(total_data[,59:65])
## for the above 3 categories, I tried use 'rpart' but it doesn't work

## predict MSZoning
table(is.na(total_data$MSZoning))
z_names <- c("Neighborhood","Condition1", "Condition2", "MSZoning")
MSZoning_rpart <- rpart(MSZoning ~.,
                        data = total_data[!is.na(total_data$MSZoning),z_names],
                        method = "class",
                        na.action = na.omit)
total_data$MSZoning[is.na(total_data$MSZoning)] <- as.character(predict(MSZoning_rpart,
                                                                        total_data[is.na(total_data$MSZoning),z_names],
                                                                        type = "class"))

## Predict PoolQC
sum(is.na(total_data$PoolQC[total_data$PoolArea > 0])) # There are 3 houses having pool but no PoolQC
total_data$PoolQC[total_data$PoolArea == 0] <- 'None'
total_data$PoolQC[total_data$PoolArea > 0]
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
pool_names <- c("YearBuilt","YearRemodAdd", "PoolQC", "PoolArea","ExterQual","ExterCond", "YrSold","SaleType","SaleCondition")
PoolQC_rpart <- rpart(as.factor(PoolQC) ~ .,
                      data = total_data[total_data$PoolArea > 0,pool_names],
                      method = "class",
                      na.action = na.omit)
total_data$PoolQC[is.na(total_data$PoolQC)] <- predict(PoolQC_rpart,
                                                       total_data[is.na(total_data$PoolQC),pool_names],
                                                       type = "class")
total_data[,cha_var] <- lapply(total_data[,cha_var], as.character)
total_data$PoolQC[total_data$PoolArea > 0]

## Basement
total_data[is.na(total_data$BsmtExposure) | is.na(total_data$BsmtCond) | is.na(total_data$BsmtFinSF1) |
             is.na(total_data$BsmtFinSF2) | is.na(total_data$BsmtFinType1) | is.na(total_data$BsmtFinType2) |
             is.na(total_data$BsmtFullBath) | is.na(total_data$BsmtHalfBath) | is.na(total_data$BsmtQual) |
             is.na(total_data$BsmtUnfSF) | is.na(total_data$TotalBsmtSF),31:39]
bsmt.index <- is.na(total_data$BsmtCond) & is.na(total_data$BsmtQual)
total_data[bsmt.index,c("BsmtFinSF1", "BsmtFinSF2", "BsmtUnfSF", "TotalBsmtSF")] <- 0
total_data[bsmt.index,c("BsmtQual", "BsmtCond", "BsmtExposure", "BsmtFinType1", "BsmtFinType2")] <- "None"
total_data[bsmt.index,31:39]
pbsmt.index <- (is.na(total_data$BsmtExposure) | is.na(total_data$BsmtCond) | is.na(total_data$BsmtFinSF1) |
  is.na(total_data$BsmtFinSF2) | is.na(total_data$BsmtFinType1) | is.na(total_data$BsmtFinType2) |
  is.na(total_data$BsmtFullBath) | is.na(total_data$BsmtHalfBath) | is.na(total_data$BsmtQual) |
  is.na(total_data$BsmtUnfSF) | is.na(total_data$TotalBsmtSF)) & total_data$TotalBsmtSF > 0
total_data[pbsmt.index,31:39]
b_names <- c(colnames(total_data[,31:39]), "BsmtFullBath","BsmtHalfBath")
## BsmtQual
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
BsmtQual_rpart <- rpart(as.factor(BsmtQual) ~ .,
                      data = total_data[!bsmt.index,b_names],
                      method = "class",
                      na.action = na.omit)
total_data$BsmtQual[is.na(total_data$BsmtQual)] <- predict(BsmtQual_rpart,
                                                       total_data[is.na(total_data$BsmtQual),b_names],
                                                       type = "class")
## BsmtCond
BsmtCond_rpart <- rpart(as.factor(BsmtCond) ~ .,
                        data = total_data[!bsmt.index,b_names],
                        method = "class",
                        na.action = na.omit)
total_data$BsmtCond[is.na(total_data$BsmtCond)] <- predict(BsmtCond_rpart,
                                                           total_data[is.na(total_data$BsmtCond),b_names],
                                                           type = "class")
## BsmtExposure
BsmtExposure_rpart <- rpart(as.factor(BsmtExposure) ~ .,
                            data = total_data[!bsmt.index,b_names],
                            method = "class",
                            na.action = na.omit)
total_data$BsmtExposure[is.na(total_data$BsmtExposure)] <- predict(BsmtExposure_rpart,
                                                                   total_data[total_data$BsmtExposure ,b_names],
                                                                   type = "class")
## BsmtFinType2
BsmtFinType2_rpart <- rpart(as.factor(BsmtFinType2) ~ .,
                            data = total_data[!bsmt.index,b_names],
                            method = "class",
                            na.action = na.omit)
total_data$BsmtFinType2[is.na(total_data$BsmtFinType2)] <- predict(BsmtFinType2_rpart,
                                                                   total_data[is.na(total_data$BsmtFinType2),b_names],
                                                                   type = "class")
## BsmtFullBath
bath_names <- c("BsmtFullBath", "BsmtHalfBath", "FullBath", "HalfBath","BsmtFinSF1", "BsmtFinSF2","BsmtUnfSF","TotalBsmtSF","BsmtQual","BsmtCond")
BsmtFullBath_rpart <- rpart(BsmtFullBath ~.,
                          data = total_data[!is.na(total_data$BsmtFullBath),bath_names],
                          method = "anova",
                          na.action = na.omit)
total_data$BsmtFullBath[is.na(total_data$BsmtFullBath)] <- round(as.numeric(as.character(predict(BsmtFullBath_rpart,
                                                                                             total_data[is.na(total_data$BsmtFullBath),bath_names]))))
## BsmtHalfBath
BsmtHalfBath_rpart <- rpart(BsmtHalfBath ~.,
                            data = total_data[!is.na(total_data$BsmtHalfBath),bath_names],
                            method = "anova",
                            na.action = na.omit)
total_data$BsmtHalfBath[is.na(total_data$BsmtHalfBath)] <- round(as.numeric(as.character(predict(BsmtHalfBath_rpart,
                                                                                                 total_data[is.na(total_data$BsmtHalfBath),bath_names]))))

sum(is.na(total_data[,b_names]))
total_data[,cha_var] <- lapply(total_data[,cha_var], as.character)
## Masonry venner
summary(as.factor(total_data$MasVnrType))
table(total_data$MasVnrArea[total_data$MasVnrType == "None"])
total_data$MasVnrArea <- ifelse(total_data$MasVnrArea == 1,
                                0,total_data$MasVnrArea)
total_data$MasVnrType[total_data$MasVnrArea > 0 & total_data$MasVnrType == "None" & !is.na(total_data$MasVnrType)] <- rep(NA, 4)
total_data[is.na(total_data$MasVnrType) | is.na(total_data$MasVnrArea),c("MasVnrArea", "MasVnrType")]
total_data$MasVnrArea[is.na(total_data$MasVnrType) & is.na(total_data$MasVnrArea)] <- 0
total_data$MasVnrType[is.na(total_data$MasVnrType) & total_data$MasVnrArea == 0] <- 'None'
total_data[is.na(total_data$MasVnrType) | is.na(total_data$MasVnrArea),c("MasVnrArea", "MasVnrType")]
## MasVnrType
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
MasVnrType_rpart <- rpart(as.factor(MasVnrType) ~ .,
                            data = total_data[!is.na(total_data$MasVnrType),c("MasVnrArea", "MasVnrType")],
                            method = "class",
                            na.action = na.omit)
total_data$MasVnrType[is.na(total_data$MasVnrType)] <- predict(MasVnrType_rpart,
                                                               total_data[is.na(total_data$MasVnrType), c("MasVnrArea", "MasVnrType")],
                                                               type = "class")
## Functional
table(is.na(total_data$Functional))
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
Functional_rpart <- rpart(as.factor(Functional) ~ .,
                          data = total_data[!is.na(total_data$Functional),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "Functional")],
                          method = "class",
                          na.action = na.omit)
total_data$Functional[is.na(total_data$Functional)] <- predict(Functional_rpart,
                                                               total_data[is.na(total_data$Functional), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "Functional")],
                                                               type = "class")
## Utilities
table(is.na(total_data$Utilities))
total_data[,cha_var] <- lapply(total_data[,cha_var], as.factor)
Utilities_rpart <- rpart(as.factor(Utilities) ~ .,
                          data = total_data[!is.na(total_data$Utilities),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "Functional","Utilities")],
                          method = "class",
                          na.action = na.omit)
total_data$Utilities[is.na(total_data$Utilities)] <- predict(Utilities_rpart,
                                                               total_data[is.na(total_data$Utilities), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "Functional","Utilities")],
                                                               type = "class")
## Exterior
total_data[is.na(total_data$Exterior1st) | is.na(total_data$Exterior2nd),c("Exterior1st","Exterior2nd")]
Exterior1st_rpart <- rpart(as.factor(Exterior1st) ~ .,
                  data = total_data[!is.na(total_data$Exterior1st),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "ExterCond","ExterQual","Exterior1st","Exterior2nd")],
                  method = "class",
                  na.action = na.omit)
total_data$Exterior1st[is.na(total_data$Exterior1st)] <- predict(Exterior1st_rpart,
                                                             total_data[is.na(total_data$Exterior1st), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "ExterCond","ExterQual","Exterior1st","Exterior2nd")],
                                                             type = "class")
Exterior2nd_rpart <- rpart(as.factor(Exterior2nd) ~ .,
                        data = total_data[!is.na(total_data$Exterior2nd),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "ExterCond","ExterQual","Exterior1st","Exterior2nd")],
                        method = "class",
                        na.action = na.omit)
total_data$Exterior2nd[is.na(total_data$Exterior2nd)] <- predict(Exterior2nd_rpart,
                                                              total_data[is.na(total_data$Exterior2nd), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "ExterCond","ExterQual","Exterior1st","Exterior2nd")],
                                                              type = "class")
total_data[2152,c("Exterior1st","Exterior2nd")]
## Electrical
table(is.na(total_data$Electrical))
# Predict Electrical
e_names <- c("BldgType", "HouseStyle", "OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "Electrical")
Electrical_rpart <- rpart(as.factor(Electrical) ~ .,
                    data = total_data[!is.na(total_data$Electrical),e_names],
                    method = "class",
                    na.action=na.omit)
total_data$Electrical[is.na(total_data$Electrical)] <- as.character(predict(Electrical_rpart, 
                                                            total_data[is.na(total_data$Electrical),e_names], 
                                                            type = "class"))
## KitchenQual
table(is.na(total_data$KitchenQual))
total_data[is.na(total_data$KitchenQual),c("KitchenQual","KitchenAbvGr")]
KitchenQual_rpart <- rpart(as.factor(KitchenQual) ~ .,
                           data = total_data[!is.na(total_data$KitchenQual),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "KitchenQual","KitchenAbvGr")],
                           method = "class",
                           na.action = na.omit)
total_data$KitchenQual[is.na(total_data$KitchenQual)] <- predict(KitchenQual_rpart,
                                                                 total_data[is.na(total_data$KitchenQual), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "KitchenQual","KitchenAbvGr")],
                                                                 type = "class")
## LotFrontage
table(is.na(total_data$LotArea))
table(is.na(total_data$LotShape))
table(is.na(total_data$LotConfig))
table(is.na(total_data$LotFrontage))
## LotFrontage
l_names <- c("LotArea", "LotShape", "LotConfig", "LotFrontage")
LotFrontage_rpart <- rpart(LotFrontage ~.,
                          data = total_data[!is.na(total_data$LotFrontage),l_names],
                          method = "anova",
                          na.action = na.omit)
total_data$LotFrontage[is.na(total_data$LotFrontage)] <- round(as.numeric(as.character(predict(LotFrontage_rpart,
                                                                                             total_data[is.na(total_data$LotFrontage),l_names]))))
## SaleType
table(is.na(total_data$SaleCondition))
table(is.na(total_data$SaleType))
SaleType_rpart <- rpart(as.factor(SaleType) ~ .,
                           data = total_data[!is.na(total_data$SaleType),c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "SaleType")],
                           method = "class",
                           na.action = na.omit)
total_data$SaleType[is.na(total_data$SaleType)] <- predict(SaleType_rpart,
                                                                 total_data[is.na(total_data$SaleType), c("OverallQual", "OverallCond", "YearBuilt", "YearRemodAdd", "SaleCondition", "SaleType")],
                                                                 type = "class")
## check NA's
sapply(total_data[,1:80], function(x) sum(is.na(x)))
