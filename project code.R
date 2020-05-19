rm(list=ls(all=TRUE))
## Loading the data###
setwd("G:\\Project")
Saudiarabia_data <- read.csv("Saudi Arabia.csv",header = T, sep = ",")

##Checking for NAs..
sum(is.na(Saudiarabia_data))
summary(Saudiarabia_data)

##There are are 1532 NAs, hence imputing the values using Time series Imputation..
library("imputeTS")
Main_data = na.locf(Saudiarabia_data, option = "nocb", na.remaining = "rev")
sum(is.na(Main_data))
summary(Main_data)

##Finding collinearity b/w the variables####
###library(corrplot)
##library(caret)
##data2 <- cor(Main_data)
##write.csv(data2,"data2.csv",row.names = F)
##findCorrelation(data2,cutoff = 0.9,verbose = T)

##Removing Year from Main_data and train
Main_data$Year=NULL

#Scaling the data
##library(vegan)
##Main_data_std <- data.frame(decostand(Main_data,method = "standardize"),"Foreign.direct.investment..net.inflows....of.GDP." = Main_data$Foreign.direct.investment..net.inflows....of.GDP.)
## Performing Random Forest to get the top variables based on the NodePurity levels
require(randomForest)
rf_DL <- randomForest(Foreign.direct.investment..net.inflows....of.GDP. ~ ., data=Main_data, keep.forest=TRUE, ntree=30)
rf_DL

# importance of attributes
(rf_DL$importance)
round(importance(rf_DL), 2)
importanceValues = data.frame(attribute=rownames(round(importance(rf_DL), 2)),
                              MeanDecreaseGini = round(importance(rf_DL), 2))
importanceValues = importanceValues[order(-importanceValues$IncNodePurity),]

str(importanceValues)
# Top 30 Important attributes
Top30ImpAttrs = as.character(importanceValues$attribute[1:30])
Top30ImpAttrs
varImpPlot(rf_DL)

###Considering the top 30 variables to run linear regression##

name<-c("Foreign.direct.investment..net.inflows....of.GDP.",
        "Net.foreign.assets..current.LCU.",                                  
        "Net.primary.income..BoP..current.US..",                                
        "Total.reserves..includes.gold..current.US..",                     
        "Service.imports..BoP..current.US..",                        
        "GDP.growth..annual...",      
        "Commercial.service.imports..current.US..",                       
        "Real.effective.exchange.rate.index..2010...100.",                                                 
        "Inflation..GDP.deflator..annual...",
        "Permanent.cropland....of.land.area.",                                  
        "Reserves.and.related.items..BoP..current.US..",                       
        "Energy.imports..net....of.energy.use.",                            
        "Trade....of.GDP.",                
        "Primary.income.receipts..BoP..current.US..",                                                 
        "Energy.use..kg.of.oil.equivalent.per.capita.",                
        "Net.income.from.abroad..current.US..",         
        "Primary.income.payments..BoP..current.US.." ,                              
        "Domestic.credit.to.private.sector.by.banks....of.GDP.",                      
        "Claims.on.central.government..etc.....GDP.",                          
        "Goods.imports..BoP..current.US.." ,                     
        "Adjusted.savings..natural.resources.depletion....of.GNI.",                           
        "Railways..passengers.carried..million.passenger.km." ,                                 
        "Age.dependency.ratio..young....of.working.age.population." ,                               
        "Food.exports....of.merchandise.exports." ,                          
        "Imports.of.goods.and.services....of.GDP.",
        "Population.growth..annual...",
        "Final.consumption.expenditure..etc.....of.GDP.",
        "Merchandise.trade....of.GDP.",
        "Industry..value.added..annual...growth.",
        "Import.value.index..2000...100.",
        "Ores.and.metals.exports....of.merchandise.exports.")

Top30<-Main_data[names(Main_data)%in% name]

##Running the model on top30 attributes data###
LinReg1<-lm(Foreign.direct.investment..net.inflows....of.GDP. ~ ., data=Top30)
summary(LinReg1)
plot(LinReg1)

## Running stepAIC model to find out the significant variables
library(MASS)
stepAIC(LinReg1, direction = "both")
LinReg_AIC1<-lm(Foreign.direct.investment..net.inflows....of.GDP.~ Commercial.service.imports..current.US..+Imports.of.goods.and.services....of.GDP.+
                  Net.income.from.abroad..current.US..+ Primary.income.receipts..BoP..current.US..+Import.value.index..2000...100.,data=Main_data)
summary(LinReg_AIC1)


#################Using Lasso Regression#############

####Defining target variable####
#Target Varaible
##Main_data1 <- as.matrix(data.frame(Main_data))
##str(Main_data1)
##dim(Main_data1)
##y=Main_data$Foreign.direct.investment..net.inflows....of.GDP.[trainRows]
##ytest = Main_data$Foreign.direct.investment..net.inflows....of.GDP.[-trainRows]
##head(ytest)
# Lasso Regression  using glmnet - L1 norm
##library(glmnet)

##summary(train)
# fit model
##fit1 <- glmnet(train,y,alpha = 0,family = "gaussian")

#plot(fit1,xvar="lambda",label=TRUE)
#plot(fit1,xvar="dev",label=TRUE)

###TimeSeries###

library(DataCombine)

r=data.frame()
r
j=2

summary_matrix = matrix(nrow=120, ncol = 4, dimnames = list(NULL, c('Var1','LagValue','R2','AdjR2')))
counter = 1

for(j in 2:31)
{
  for (i in 1:4)
  {
    v = colnames(Top30[j])
    x = slide(Top30, Var= v , NewVar = v, slideBy = -i)
    model2 =lm(Foreign.direct.investment..net.inflows....of.GDP. ~.,data = x)
    r[i,(j-1)]=summary(model2)$r.squared 
    summary_matrix[counter,] = c(v,-i,summary(model2)$adj.r.squared,summary(model2)$r.squared)
    print(summary_matrix)
    print(summary(model2)$adj.r.squared)
    counter = counter + 1
  }
 }

Final_result= as.data.frame(summary_matrix)
Final_result

write.csv(Final_result,"data.csv", row.names=F)

##install.packages("vars")
##library(vars)
##res <- subset(Main_data,select=c(Foreign.direct.investment..net.inflows....of.GDP.,
                                 ##Commercial.service.imports..current.US..,
                                 ##Inflation..GDP.deflator..annual...,
                                 ##Net.income.from.abroad..current.US..,
                                 ##Net.primary.income..BoP..current.US..,
                                 ##Primary.income.payments..BoP..current.US..,
                                 ##Primary.income.receipts..BoP..current.US..,
                                 ##Service.imports..BoP..current.US..,Trade....of.GDP.))
#res1<-diff(res)

##head(res)
##arMod <- VAR(res[,c(1:4)],type="both",lag.max = 3,ic="AIC")
##varMod
##summary(varMod)


