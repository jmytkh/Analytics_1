library(data.table)
library(tidyr)
library(dplyr)

# Set working directory (Change Accordingly)
setwd("C:/Users/jerem/Documents/BC2406 Analytics/Project 1")
setwd("C:/Users/Shermine/Desktop/NTU/Year 2 Sem 1/Analytics 1 - Visual & Predictive Techniques/Project/Using/Indicators")

#--------------------------------------------------------------------------
# Importing datasets and Data Cleaning
#-------------------------------------------------------------------------

# Import main datasets
# replace NA values with row mean value only
# choose and rename columns 2010 - 2020
gdp <- fread("Annual GDP Growth.csv")
ind <- which(is.na(gdp), arr.ind=TRUE)
gdp[ind] <- rowMeans(gdp[, 5:65],  na.rm = TRUE)[ind[,1]]
gdp <- gdp[2:267,c(1, 55:65)]
setnames(gdp, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) 

pm25 <- fread("Annual PM 2.5 Exposure.csv")
ind <- which(is.na(pm25), arr.ind=TRUE)
pm25[ind] <- rowMeans(pm25[, 5:65],  na.rm = TRUE)[ind[,1]]
pm25 <- pm25[2:267,c(1, 55:65)]
setnames(pm25, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) 

dpt <- fread("DPT Immunisation.csv")
ind <- which(is.na(dpt), arr.ind=TRUE)
dpt[ind] <- rowMeans(dpt[, 5:65],  na.rm = TRUE)[ind[,1]]
dpt <- dpt[2:267,c(1, 55:65)]
setnames(dpt, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) 

gini <- fread("Gini Coefficient.csv")
ind <- which(is.na(gini), arr.ind=TRUE)
gini[ind] <- rowMeans(gini[, 5:65],  na.rm = TRUE)[ind[,1]]
gini <- gini[2:267,c(1, 55:65)]
setnames(gini, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) 

measles <- fread("Measles Immunisation.csv")
ind <- which(is.na(measles), arr.ind=TRUE)
measles[ind] <- rowMeans(measles[, 5:65],  na.rm = TRUE)[ind[,1]]
measles <- measles[2:267,c(1, 55:65)]
setnames(measles, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020")) 

mortalityRate <- fread("Neonatal Mortality Rate.csv")
ind <- which(is.na(mortalityRate), arr.ind=TRUE)
mortalityRate[ind] <- rowMeans(mortalityRate[, 5:65],  na.rm = TRUE)[ind[,1]]
mortalityRate <- mortalityRate[2:267,c(1, 55:65)]
setnames(mortalityRate, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

slums <- fread("Population Living In Slums.csv")
ind <- which(is.na(slums), arr.ind=TRUE)
slums[ind] <- rowMeans(slums[, 5:65],  na.rm = TRUE)[ind[,1]]
slums <- slums[2:267,c(1, 55:65)]
setnames(slums, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

undernourishment <- fread("Prevalence Of Undernourishment.csv")
ind <- which(is.na(undernourishment), arr.ind=TRUE)
undernourishment[ind] <- rowMeans(undernourishment[, 5:65],  na.rm = TRUE)[ind[,1]]
undernourishment <- undernourishment[2:267,c(1, 55:65)]
setnames(undernourishment, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

weight <- fread("Severe wasting, weight for height.csv")
ind <- which(is.na(weight), arr.ind=TRUE)
weight[ind] <- rowMeans(weight[, 5:65],  na.rm = TRUE)[ind[,1]]
weight <- weight[2:267,c(1, 55:65)]
setnames(weight, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

comDis <- fread("Communicable Diseases.csv")
ind <- which(is.na(comDis), arr.ind=TRUE)
comDis[ind] <- rowMeans(comDis[, 5:65],  na.rm = TRUE)[ind[,1]]
comDis <- comDis[2:267,c(1, 55:65)]
setnames(comDis, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

pol <- fread("Political Stability.csv")
pol <- subset(pol, pol$Indicator=="Political Stability and Absence of Violence/Terrorism, Estimate")
ind <- which(is.na(pol), arr.ind=TRUE)
pol[ind] <- rowMeans(pol[, 6:80],  na.rm = TRUE)[ind[,1]]
pol <- pol[1:192,c(2, 70:80)]
setnames(pol, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

gen <- fread("gendergap.csv")
gen <- subset(gen, gen$Indicator=="Overall Global Gender Gap Index" & gen$`Subindicator Type`=="Index")
ind <- which(is.na(gen), arr.ind=TRUE)
gen[ind] <- rowMeans(gen[, 6:20],  na.rm = TRUE)[ind[,1]]
gen <- gen[1:157,c(2, 10:20)]
setnames(gen, c("Country Name","2010","2011","2012","2013","2014","2015","2016","2017","2018","2019","2020"))

# combining datasets
gdp <- pivot_longer(gdp, cols=2:12, names_to = "Year", values_to = "GDP Growth")
dpt <- pivot_longer(dpt, cols=2:12, names_to = "Year", values_to = "DPT Immunisation")
gini <- pivot_longer(gini, cols=2:12, names_to = "Year", values_to = "Gini Coefficient")
measles <- pivot_longer(measles, cols=2:12, names_to = "Year", values_to = "Measles Immunisation")
mortalityRate <- pivot_longer(mortalityRate, cols=2:12, names_to = "Year", values_to = "Infant Mortality Rate")
pm25 <- pivot_longer(pm25, cols=2:12, names_to = "Year", values_to = "Annual PM 2.5 Exposure")
slums <- pivot_longer(slums, cols=2:12, names_to = "Year", values_to = "Population Living In Slums")
undernourishment <- pivot_longer(undernourishment, cols=2:12, names_to = "Year", values_to = "Prevalence Of Undernourishment")
weight <- pivot_longer(weight, cols=2:12, names_to = "Year", values_to = "Sever wasting, weight for height")
comDis <- pivot_longer(comDis, cols=2:12, names_to = "Year", values_to = "Death From Communicable Diseases")
pol <- pivot_longer(pol, cols=2:12, names_to = "Year", values_to = "Political Stability")
gen <- pivot_longer(gen, cols=2:12, names_to = "Year", values_to = "Overall Global Gender Gap Index")


mergedData <- cbind(gdp,
                    dpt[,c(3)],
                    gini[,c(3)],
                    measles[,c(3)],
                    mortalityRate[,c(3)],
                    pm25[,c(3)],
                    slums[,c(3)],
                    undernourishment[,c(3)],
                    weight[,c(3)],
                    comDis[,c(3)])

mergedData2 <- merge(data.frame(mergedData, row.names=NULL), data.frame(pol[,c(3)], row.names=NULL), by = 0, all = TRUE)[-1]

mergedData3 <- merge(data.frame(mergedData2, row.names=NULL), data.frame(gen[,c(3)], row.names=NULL), by = 0, all = TRUE)[-1]

dataset <- na.omit(mergedData3)
fwrite(dataset, "dataset.csv")


#--------------------------------------------------------------------------
# Linear Regression Codes
#-------------------------------------------------------------------------
library(corrplot)
library(data.table)
library(ggplot2)
library(caTools)
library(car) #for avPlots()
library(tidyr)
library(dplyr)

######################################################################################
set.seed(2021) # set seed for replicable results
dt <- fread("dataset.csv")
dt.lm <- lm(Infant.Mortality.Rate ~ . - Country.Name - Year, data=dt)
summary(dt.lm)
######################################################################################

# remove insignificant variables
dt2 <- dt[,c(5,9,10):=NULL]
dt.lm2 <- lm(Infant.Mortality.Rate ~ . -Country.Name -Year, data=dt2)
summary(dt.lm2)

#vif
vif(dt.lm2)
#there is no need to remove any variablesvif(dt.lm2)


######################################################################################
'''
extra code to remove outliers
for(i in range(1:9)){
  outliers <- boxplot(dt2[[i]], plot = FALSE)$out
  dt2 <- dt2[!(dt2[[i]] %in% outliers), ]
}
'''
######################################################################################

#Plot model 3 diagnostics
plot(dt.lm2)

# Train test split
train <- sample.split(Y = dt2$Infant.Mortality.Rate, SplitRatio = 0.7)
trainset <- subset(dt2, train == T)
testset <- subset(dt2, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Infant.Mortality.Rate)
summary(testset$Infant.Mortality.Rate)

# Develop model on trainset
lm3 <- lm(Infant.Mortality.Rate ~. - Country.Name - Year, data=trainset)
summary(lm3)
residuals(lm3) 

# Residuals = Error = Actual mpg - Model Predicted mpg
RMSE.lm3.train <- sqrt(mean(residuals(lm3)^2))  # RMSE on trainset based on lm3 model.
summary(abs(residuals(lm3)))  # Check Min Abs Error and Max Abs Error.

# Apply model from trainset to predict on testset.
predict.lm3.test <- predict(lm3, newdata = testset)
testset.error <- testset$Infant.Mortality.Rate - predict.lm3.test

# Testset Errors
RMSE.lm3.test <- sqrt(mean(testset.error^2))
summary(abs(testset.error))

RMSE.lm3.train # 6.207103
RMSE.lm3.test  # 6.267764

# plot of actual against predicted value
plot(predict.lm3.test,testset$Infant.Mortality.Rate, 
     xlab="Predicted values of Y",
     ylab="Actual values of Y")
abline(a=0,b=1)



#----------------------------------------------------------------------------------------------
# CART Code
#--------------------------------------------------------------------------------------------

library(caTools)
library(rpart)
library(rpart.plot)

dt <- fread("dataset.csv")

# Generate a random number sequence that can be reproduced to verify results.
# for randomization in 10-fold CV
set.seed(2021)

# split dataset into train-test set
# 70% trainset. Stratify on Y = mortalityRate
train <- sample.split(Y = dt$Infant.Mortality.Rate, SplitRatio = 0.7)
trainset <- subset(dt, train == T)
testset <- subset(dt, train == F)

# Checking the distribution of Y is similar in trainset vs testset.
summary(trainset$Infant.Mortality.Rate)
summary(testset$Infant.Mortality.Rate)

# build CART model
# Continuous Y: Set method = 'anova'
# minsplit value changed to 2 due to the small sample size
# cp value changed to 0 to ensure grow tree to the max
cart1 <- rpart(Infant.Mortality.Rate ~ . - Country.Name - Year - Gini.Coefficient 
               - Population.Living.In.Slums - Prevalence.Of.Undernourishment,
               data = trainset, method = 'anova', control = rpart.control(minsplit = 2, cp = 0))

# plot the maximal tree and results
rpart.plot(cart1, nn = T, main = "Maximal Tree in dataset")

# print the maximal tree (Cart1) onto the console
print(cart1)

## Note: printcp() shows that if you forgot to change the default CP from 0.01 to 0,
## It would have stopped the tree growing process too early. A lot of further growth at CP < 0.01.
## CV error not above 1 hence there are no problem due to imbalanced data.
# prints out the pruning sequence and 10-fold CV errors, as a table
# can be used to identify optimal tree
printcp(cart1)

# display the pruning sequence and 10-fold CV errors, as a chart
# can be used to identify optimal tree
## unable to find the optimal tree as the plot is too compact
plotcp(cart1)

# Compute min CVerror + 1SE in maximal tree cart1.
CVerror.cap <- cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xerror"] + 
  cart1$cptable[which.min(cart1$cptable[,"xerror"]), "xstd"]

# Find the optimal CP region whose CV error is just below CVerror.cap 
# in maximal tree cart1.
i <- 1; j<- 4
while (cart1$cptable[i,j] > CVerror.cap) {
  i <- i + 1
}

# Get geometric mean of the two identified CP values in the optimal region
#if optimal tree has at least one split.
cp.opt = ifelse(i > 1, sqrt(cart1$cptable[i,1] * cart1$cptable[i-1,1]), 1)

# get a specific subtree by pruning the maximal tree (cart1) with a specific value of cp
cart2 <- prune(cart1, cp = cp.opt)

# print the optimal tree (Cart2) onto the console
print(cart2)

# plot the optimal tree and results
# The number inside each node represent the mean value of Y.
rpart.plot(cart2, nn = T, main = "Optimal Tree in dataset")

# Death From Communicable Diseases has the highest importance, 
# Sever Wasting Weight for Height has second importance,
# DPT Immunisation has third importance
cart2$variable.importance
summary(cart2)

residuals(cart2)

# Trainset Errors
# Residuals = Error = Actual Infant.Mortality.Rate - Model Predicted Infant.Mortality.Rate
# RMSE on trainset based on cart2 model.
RMSE.cart2.train <- sqrt(mean(residuals(cart2)^2))

# Check Min Abs Error and Max Abs Error.
summary(abs(residuals(cart2)))

# Apply model from trainset to predict on testset.
predict.cart2.test <- predict(cart2, newdata = testset)

# Testset Errors
# Residuals = Error = Actual Infant.Mortality.Rate - Model Predicted Infant.Mortality.Rate
# RMSE on testset based on cart2 model.
testset.error <- testset$Infant.Mortality.Rate - predict.cart2.test
RMSE.cart2.test <- sqrt(mean(testset.error^2))

# Check Min Abs Error and Max Abs Error.
summary(abs(testset.error))

# RMSE for both trainset and testset
RMSE.cart2.train
RMSE.cart2.test

# plot of actual against predicted value
plot(predict.cart2.test,testset$Infant.Mortality.Rate, 
     xlab="Predicted values of Y",
     ylab="Actual values of Y")
abline(a=0,b=1)


#---------------------------------------------------------------------------------------
# To analyse which country needs more help
#-----------------------------------------------------------------------------------------

dataset <- dt %>% rowwise() %>%
  mutate(Overall.Score = sum(35*Death.From.Communicable.Diseases+15*Measles.Immunisation
                             +1*Overall.Global.Gender.Gap.Index+20*Sever.wasting..weight.for.height
                             +15*DPT.Immunisation+6*Annual.PM.2.5.Exposure+8*GDP.Growth))


fwrite(dataset, "dataset_with_score.csv")

#install.packages("tidyverse")
library(tidyverse)

countryScoreDataset = select(dataset, c(1, 2, 15))

countryScoreDataset <- countryScoreDataset %>% 
  pivot_wider(names_from = Year, values_from = Overall.Score)

summary(dataset)
summary(countryScoreDataset)

countryScoreDataset <- countryScoreDataset %>% rowwise() %>%
  mutate(Average.Score = sum(`2010`,`2011`,`2012`,`2013`,`2014`,`2015`,`2016`,`2017`,`2018`,`2019`,`2020`)/11)

countryScoreDataset <- countryScoreDataset %>% relocate(`2019`, .after = `2018`)
countryScoreDataset <- setorder(countryScoreDataset, -Average.Score)

fwrite(countryScoreDataset, "Overall_Score.csv")

scores <- fread("Overall_Score.csv")
scores

# Display the countries and their corresponding average scores
averagescores <- scores[, c('Country.Name', 'Average.Score')]
averagescores

# Display the top 10 countries with higest scores
head(averagescores, 10)

# Display the top 3 countries with higest scores to make recommendations
head(averagescores, 3)


