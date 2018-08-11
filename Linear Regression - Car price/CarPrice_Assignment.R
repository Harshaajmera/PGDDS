###################Geely Auto Analysis #####################################
############################################################################
library(stringr)
library(car)
library(MASS)
library(tidyr)
library(dplyr)

## Set working directory and import the dataframe

carprice <- read.csv("CarPrice_Assignment.csv", stringsAsFactors = FALSE) 

## Let's see the structure

str(carprice)

## Step 1 -  Data Cleaning 
## CarName variable has company name and car model. Let's derive the column as company_name, car_model

carprice <- separate(carprice, CarName, c('CompanyName', 'Model'), sep = " ")

unique(carprice$CompanyName)
## Let's clean Car company names 
## Nisaan and nisaan

carprice$CompanyName <- tolower(carprice$CompanyName)

## toyota and toyouta

carprice$CompanyName[which(carprice$CompanyName == "toyouta")] <- "toyota"

## porsche and porcshce

carprice$CompanyName[which(carprice$CompanyName == "porcshce")] <- "porsche"

## vw and vokswagen and volkswagen

carprice$CompanyName[which(carprice$CompanyName == "vw" | carprice$CompanyName == "vokswagen")] <- "volkswagen"

## maxda and mazda

carprice$CompanyName[which(carprice$CompanyName == "maxda")] <- "mazda"

unique(carprice$CompanyName)

## Let's drop model variable as per problem statement, it is not to be consider as indepedent variable

carprice <- carprice[, -4]

## Let's see if there any duplicate values

sum(duplicated(carprice)) ## no duplicate rows in carprice

## Let's see if there are missing values

sum(is.na(carprice)) ## no missing values

## Data Preparation 

str(carprice)

## Changing all character variables to factor

## CompanyName

carprice$CompanyName <- as.factor(carprice$CompanyName)

## fueltype, aspiration

carprice$fueltype <- as.factor(carprice$fueltype)

carprice$aspiration <- as.factor(carprice$aspiration)

## doornumber

unique(carprice$doornumber)

carprice$doornumber[which(carprice$doornumber == "two")] <- "2"
carprice$doornumber[which(carprice$doornumber == "four")] <- "4"

carprice$doornumber <- as.integer(carprice$doornumber)

## carbody, drivewheel, engine location, engine type, fuel system

carprice$carbody <- as.factor(carprice$carbody)
carprice$drivewheel <- as.factor(carprice$drivewheel)
carprice$enginelocation <- as.factor(carprice$enginelocation)
carprice$enginetype <- as.factor(carprice$enginetype)
carprice$fuelsystem <- as.factor(carprice$fuelsystem)

## cylinder number

unique(carprice$cylindernumber)

carprice$cylindernumber[which(carprice$cylindernumber=="four")] <- 4
carprice$cylindernumber[which(carprice$cylindernumber=="six")] <- 6
carprice$cylindernumber[which(carprice$cylindernumber=="five")] <- 5
carprice$cylindernumber[which(carprice$cylindernumber=="eight")] <- 8
carprice$cylindernumber[which(carprice$cylindernumber=="two")] <- 2
carprice$cylindernumber[which(carprice$cylindernumber=="three")] <- 3
carprice$cylindernumber[which(carprice$cylindernumber=="twelve")] <- 12

carprice$cylindernumber <- as.integer(carprice$cylindernumber)

## Now let's see if there are any outliers in columns with number as datatype

## wheelbase, carlength, carwidth, carheight, boreratio, stroke, compressionratio

quantile(carprice$wheelbase, seq(0, 1, 0.01))

## one outlier at 99 - 100 %

quantile(carprice$carlength, seq(0,1,0.01))

## one outlier at 99 - 100%

quantile(carprice$carwidth, seq(0,1,0.01))

## no outliers present

quantile(carprice$carheight, seq(0,1,0.01))

## no outliers present

quantile(carprice$boreratio, seq(0,1,0.01))

## oultier present at 99 -100%

quantile(carprice$compressionratio, seq(0,1,0.01))

## outlier present at 90 - 91%

quantile(carprice$stroke, seq(0,1,0.01))

## outlier present at 99 - 100%
## Since the data set is small, we need not treat outliers.
## Now let's create dummy variables for all factor variables
## fueltype
levels(carprice$fueltype)<-c(1,0)
carprice$fueltype <- as.numeric(levels(carprice$fueltype))[carprice$fueltype]

## aspiration

levels(carprice$aspiration)<-c(1,0)
carprice$aspiration <- as.numeric(levels(carprice$aspiration))[carprice$aspiration]

## engine location

levels(carprice$enginelocation)<-c(1,0)
carprice$enginelocation <- as.numeric(levels(carprice$enginelocation))[carprice$enginelocation]


## CompanyName

dummy_1 <- data.frame(model.matrix( ~CompanyName, data = carprice))
dummy_1 <- dummy_1[, -1]
carprice <- cbind(carprice[, -3], dummy_1)

## Carbody

dummy_2 <- data.frame(model.matrix(~carbody, data = carprice))
dummy_2 <- dummy_2[, -1]
carprice <- cbind(carprice[, -6], dummy_2)

## drivewheel

dummy_3 <- data.frame(model.matrix(~drivewheel, data = carprice))
dummy_3 <- dummy_3[, -1]
carprice <- cbind(carprice[, -6], dummy_3)

## enginetype

dummy_4 <- data.frame(model.matrix(~enginetype, data = carprice))
dummy_4 <- dummy_4[, -1]
carprice <- cbind(carprice[, -12], dummy_4)

## fuel system

dummy_5 <- data.frame(model.matrix(~fuelsystem, data = carprice))
dummy_5 <- dummy_5[, -1]
carprice <- cbind(carprice[, -14], dummy_5)


# Divide into training and test data set
#set the seed to 100, let's run it 
set.seed(100)

# randomly generate row indices for train dataset
trainindices= sample(1:nrow(carprice), 0.7*nrow(carprice))
# generate the train data set
train = carprice[trainindices,]

#Similarly store the rest of the observations into an object "test".
test = carprice[-trainindices,]

##### Creating the regression models using StepAIC.

lm_model1 = lm(price~.,data = train)
summary(lm_model1)

### Multiple R-squared:  0.9814,	Adjusted R-squared:  0.97 

# In stepAIC function, we pass our first model i.e lm_model1 and 
# direction is set as both, because in stepwise,  both the forward selection 
# of variables and backward elimination of variables happen simultaneously.

step_lm_model1<- stepAIC(lm_model1, direction="both")
step_lm_model1

## Droping the variables which are insignificant as per stepAIC. 
## Also droping variables with NA as p value

lm_model2 <- lm(formula = price ~ car_ID + aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + stroke + peakrpm + citympg + CompanyNameaudi + 
                  CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                  CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                  CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                  CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                  CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                  CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                  fuelsystemspdi, data = train)

summary(lm_model2)

## Let's see the VIF values now

vif(lm_model2)

## Multiple R-squared:  0.9805,	Adjusted R-squared:  0.9736
## Car_ID is highly correated, has highest vif value, let's remove and see

lm_model3 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + stroke + peakrpm + citympg + CompanyNameaudi + 
                  CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                  CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                  CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                  CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                  CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                  CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                  fuelsystemspdi, data = train)

summary(lm_model3)

## Multiple R-squared:  0.9789,	Adjusted R-squared:  0.9718 
## Let's see what happens if we remove peakrpm

vif(lm_model3)

lm_model4 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + stroke + citympg + CompanyNameaudi + 
                  CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                  CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                  CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                  CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                  CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                  CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                  fuelsystemspdi, data = train)

summary(lm_model4)

## Multiple R-squared:  0.9749,	Adjusted R-squared:  0.9666 

vif(lm_model4)

## We can get rid of citympg as vif is above 2 and p value > 0.05

lm_model5 <- lm(formula = price ~ aspiration + enginelocation + carwidth + 
                  curbweight + enginesize + stroke + CompanyNameaudi + 
                  CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                  CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                  CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                  CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                  CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                  CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                  carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                  drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                  fuelsystemspdi, data = train)

summary(lm_model5)

## Multiple R-squared:  0.9742,	Adjusted R-squared:  0.9661 

vif(lm_model5)

## We can get rid of curbweight as VIF is too high and pvalue is ~ 0.05

lm_model6<- lm(formula = price ~ aspiration + enginelocation + carwidth
                  + enginesize + stroke + CompanyNameaudi + 
                 CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                 CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                 carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
                 drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                 fuelsystemspdi, data = train)

summary(lm_model6)

## Multiple R-squared:  0.9733,	Adjusted R-squared:  0.9652 

vif(lm_model6)

## Carbody variables are highly correlated and are less significant, let's see after getting rid of them

lm_model7<- lm(formula = price ~ aspiration + enginelocation + carwidth
               + enginesize + stroke + CompanyNameaudi + 
                 CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                 CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                 CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                 CompanyNamepeugeot + CompanyNameplymouth + CompanyNameporsche + 
                 CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                 CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                 drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                 fuelsystemspdi, data = train)

summary(lm_model7)

## Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9645

vif(lm_model7)

## Let's get rid of CompanyNameporsche as high vif value and high p-value

lm_model8<-lm(formula = price ~ aspiration + enginelocation + carwidth
              + enginesize + stroke + CompanyNameaudi + 
                CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                CompanyNamepeugeot + CompanyNameplymouth + 
                CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                drivewheelrwd + enginetyperotor + fuelsystem2bbl + fuelsystemmpfi + 
                fuelsystemspdi, data = train)

summary(lm_model8)

## Multiple R-squared:  0.9717,	Adjusted R-squared:  0.9647 

vif(lm_model8)

## Let's get rid of fuelsystem2bbl, fuelsystemspdi as p-value > 0.05 and vif is also high

lm_model9 <- lm(formula = price ~ aspiration + enginelocation + carwidth
                + enginesize + stroke + CompanyNameaudi + 
                  CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                  CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                  CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                  CompanyNamepeugeot + CompanyNameplymouth + 
                  CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                  CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                  drivewheelrwd + enginetyperotor + fuelsystemmpfi
                  , data = train)

summary(lm_model9)

## Multiple R-squared:  0.9707,	Adjusted R-squared:  0.9641

vif(lm_model9)

## We can try getting rid of fuelsystemmpfi a as p-value is > 0.05 and vif is also high

lm_model10 <- lm(formula = price ~ aspiration + enginelocation + carwidth
                 + enginesize + stroke + CompanyNameaudi + 
                   CompanyNamebmw + CompanyNamechevrolet + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                   drivewheelrwd + enginetyperotor
                 , data = train)


summary(lm_model10)

## Multiple R-squared:  0.9703,	Adjusted R-squared:  0.9639

vif(lm_model10)

## Let's get rid of CompanyNamechevrolet as no significance.

lm_model11 <- lm(formula = price ~ aspiration + enginelocation + carwidth
                 + enginesize + stroke + CompanyNameaudi + 
                   CompanyNamebmw + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                   drivewheelrwd + enginetyperotor
                 , data = train)

summary(lm_model11)


## Multiple R-squared:  0.9698,	Adjusted R-squared:  0.9637

vif(lm_model11)

## Let's get rid of carwidth and see as vif is high.

lm_model12 <- lm(formula = price ~ aspiration + enginelocation
                 + stroke + CompanyNameaudi + enginesize +
                   CompanyNamebmw + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                   drivewheelrwd + enginetyperotor
                 , data = train)

summary(lm_model12)

## Multiple R-squared:  0.9609,	Adjusted R-squared:  0.9533 

vif(lm_model12)

## Let's remove CompanyNameaudi

lm_model13 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize + stroke + 
                   CompanyNamebmw + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo + 
                   drivewheelrwd + enginetyperotor
                 , data = train)


summary(lm_model13)

## Multiple R-squared:  0.9596,	Adjusted R-squared:  0.9523

vif(lm_model13)

## Let's remove drivewheelrwd


lm_model14 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize + stroke + 
                   CompanyNamebmw + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo 
                    + enginetyperotor
                 , data = train)

summary(lm_model14)

## Multiple R-squared:  0.9584,	Adjusted R-squared:  0.9512

vif(lm_model14)

## Let's remove CompanyNamebmw as p value is high as compared with others

lm_model15 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize + stroke 
                    + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda + 
                   CompanyNamemercury + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesaab + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo 
                 + enginetyperotor
                 , data = train)

summary(lm_model15)

## Multiple R-squared:  0.9567,	Adjusted R-squared:  0.9496 

vif(lm_model15)

## If we remove CompanyNamesaab, CompanyNamemercury then

lm_model16 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize + stroke 
                 + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda
                   + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen + CompanyNamevolvo 
                 + enginetyperotor
                 , data = train)

summary(lm_model16)

## Multiple R-squared:   0.95,	Adjusted R-squared:  0.9428 

vif(lm_model16)

## If we remove CompanyNamevolvo then

lm_model17 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize + stroke 
                 + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda
                 + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen
                 + enginetyperotor
                 , data = train)

summary(lm_model17)

## Multiple R-squared:  0.9478,	Adjusted R-squared:  0.9407 

vif(lm_model17)

## Now see if we can remove stroke as well

lm_model18 <- lm(formula = price ~ aspiration + enginelocation
                 + enginesize   + CompanyNamedodge + 
                   CompanyNamehonda + CompanyNameisuzu + CompanyNamemazda
                 + CompanyNamemitsubishi + CompanyNamenissan + 
                   CompanyNamepeugeot + CompanyNameplymouth + 
                   CompanyNamerenault + CompanyNamesubaru + 
                   CompanyNametoyota + CompanyNamevolkswagen
                 + enginetyperotor
                 , data = train)

summary(lm_model18)

## Multiple R-squared:  0.9435,	Adjusted R-squared:  0.9363 

vif(lm_model18)


## All vif's and p-value looks good. So, we can stop here.

## aspiration 
## enginelocation
## enginesize   
## CompanyNamedodge 
## CompanyNamehonda 
## CompanyNameisuzu 
## CompanyNamemazda
## CompanyNamemitsubishi 
## CompanyNamenissan 
## CompanyNamepeugeot
## CompanyNameplymouth 
## CompanyNamerenault
## CompanyNamesubaru 
## CompanyNametoyota  
## CompanyNamevolkswagen
## enginetyperotor

## Let's see the accuracy of our model

Predict_1 <- predict(lm_model18,test[,-21])

test$test_price <- Predict_1

r <- cor(test$price,test$test_price)

rsquared <- cor(test$price,test$test_price)^2

rsquared

### As we observed Model 18 R^2 and Adjusted R^2 we are Getting above 90% accuracy in Training data
### Same we Predit in the Test data and getting approximately 13% Error.... 
