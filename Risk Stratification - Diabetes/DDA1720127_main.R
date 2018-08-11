## Business problem - to identify high-risk diabetic patients through risk stratification.

library(tidyr)
library(dplyr)
library(ggplot2)
library(icd)

## Set the working directory

diabetes_data <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)

str(diabetes_data)

## Data preparation

## Check if any column is redundant

duplicated(colnames(diabetes_data))

## No redundant columns.

## Check for missing values and treat them accordingly.

## there is "?" representing missing values in the data

## replace "?" with NA in the dataframe

diabetes_data <- replace(diabetes_data, diabetes_data =="?", NA)

sapply(X = diabetes_data, FUN = function(x) sum(is.na(x)))

## race, weight, payer code, medical speciality, diag 1, diag 2, diag 3 are variables with NA values


## We do not need race variable so we can ignore it.
## As weight variable has a large number of missing values, we cannot consider this variable for the model, so eliminate it.
## We do not need payer information and medical speciality , so we can ignore it.

## Now, let's derive cormobidity using diag_1, diag_2, diag_3

diabetes_data$diag_1 <- as.icd9(diabetes_data$diag_1)
diabetes_data$diag_2 <- as.icd9(diabetes_data$diag_2)
diabetes_data$diag_3 <- as.icd9(diabetes_data$diag_3)

diabetes_codes <- union(union(unique(diabetes_data[diabetes_data$diag_1 >= 250 & diabetes_data$diag_1 < 251, ]$diag_1),
unique(diabetes_data[diabetes_data$diag_2 >= 250 & diabetes_data$diag_2 < 251, ]$diag_2)),
unique(diabetes_data[diabetes_data$diag_3 >= 250 & diabetes_data$diag_3 < 251, ]$diag_3))

diabetes_codes <- diabetes_codes[!diabetes_codes %in% NA]

circul_codes <- union(union(unique(diabetes_data[diabetes_data$diag_1 >= 390 & diabetes_data$diag_1 <= 459, ]$diag_1),
                            unique(diabetes_data[diabetes_data$diag_2 >= 390 & diabetes_data$diag_2 <= 459, ]$diag_2)),
                      unique(diabetes_data[diabetes_data$diag_3 >= 390 & diabetes_data$diag_3 <= 459, ]$diag_3))
circul_codes <- circul_codes[!circul_codes %in% c(40, 41, 42, NA)]

icddata_1<- data_frame(encounter_id = diabetes_data$encounter_id, diag_1 = diabetes_data$diag_1)
icddata_2<- data_frame(encounter_id = diabetes_data$encounter_id, diag_2 = diabetes_data$diag_2)
icddata_3<- data_frame(encounter_id = diabetes_data$encounter_id, diag_3 = diabetes_data$diag_3)

my_map_1 <- list("diag_1_diab" = diabetes_codes,
                 "diag_1_circ" = circul_codes)
my_map_2 <- list("diag_2_diab" = diabetes_codes,
                 "diag_2_circ" = circul_codes)
my_map_3 <- list("diag_3_diab" = diabetes_codes,
                 "diag_3_circ" = circul_codes)

final_output <- data.frame(comorbid(icddata_1, map = my_map_1))
final_output <- cbind(final_output,data.frame(comorbid(icddata_2, map = my_map_2)))
final_output <- cbind(final_output,data.frame(comorbid(icddata_3, map = my_map_3)))

comorbidity <- ifelse(final_output$diag_1_diab == FALSE & final_output$diag_1_circ == FALSE & final_output$diag_2_diab == FALSE & final_output$diag_2_circ == FALSE & final_output$diag_3_diab == FALSE & final_output$diag_3_circ == FALSE, comorbidity <- 0,
       ifelse((final_output$diag_1_diab == TRUE | final_output$diag_2_diab == TRUE | final_output$diag_3_diab == TRUE) & (final_output$diag_1_circ == FALSE & final_output$diag_2_circ == FALSE & final_output$diag_3_circ == FALSE), comorbidity <- 1,
              ifelse((final_output$diag_1_circ == TRUE | final_output$diag_2_circ == TRUE | final_output$diag_3_circ == TRUE) & (final_output$diag_1_diab == FALSE & final_output$diag_2_diab == FALSE & final_output$diag_3_diab == FALSE), comorbidity <- 2,
                     ifelse((final_output$diag_1_diab == TRUE | final_output$diag_2_diab == TRUE | final_output$diag_3_diab == TRUE) & (final_output$diag_1_circ == TRUE | final_output$diag_2_circ == TRUE | final_output$diag_3_circ == TRUE), comorbidity <- 3,
                            comorbidity <- NA))))

final_output <- cbind(final_output, comorbidity)

diabetes_data <- cbind(diabetes_data, comorbidity = final_output$comorbidity)

## Let's convert all the character variables to factor

diabetes_data <- mutate_if(diabetes_data, is.character, as.factor)

## Exploratory Analysis
## Univariate

## Let's see how many have Diabetes of all patients

ggplot(data = diabetes_data, aes(x = comorbidity)) + geom_bar(fill = 'red')

## Maximum patients have circulatory disease and not diabetes

## Let's analyze how many males and females

ggplot(data = diabetes_data, aes(x = gender)) + geom_bar(fill = 'red')

## Female patients are more as compared to males.

## Let's analyze different age groups

ggplot(data = diabetes_data, aes(x = age)) + geom_bar(fill = 'red')

## maximum people age group is from 50 - 90 yrs

## Let's analyze how many take diabetes medicines

ggplot(data = diabetes_data, aes(x = diabetesMed)) + geom_bar(fill = 'red')

## approx. 77000 takes medicines for diabetes

## Insulin analysis

ggplot(data = diabetes_data, aes(x = insulin)) + geom_bar(fill = 'red')

## maximum patients dont take insulin and many have steady intake, however for some it went high.

## Max glucose serum

ggplot(data = diabetes_data, aes(x = max_glu_serum)) + geom_bar(fill = 'red')

## Lesser patients have glucose level high  

## bivariate analysis

## Let's see how many males and females are suffering from diabetes

ggplot(data = diabetes_data, aes(x = comorbidity, fill = gender)) + geom_bar(position = "dodge") 

## females are at higher risk as compared to males for both diabetes and circulatory diseases.

## Let's analyse  how is the insulin pattern for different comorbidity

ggplot(data = diabetes_data, aes(x = comorbidity, fill = insulin)) + geom_bar(position = "dodge") 

## Let's analyze on diabetes medicines and gender and comorbidity

ggplot(data = diabetes_data, aes(x = gender, fill = diabetesMed)) + geom_bar(position = "dodge") 

## patients have diabetes and readmitted in the hospital

ggplot(data = diabetes_data, aes(x = comorbidity, fill = readmitted)) + geom_bar(position = "dodge")

## Let's create dummy variables for all categorical data

diabetes_data$gender <- ifelse(diabetes_data$gender=="Male",1,0)

diabetes_data$change <- ifelse(diabetes_data$gender=="ch",1,0)

diabetes_data$diabetesMed <- ifelse(diabetes_data$diabetesMed=="Yes",1,0)

diabetes_data$readmitted <- ifelse(diabetes_data$readmitted == "NO", 0, 1)

## Removing weight, payer_code and medical_speciality columns, as they are of not much help. 

diabetes_data <- diabetes_data[, -6]
diabetes_data <- diabetes_data[, -c(10, 11)]

## As we have deribed comorbidity variable so we do not need diag_1, diag_2, diag_3 variables

diabetes_data <- diabetes_data[, - c(16,17,18)]

## We donot require all the medicines columns except insulin, as we are only interested in risk stratification

## diabetes_data <- diabetes_data[, -c(19:35, 37, 38, 39, 40, 41)]

dummy_1 <- data.frame(model.matrix( ~max_glu_serum, data = diabetes_data))
dummy_1 <- dummy_1[,-1]
diabetes_data <- cbind(diabetes_data[,-17], dummy_1)

dummy_2 <- data.frame(model.matrix( ~age, data = diabetes_data))
dummy_2 <- dummy_2[,-1]
diabetes_data <- cbind(diabetes_data[,-5], dummy_2)

dummy_3 <- data.frame(model.matrix( ~A1Cresult, data = diabetes_data))
dummy_3 <- dummy_3[,-1]
diabetes_data <- cbind(diabetes_data[,-16], dummy_3)

## Let's remove examide and citoglipton column as all rows have same value

diabetes_data <- diabetes_data[, -c(31,32)]

# All the factor variables taken into seperate data frame
diabetes_Final_Char <- diabetes_data[, c(16:36)]
str(diabetes_Final_Char)

# Creating dummy variables for factor attributes
dummies<- data.frame(sapply(diabetes_Final_Char, 
                            function(x) data.frame(model.matrix(~x-1,data =diabetes_Final_Char))[,-1]))

final_data_diabetes_model <- cbind(diabetes_data[, -c(16:36)], dummies)

## We dont require race for model building, so let's remove it

final_data_diabetes_model <- final_data_diabetes_model[, -3]

str(final_data_diabetes_model)


## final_data_diabetes_model <- final_data_diabetes_model[, -c(1,2)]

## Model building

## Algorithm - Logistic regression

## Splitting the data between train and test
library(MASS)
library(caTools)
library(car)
library(e1071)

set.seed(100)

indices <- sample.split(final_data_diabetes_model$readmitted, SplitRatio = 0.7)

train = final_data_diabetes_model[indices,]

test = final_data_diabetes_model[!(indices),]


# Initial Model 
model_1 = glm(readmitted ~ ., data = train, family="binomial")
summary(model_1) #AIC: 91626

model_2 <- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                 discharge_disposition_id  + time_in_hospital
               + num_procedures + num_medications + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses 
               + diabetesMed + comorbidity + max_glu_serum.300 + 
                 max_glu_serumNone  + age.10.20. + age.20.30. + 
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_2) #AIC: 91621

# Step wise selection
model_3 = stepAIC(model_2,direction = "both")
summary(model_3) #AIC: 91626


vif(model_3)

## Removing age.20.30 as z value > 0.05

model_4 <- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                 discharge_disposition_id  + time_in_hospital
               + num_procedures + num_medications + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses 
               + diabetesMed + comorbidity + max_glu_serum.300 + 
                 max_glu_serumNone + age.10.20. +
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_4)

vif(model_4)

## Removing age.10.20.


model_5<- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                 discharge_disposition_id  + time_in_hospital
               + num_procedures + num_medications + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses 
               + diabetesMed + comorbidity + max_glu_serum.300 + 
                 max_glu_serumNone + 
                 age.30.40. + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                 age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_5)

vif(model_5)

## Removing age.30.40.

model_6<- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                discharge_disposition_id  + time_in_hospital
              + num_procedures + num_medications + number_outpatient + 
                number_emergency + number_inpatient + number_diagnoses 
              + diabetesMed + comorbidity + max_glu_serum.300 + 
                max_glu_serumNone + age.40.50. + age.50.60. + age.60.70. + age.70.80. + 
                age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_6)

vif(model_6)

## Removing age.40.50. and num_medications

model_7<- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                discharge_disposition_id  + time_in_hospital
              + num_procedures + number_outpatient + 
                number_emergency + number_inpatient + number_diagnoses 
              + diabetesMed + comorbidity + max_glu_serum.300 + 
                max_glu_serumNone + age.50.60. + age.60.70. + age.70.80. + 
                age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_7)

vif(model_7)

## Removing age.50.60.

model_8<- glm(readmitted ~ encounter_id + patient_nbr + gender + 
                discharge_disposition_id  + time_in_hospital
              + num_procedures + number_outpatient + 
                number_emergency + number_inpatient + number_diagnoses 
              + diabetesMed + comorbidity + max_glu_serum.300 + 
                max_glu_serumNone + age.60.70. + age.70.80. + 
                age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_8)

## Removing gender and max_glu_serum.300

model_9 <- glm(readmitted ~ encounter_id + patient_nbr + 
                discharge_disposition_id  + time_in_hospital
              + num_procedures + number_outpatient + 
                number_emergency + number_inpatient + number_diagnoses 
              + diabetesMed + comorbidity + 
                max_glu_serumNone + age.60.70. + age.70.80. + 
                age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_9)

## Removing max_glu_serumNone

model_10 <- glm(readmitted ~ encounter_id + patient_nbr + 
                 discharge_disposition_id  + time_in_hospital
               + num_procedures + number_outpatient + 
                 number_emergency + number_inpatient + number_diagnoses 
               + diabetesMed + comorbidity + age.60.70. + age.70.80. + 
                 age.80.90. + insulin.xNo + insulin.xSteady, data = train, family = "binomial")

summary(model_10)

## All the variable's z value is much less. so we can conclude model_10 to be final model

#----------------------------
# Model Evaluation
#----------------------------

# Predicting the probabilities of attrition for test data
test_pred <- predict(model_10, type = "response", newdata = test[, -17])

# Lets see the summary
summary(test_pred)

test$prob <- test_pred

# Lets use the probability cutoff of 70%
test_pred_risk <- factor(ifelse(test_pred > 0.50, "Yes", "No"))
test_actual_risk <- factor(ifelse(test$readmitted==1,"Yes","No"))

library(caret)

test_conf <- confusionMatrix(test_pred_risk,test_actual_risk,positive = "Yes")

test_conf

# Now lets select the threshold value
perform_fn <- function(cutoff) 
{
  test_pred_risk <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- confusionMatrix(test_pred_risk, test_actual_risk, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# Creating cutoff value from 0.000701 to 0.920678
summary(test_pred)

s = seq(.01,.92,length=100)


OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

# Plot for sensitivity & specificity trend
plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]

# Lets choose cutoff value of 0.14787879 from final model

test_pred_risk <- factor(ifelse(test_pred >=0.5386, "Yes", "No"))

conf_final <- confusionMatrix(test_pred_risk, test_actual_risk, positive = "Yes")
conf_final

acc <- conf_final$overall[1]

sens <- conf_final$byClass[1]

spec <- conf_final$byClass[2]

## Accuracy : 0.6294 
## Sensitivity : 0.4558          
## Specificity : 0.7778

## Let's use another algoorithm - Decision tree

library(rpart)
library(rpart.plot)

diabetes_data_1 <- read.csv("diabetic_data.csv", stringsAsFactors = FALSE)

## replace "?" with NA in the dataframe

diabetes_data_1<- replace(diabetes_data_1, diabetes_data_1 =="?", NA)

sapply(X = diabetes_data_1, FUN = function(x) sum(is.na(x)))

## Let's bind comorbidity to 

diabetes_data_1 <- cbind(diabetes_data_1, comorbidity = final_output$comorbidity)

## Let's convert all the character variables to factor

diabetes_data_1 <- mutate_if(diabetes_data_1, is.character, as.factor)

## Let's remove variables like diag_1, diag_2, diag_3, weight, race, payed code, medical speciality.

diabetes_data_1 <- diabetes_data_1[, -c(3, 6, 11, 12, 19, 20, 21)]

## Changing the column readmitted with > 30 or < 30 as Yes

levels(diabetes_data_1$readmitted)

diabetes_data_1$readmitted <- ifelse(diabetes_data_1$readmitted == "NO", "NO", "YES")

diabetes_data_1$readmitted <- as.factor(diabetes_data_1$readmitted)

diabetes_data_1 <- diabetes_data_1[, -c(33, 34)]

prop.table(table(diabetes_data_1$readmitted))

# divide into train and test set
set.seed(123)
split.indices <- sample(nrow(diabetes_data_1), nrow(diabetes_data_1)*0.7, replace = F)
train_1 <- diabetes_data_1[split.indices, ]
test_1 <- diabetes_data_1[-split.indices, ]

#1 build tree model- default hyperparameters

tree.model <- rpart(readmitted ~ .,                     
                    data = train_1,                   
                    method = "class",               
                    parms = list(split = "information")
)

# display decision tree
prp(tree.model)

# make predictions on the test set
tree.predict <- predict(tree.model, test_1, type = "class")


# evaluate the results
confusionMatrix(tree.predict, test_1$readmitted, positive = "YES") 


#5 Cross test to choose CP 
library(caret)

# set the number of folds in cross test to 5
tree.control = trainControl(method = "cv", number = 5)

# set the search space for CP
tree.grid = expand.grid(cp = seq(0, 0.02, 0.0025))

# train model
tree.model <- train(readmitted ~ .,
                    data = train_1,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = tree.control,
                    tuneGrid = tree.grid,
                    control = rpart.control(minsplit = 50,
                                            minbucket = 20))

# look at cross validated model results
tree.model

# look at best value of hyperparameter
tree.model$bestTune

# make predictions on test set
tree.predict <- predict.train(tree.model, test_1)

# accuracy
test_conf2 <- confusionMatrix(tree.predict, test_1$readmitted)  

## Accuracy : 0.6385
## Sensitivity : 0.7786          
## Specificity : 0.4730


