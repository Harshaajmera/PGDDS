#BUSINESS UNDERSTANDING
#CMS rates hospitals in the US on a scale of 1-5 with the objective to make it easier for 
#patients and consumers to compare the quality of hospitals. 

#PROBLEM STATEMENT 
#The purpose of this project is to develop an approach to calculate Hospital Ratings as done by CMS.

#DATA UNDERSTANDING
#
#CMS collected information for various measures through different quality reporting programs and surveys conducted in hospitals. All the measures were grouped based on the quality measure they define. The measures in the data are broadly classified into below Measure Groups.
#1.	Mortality
#2.	Safety of Care
#3.	Readmission
#4.	Patient Experience
#5.	Effectiveness of Care
#6.	Timeliness of Care
#7.	Efficient Use of Medical Imaging 
#Models is build for each of the Measure Groups and a weighted approach is used for star rating calculations from these measure group scores.

#CMS collected data through various surveys and was made available in the for of
# 58 CSV files
# The 64 measures being measured are scattered oin few of these files 
# The list of all the measures required for analysis were first classified to measure group they belong to and mapped to the files they are available in
# The required measures were then extracted into respective sheet for Measure group
# EDA was performed on these Measure Groups to arive at the group score


#Include the required libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(randomForest)
library(cowplot)
library(GGally)
#************************************************************************************************
#
# DATA CLEANING and PREPARATION

#************************************************************************************************
## MEASURE GROUP 1- MORTALITY
## There are 7 Measures for Mortality that were available in below two files
#File 1: Readmission and Deaths - Hospital.csv
#File 2: Complication - Hospital.csv
#Death rate for heart attack patients	MORT_30_AMI
#Death rate for coronary artery bypass graft (CABG) surgery patients	MORT_30_CABG
#Death rate for chronic obstructive pulmonary disease (COPD) patients	MORT_30_COPD
#Death rate for heart failure patients	MORT_30_HF
#Death rate for pneumonia patients	MORT_30_PN
#Death rate for stroke patients	MORT_30_STK
#Deaths among patients with serious treatable complications after surgery	PSI_4_SURG_COMP


#Read the Readmissions and Deaths - Hospital.csv and fetch the measures available in the sheet
readm_death_hospital <- read.csv("Readmissions and Deaths - Hospital.csv", stringsAsFactors = FALSE)
mortality_measures <- filter(readm_death_hospital, Measure.ID %in% c("MORT_30_AMI", "MORT_30_CABG", "MORT_30_COPD", "MORT_30_HF", "MORT_30_PN", "MORT_30_STK"))
mortalityMeasures <- mortality_measures[, c("Provider.ID", "Measure.ID", "Score")]
mortalityMeasures1 <- spread(mortalityMeasures, Measure.ID, Score, convert = F)

#Read the Complications - Hospital.csv and fetch the measures available in the sheet
complication_hospital <- read.csv("Complications - Hospital.csv", stringsAsFactors = FALSE)
complication_measures <- complication_hospital[, c("Provider.ID", "Measure.ID", "Score")]
complication_measure1 <- spread(complication_measures, Measure.ID, Score, convert = F)

## Final sheet for Mortality measures
mortalityMeasures1 <- merge(x = mortalityMeasures1, y = complication_measure1[, c("Provider.ID", "PSI_4_SURG_COMP")], by = "Provider.ID")


#************************************************************************************************
## MEASURE GROUP 2- SAFETY OF CARE
## There are 8 Measures for Safety and Care that were available in below  files
#File 1: Healthcare Associated Infections - Hospital.csv
#Central Line Associated Bloodstream Infection (ICU + select Wards)	HAI_1_SIR ,
#Central Line Associated Bloodstream Infection (ICU only)	HAI_1a_SIR
#Catheter Associated Urinary Tract Infections (ICU + select Wards)	HAI_2_SIR
#Catheter Associated Urinary Tract Infections (ICU only)	HAI_2a_SIR
#Surgical site infections from colon surgery (SSI: Colon)	HAI_3_SIR
#Surgical site infections from abdominal hysterectomy (SSI: Hysterectomy)	HAI_4_SIR
#Methicillin-resistant Staphylococcus Aureus (MRSA) Blood Laboratory-identified Events (Bloodstream infections)	HAI_5_SIR
#Clostridium difficile (C.diff.) Laboratory-identified Events (Intestinal infections)	HAI_6_SIR
#Rate of complications for hip/knee replacement patients	COMP_HIP_KNEE
#Serious complications	PSI_90_SAFETY


#Read the Healthcare Associated Infections - Hospital.csv and fetch the measures available in the sheet
health_assoc_infect_hospital <- read.csv("Healthcare Associated Infections - Hospital.csv", stringsAsFactors = FALSE)
unique(data.frame(health_assoc_infect_hospital$Measure.ID,health_assoc_infect_hospital$Measure.Name))
safety_care_measures <- filter(health_assoc_infect_hospital, Measure.ID %in% c("HAI_1_SIR", "HAI_1a_SIR", "HAI_2_SIR", "HAI_2a_SIR", "HAI_3_SIR", "HAI_4_SIR", "HAI_5_SIR", "HAI_6_SIR"))
safety_care_measures <- safety_care_measures[, c("Provider.ID", "Measure.ID", "Score")]
safety_care_measures1 <- spread(safety_care_measures, Measure.ID, Score, convert = F)

## Final sheet for safety measures
safety_care_measures1 <- merge(x = safety_care_measures1, y = complication_measure1[, c("Provider.ID", "COMP_HIP_KNEE", "PSI_90_SAFETY")], by = "Provider.ID")


#************************************************************************************************
## MEASURE GROUP 3- READMISSIONS
## There are 9 Measures for Readmissions that were available in below  files
#File 1: Readmission and Deaths - Hospital.csv
#Hospital Return Days for heart attack patients	READM_30_AMI
#Rate of unplanned readmission for coronary artery bypass graft (CABG) surgery patients	READM_30_CABG
#Rate of unplanned readmission for chronic obstructive pulmonary disease (COPD) patients	READM_30_COPD
#Hospital return days for heart failure patients	READM_30_HF
#Rate of unplanned readmission after hip/knee surgery	READM_30_HIP_KNEE
#Rate of unplanned readmission for pneumonia patients	READM_30_PN
#Rate of unplanned readmission for stroke patients	READM_30_STK
#Rate of unplanned readmission after discharge from hospital (hospital-wide)	READM_30_HOSP_WIDE
#Rate of unplanned hospital visits after an outpatient colonoscopy	


#Read the Readmissions and Deaths - Hospital.csv and fetch the measures available in the sheet
Readmission_measures <- filter(readm_death_hospital, Measure.ID %in% c("READM_30_AMI", "READM_30_CABG", "READM_30_COPD", "READM_30_HF", "READM_30_HIP_KNEE", "READM_30_PN", "READM_30_STK", "READM_30_HOSP_WIDE"))
Readmission_measures <- Readmission_measures[, c("Provider.ID", "Measure.ID", "Score")]
#Fina; sheet for Readmission Measures
Readmission_measures1 <- spread(Readmission_measures, Measure.ID, Score, convert = F)

#************************************************************************************************
## MEASURE GROUP 4- PATIENT EXPERIENCE
## There are 11 Measures for Readmissions that were available in below  files
#File 1: HCAHPS - Hospital.csv
#Patients who reported that their nurses communicated well	H_COMP_1_STAR_RATING
#Patients who reported that their doctors communicated well	H_COMP_2_STAR_RATING
#Patients who reported that they received help as soon as they wanted	H_COMP_3_STAR_RATING
#Patients who reported that their pain was well controlled	H_COMP_4_STAR_RATING
#Patients who reported that staff explained about medicines before giving it to them	H_COMP_5_STAR_RATING
#Patients who reported that their room and bathroom were clean	H_CLEAN_STAR_RATING
#Patients who reported that the area around their room was quiet at night	H_QUIET_STAR_RATING
#Patients who reported that they were given information about what to do during their recovery at home	H_COMP_6_STAR_RATING
#Patients who understood their care when they left the hospital	H_COMP_7_STAR_RATING
#Patients who gave their hospital a rating on a scale from 0 (lowest) to 10 (highest)	H_HSP_RATING_STAR_RATING
#Patients who would recommend the hospital to their friends and family	H_RECMND_STAR_RATING

#Read the HCAHPS - Hospital.csv and fetch the required problems
HCAHPS_hospital <- read.csv("HCAHPS - Hospital.csv", stringsAsFactors = FALSE)
patient_exp_measure <- filter(HCAHPS_hospital, HCAHPS.Measure.ID %in% c("H_COMP_1_STAR_RATING", "H_COMP_2_STAR_RATING", "H_COMP_3_STAR_RATING", "H_COMP_4_STAR_RATING", "H_COMP_5_STAR_RATING", "H_CLEAN_STAR_RATING", "H_QUIET_STAR_RATING", "H_COMP_6_STAR_RATING", "H_COMP_7_STAR_RATING", "H_HSP_RATING_STAR_RATING", "H_RECMND_STAR_RATING"))
patient_exp_measure <- patient_exp_measure[, c("Provider.ID", "HCAHPS.Measure.ID", "Patient.Survey.Star.Rating")]
## Final sheet for Patience experience
patient_exp_measure1 <- spread(patient_exp_measure, HCAHPS.Measure.ID, Patient.Survey.Star.Rating, convert = F)

#************************************************************************************************
## MEASURE GROUP 5- TIMELINESS OF CARE
## There are 7 Measures for Timeliness of care that were available in below  files
#FILE : Timely and Effective Care - Hospital.csv
#Average (median) time patients spent in the emergency department, before they were admitted to the hospital as an inpatient	ED_1b
#Average (median) time patients spent in the emergency department, after the doctor decided to admit them as an inpatient before leaving the emergency department for their inpatient room	ED_2b
#Average (median) number of minutes before outpatients with chest pain or possible heart attack who needed specialized care were transferred to another hospital	OP_20
#Average (median) number of minutes before outpatients with chest pain or possible heart attack got an ECG	OP_23
#Average (median) time patients spent in the emergency department before leaving from the visit	OP_18b
#Average (median) time patients spent in the emergency department before they were seen by a healthcare professional	OP_22
#Average (median) time patients who came to the emergency department with broken bones had to wait before getting pain medication	OP_21

timely_eff_care_hospital <- read.csv("Timely and Effective Care - Hospital.csv", stringsAsFactors = FALSE)
timeliness_measures <- filter(timely_eff_care_hospital, Measure.ID %in% c("ED_1b", "ED_2b", "OP_20", "OP_23", "OP_18b", "OP_22", "OP_21"))
timeliness_measures <- timeliness_measures[, c("Provider.ID", "Measure.ID", "Score")]

## Final sheet for Timeliness of care
timeliness_measures1 <- spread(timeliness_measures, Measure.ID, Score, convert = F)

#************************************************************************************************
## MEASURE GROUP 6- EFFICIENT USE oF MEDICAL IMAGING
## There are 5 Measures for Efficient Use of Medical Imaging that were available in below  files
#FILE : Outpatient Imaging Efficiency - Hospital.csv

#Outpatients with low-back pain who had an MRI without trying recommended treatments first, such as physical therapy	OP_8
#Outpatient CT scans of the abdomen that were "combination" (double) scans	OP_10
#Outpatient CT scans of the chest that were "combination" (double) scans	OP_11
#Outpatients who got cardiac imaging stress tests before low-risk outpatient surgery	OP_13
#Outpatients with brain CT scans who got a sinus CT scan at the same time	OP_14

outpat_imag_eff_hospital <- read.csv("Outpatient Imaging Efficiency - Hospital.csv", stringsAsFactors = FALSE)
eff_imaging_measure <- filter(outpat_imag_eff_hospital, Measure.ID %in% c("OP_8", "OP_10", "OP_11", "OP_13", "OP_14"))
eff_imaging_measure <- eff_imaging_measure[, c("Provider.ID", "Measure.ID", "Score")]

## Ffinal sheet for Efficient use of Medical Imaging
eff_imaging_measure1 <- spread(eff_imaging_measure, Measure.ID, Score, convert = F)

#************************************************************************************************
## MEASURE GROUP 7- EFFECTIVENESS OF CARE
## There are 5 Measures for Efficient Use of Medical Imaging that were available in below  files
#FILE : Outpatient Imaging Efficiency - Hospital.csv
#Patients assessed and given influenza vaccination	IMM_2
#Healthcare workers given influenza vaccination	IMM_3_OP_27_FAC_ADHPCT
#Outpatients with chest pain or possible heart attack who received aspirin within 24 hours of arrival or before transferring from the emergency department	OP_4
#Percentage of patients who left the emergency department before being seen	OP_22
#Percentage of patients who came to the emergency department with stroke symptoms who received brain scan results within 45 minutes of arrival	OP_23
#Percentage of patients receiving appropriate recommendation for follow-up screening colonoscopy	OP_29
#Percentage of patients with history of polyps receiving follow-up colonoscopy in the appropriate timeframe	OP_30
#Percent of mothers whose deliveries were scheduled too early (1-2 weeks early), when a scheduled delivery was not medically necessary	PC_01
#Patients who developed a blood clot while in the hospital who did not get treatment that could have prevented it	VTE_6
#Percentage of patients receiving appropriate radiation therapy for cancer that has spread to the bone	- could not find


eff_care_measures <- filter(timely_eff_care_hospital, Measure.ID %in% c("IMM_2", "IMM_3_OP_27_FAC_ADHPCT", "OP_4", "OP-22", "OP-23", "OP_29", "OP_30", "PC_01", "VTE-6"))
eff_care_measures <- eff_care_measures[, c("Provider.ID", "Measure.ID", "Score")]

## Final sheet for Effectiveness of Care
eff_care_measures1 <- spread(eff_care_measures, Measure.ID, Score, convert = F)

#************************************************************************************
## clubbing all the sheets to one
CMS_rating <- merge(mortalityMeasures1, safety_care_measures1, by = "Provider.ID")
CMS_rating <- merge(CMS_rating, Readmission_measures1, by = "Provider.ID")
CMS_rating <- merge(CMS_rating, patient_exp_measure1, by = "Provider.ID")
CMS_rating <- merge(CMS_rating, timeliness_measures1, by = "Provider.ID")
CMS_rating <- merge(CMS_rating, eff_care_measures1, by = "Provider.ID")
CMS_rating <- merge(CMS_rating, eff_imaging_measure1, by = "Provider.ID")


str(CMS_rating)

## Let's replace all Not Available values to 0 in the scores
CMS_rating[] <- lapply(CMS_rating, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)

## Lets append Hospital overall rating 
Hospital_info <- read.csv("Hospital General Information.csv", stringsAsFactors = FALSE)
str(Hospital_info)

Hospital_rating <- Hospital_info[, c(1, 13)]

CMS_rating <- merge(CMS_rating, Hospital_rating, by = "Provider.ID")

## Let's convert all the measure to integer
## and convert Hospital overall rating to factor
CMS_rating$Hospital.overall.rating <- as.factor(CMS_rating$Hospital.overall.rating)


measures <- c(names(CMS_rating[, 2:55]))

CMS_rating[, measures] <-  data.frame(sapply(CMS_rating[, 2:55], as.double))

str(CMS_rating)
str(Hospital_rating)

## We have 7 sheets for each Measure Group
## We have one consolidated sheet with all the measures and the over all rating field
## Let start Exploratory Data Analysis

par(mfrow=c(3,3))
## Univariate analysis
plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating)) + geom_bar() 
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_AMI)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_CABG)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_COPD)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_HF)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_PN)) + geom_boxplot()
plot7 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = MORT_30_STK)) + geom_boxplot()
plot8 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = PSI_4_SURG_COMP)) + geom_boxplot()

plot_grid(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8, labels ="Auto")


Correlation(x = CMS_rating[, 2:8], CMS_rating[, 2:8])
ggpairs(CMS_rating[, 2:8])

#Measure Group 1 - Plots Inferences
## Most of the hospitals have got 3 star rating
## Low rating Hospitals has have Death rate of heart attack patients
## Hospital rated 3 have a lot of outliers.
## High rated hospitals have low Death rate of pneumonia patients.
## Low rated hospitals have high Death rate of stroke patients.
## No much inference from the plot.




## Plots for Measure Group 2 - Safety of Care
par(mfrow=c(3,3))
plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_1_SIR)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_1a_SIR)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_2_SIR)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_2a_SIR)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_3_SIR)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_4_SIR)) + geom_boxplot()
plot7 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_5_SIR)) + geom_boxplot()
plot8 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = HAI_6_SIR)) + geom_boxplot()
plot9 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = COMP_HIP_KNEE)) + geom_boxplot()
plot10 <-ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = PSI_90_SAFETY)) + geom_boxplot()

plot_grid(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9, labels ="Auto")


cor(x = CMS_rating[, 9:18], CMS_rating[, 9:18])
ggpairs(CMS_rating , column= 9:18, ggplot2::aes(color=Hospital.overall.rating))
## Hospitals with low rating have more serious surgical complications.

#Inferences - Measure Group 2
#High Correlation for Last two Measures with target variable
#Hospitals with Lower Rating have High Values for PSI_90_SAFETY and COMP_HIP_KNEE


#Plots - Measure Group 3 - Readmissions
str(Readmission_measures1)

plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_AMI)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_STK)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_CABG)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_HIP_KNEE)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_HOSP_WIDE)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_COPD)) + geom_boxplot()
plot7 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_HF)) + geom_boxplot()
plot8 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = READM_30_PN)) + geom_boxplot()
plot_grid(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8, labels ="Auto")


cor(x = CMS_rating[, 19:26], CMS_rating[, 19:26])
ggpairs(CMS_rating , column= 19:26, ggplot2::aes(color=Hospital.overall.rating))

## Hospitals with lower rating have high readmission rate for heart attack & stroke patients
## Highest rated hospital have lower readmissions of COPD, heart failure & pneumonia  patients

#Measure Group 4 - Plots - Patience Experience
str(patient_exp_measure1)
par(mfrow=c(4,3))
plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_CLEAN_STAR_RATING)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_1_STAR_RATING)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_2_STAR_RATING)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_3_STAR_RATING)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_4_STAR_RATING)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_5_STAR_RATING)) + geom_boxplot()
plot7 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_6_STAR_RATING)) + geom_boxplot()
plot8 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_COMP_7_STAR_RATING)) + geom_boxplot()
plot9 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_HSP_RATING_STAR_RATING)) + geom_boxplot()
plot10 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_QUIET_STAR_RATING)) + geom_boxplot()
plot11 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = H_RECMND_STAR_RATING)) + geom_boxplot()

plot_grid(plot1, plot2,plot3,plot4,plot5,plot6,plot7,plot8,plot9,plot10,plot11, labels ="Auto")
cor(x = CMS_rating[, 27:37], CMS_rating[, 27:37])
ggpairs(CMS_rating , column= 27:37, ggplot2::aes(color=Hospital.overall.rating))



#Measure Group 5 - Plots - Timeliness of Care
plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = ED_1b)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = ED_2b)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_18b)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_20)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_21)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_22)) + geom_boxplot()
plot7 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_23)) + geom_boxplot()
plot_grid(plot1, plot2,plot3,plot4,plot5,plot6,plot7, labels ="Auto")


## Hospitals with low rating have high average time for patients diagnosis
cor(CMS_rating[, 38:44], CMS_rating[, 38:44])
ggpairs(CMS_rating , column= 38:44, ggplot2::aes(color=Hospital.overall.rating))



#Measure Group 6 - Plots - Effectiveness Of Care

str(eff_care_measures1)

plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = IMM_2)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = IMM_3_OP_27_FAC_ADHPCT)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_29)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_30)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_4)) + geom_boxplot()
plot6 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = PC_01)) + geom_boxplot()

plot_grid(plot1, plot2,plot3,plot4,plot5,plot6, labels ="Auto")

cor(CMS_rating[, 45:50], CMS_rating[, 45:50])
ggpairs(CMS_rating , column= 45:50, ggplot2::aes(color=Hospital.overall.rating))

#Measure Group 7 - Medical Imaging

plot1 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_10)) + geom_boxplot()
plot2 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_11)) + geom_boxplot()
plot3 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_13)) + geom_boxplot()
plot4 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_14)) + geom_boxplot()
plot5 <- ggplot(data = CMS_rating, aes(x = Hospital.overall.rating, y = OP_8)) + geom_boxplot()

plot_grid(plot1, plot2,plot3,plot4,plot5, labels ="Auto")

cor(CMS_rating[, 51:55], CMS_rating[, 51:55])
ggpairs(CMS_rating , column= 51:55, ggplot2::aes(color=Hospital.overall.rating))
## Higher rated hospitals have high percent of vaccinations

.


CMS_rating1 <- CMS_rating

## scaling all the nummeric variables
CMS_rating[, measures] <- scale(CMS_rating[, 2:55])

#######################################################################################
#MODELLING

#########################################################################################
#PART 1 - SUPERVISED LEARNING - RANDOM FOREST
#Lets predict all the target variable using all the other factors and RANDOM forecast::

str(CMS_rating)
CMS_rating_backup <- CMS_rating

#Shuffle the data
# Shuffle the data
shuffledata <- CMS_rating[sample(nrow(CMS_rating)), ]

# Split the data into train and test
ntrain <- as.integer(nrow(shuffledata)*0.8)
traindata_RF <- shuffledata[1:ntrain, ]
testdata_RF <- shuffledata[(ntrain+1):nrow(shuffledata), ]

# Build the random forest
set.seed(71)
data.rf <- randomForest(Hospital.overall.rating ~ ., data=traindata_RF, proximity=FALSE,
                        ntree=1000, mtry=5, do.trace=TRUE, na.action=na.omit)
data.rf
#Importance Factor of Each Variable
importance(data.rf)

#Plot Random Forest Results
plot(data.rf)
varImpPlot(data.rf)

testPred_RF <- predict(data.rf, newdata=testdata_RF)
table(testPred_RF, testdata_RF$Hospital.overall.rating)

#Call:
#  randomForest(formula = Hospital.overall.rating ~ ., data = traindata_RF,      proximity = FALSE, ntree = 1000, mtry = 5, do.trace = TRUE,      na.action = na.omit) 
#Type of random forest: classification
#Number of trees: 1000
#No. of variables tried at each split: 5

#OOB estimate of  error rate: 21.46%
#Confusion matrix:
#  1   2    3   4  5 Not Available class.error
#1             17  71    0   0  0             1  0.80898876
#2              0 345  211   0  0             6  0.38612100
#3              0  42 1231  72  0            53  0.11945637
#4              0   1  267 501  3            20  0.36742424
#5              0   0    0  46 25             9  0.68750000
#Not Available  0   0    7  14  4           908  0.02679528
#> testPred_RF <- predict(data.rf, newdata=testdata_RF)
#> table(testPred_RF, testdata_RF$Hospital.overall.rating)

#testPred_RF       1   2   3   4   5 Not Available
##1               4   0   0   0   0             0
#2              23  65  10   0   0             0
#3               0  54 325  60   0             3
#4               0   0  24 106  21             7
#5               0   0   0   0   7             0
#Not Available   1   3  15   6   3           227
  
#Calculating the Accuracy from Confusion Matrix

#Random FOrest - Prediction Accuracy on TRAIN = 78.54%

#Random Foresr - Prediction Accuracy on TEST = 76.14%

########################################################################################

#PART 2 - UNSUPEVISED METHOD - CLUSTERING BASED RATE CALCULATION

#Since the predictions for Supervised learning methods were not satisfactory
#Lets use Clustering Based approach to predict the results
#Unsupervised Method will use the below approach
#1 - Factor Analysis for Each Measure Group to come up with Group Score
#2 - Weighted Average Method on the Factors for each Measure Group to Calculate the Final score

###################################################################################
# Part 1 -  This method uses factor analysis (or latent variable analysis) to find the weight of each measure in respective groups.

par(mfrow=c(1,1))
#Factor Analysis for Measure Group 1 - Mortality

##Step 1: Determining the number of factors via PCA
## Let's replace all Not Available values to 0 in the scores
mortalityMeasures1[] <- lapply(mortalityMeasures1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
#Convert all the measure values as numeric
mortalityMeasures1[, 2:8] <-  data.frame(sapply(mortalityMeasures1[, 2:8], as.double))
mortalityMeasures1[, 2:8] <- scale(mortalityMeasures1[, 2:8])
sum(is.na(mortalityMeasures1))

survey_mortality_measures<- princomp(mortalityMeasures1[, 2:8],cor=TRUE)
summary(survey_mortality_measures)
plot(survey_mortality_measures)

str(survey_mortality_measures)

### Based on summary received, we will be using the factor value as  
## It has to be noted that in the analysis, we will also be performing regression for scoring
## Factors: 1 ,2,3 were triied.Getting best results for Factor 3
## Rotation: varimax (default)
## Scores: Regression
survey_mortality_measures_fact <- factanal (mortalityMeasures1[, 2:8],
                                      factors = 3,
                                      rotation = "varimax",
                                      scores = "regression")
survey_mortality_measures_fact
head(survey_mortality_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
mortalityMeasures1$Mortality_GroupScore <- (survey_mortality_measures_fact$scores[,1]^2 + survey_mortality_measures_fact$scores[,2]^2 + survey_mortality_measures_fact$scores[,3]^2)/3
str(mortalityMeasures1)

#Factor Analysis for Measure Group 2 - Safety Of Care

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(safety_care_measures1)
safety_care_measures1[] <- lapply(safety_care_measures1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
safety_care_measures1[, 2:11] <-  data.frame(sapply(safety_care_measures1[, 2:11], as.double))
safety_care_measures1[, 2:11] <- scale(safety_care_measures1[, 2:11])
sum(is.na(safety_care_measures1))

##Step 1: Determining Measure Group Score via PCA
survey_safety_care_measures<- princomp(safety_care_measures1[, 2:11],cor=TRUE)
summary(survey_safety_care_measures)
plot(survey_safety_care_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1  ## Rotation: varimax (default) ## Scores: Regression
survey_safety_care_measures_fact <- factanal (safety_care_measures1[, 2:11],
                                            factors = 2,
                                            rotation = "varimax",
                                            scores = "regression")
survey_safety_care_measures_fact #Getting p value 0 for any value of factor.So taking 1 only
head(survey_safety_care_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
safety_care_measures1$SafetyCare_GroupScore <- (survey_safety_care_measures_fact$scores[,1]^2 +survey_safety_care_measures_fact$scores[,2]^2)/2

#Factor Analysis for Measure Group 3 - Readmissions
str(Readmission_measures1)

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(Readmission_measures1)
Readmission_measures1[] <- lapply(Readmission_measures1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
Readmission_measures1[, 2:9] <-  data.frame(sapply(Readmission_measures1[, 2:9], as.double))
Readmission_measures1[, 2:9] <- scale(Readmission_measures1[, 2:9])
sum(is.na(Readmission_measures1))

##Step 1: Determining Measure Group Score via PCA
Readmission_measures<- princomp(Readmission_measures1[, 2:9],cor=TRUE)
summary(Readmission_measures)
plot(Readmission_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1,2,3,4,5 tried ## Rotation: varimax (default) ## Scores: Regression
Readmission_measures_fact <- factanal (Readmission_measures1[, 2:9],
                                              factors = 4,
                                              rotation = "varimax",
                                              scores = "regression")
Readmission_measures_fact #Getting the best value of P when factor =4 .Taking scores when factor =4
head(survey_safety_care_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
Readmission_measures1$Readmission_GroupScore <- (Readmission_measures_fact$scores[,1]^2+Readmission_measures_fact$scores[,2]^2+Readmission_measures_fact$scores[,3]^2+Readmission_measures_fact$scores[,4]^2)/4


#Factor Analysis for Measure Group 4 - Patient Experience
str(patient_exp_measure1)

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(patient_exp_measure1)
patient_exp_measure1[] <- lapply(patient_exp_measure1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
patient_exp_measure1[, 2:12] <-  data.frame(sapply(patient_exp_measure1[, 2:12], as.double))
patient_exp_measure1[, 2:12] <- scale(patient_exp_measure1[, 2:12])
sum(is.na(patient_exp_measure1))

##Step 1: Determining Measure Group Score via PCA
PatientExperience_measures<- princomp(patient_exp_measure1[, 2:12],cor=TRUE)
summary(PatientExperience_measures)
plot(PatientExperience_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1  ## Rotation: varimax (default) ## Scores: Regression
PatientExperience_measures_fact <- factanal (patient_exp_measure1[, 2:12],
                                       factors = 6,
                                       rotation = "varimax",
                                       scores = "regression")
PatientExperience_measures_fact #Getting best p value when Factors=6 .Taking scores when factors =6
head(PatientExperience_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
patient_exp_measure1$PatientExperience_GroupScore <- (PatientExperience_measures_fact$scores[,1]^2
                                                      +PatientExperience_measures_fact$scores[,2]^2
                                                      +PatientExperience_measures_fact$scores[,3]^2
                                                      +PatientExperience_measures_fact$scores[,4]^2
                                                      +PatientExperience_measures_fact$scores[,5]^2
                                                      +PatientExperience_measures_fact$scores[,6]^2)/6

#Factor Analysis for Measure Group 5 - Timeliness of Care

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(timeliness_measures1)
timeliness_measures1[] <- lapply(timeliness_measures1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
timeliness_measures1[, 2:8] <-  data.frame(sapply(timeliness_measures1[, 2:8], as.double))
timeliness_measures1[, 2:8] <- scale(timeliness_measures1[, 2:8])
sum(is.na(timeliness_measures1))

##Step 1: Determining Measure Group Score via PCA
Timeliness_measures<- princomp(timeliness_measures1[, 2:8],cor=TRUE)
summary(Timeliness_measures)
plot(Timeliness_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1  ## Rotation: varimax (default) ## Scores: Regression
Timeliness_measures_fact <- factanal (timeliness_measures1[, 2:8],
                                             factors = 3,
                                             rotation = "varimax",
                                             scores = "regression")
Timeliness_measures_fact #Getting best values when Factor=3
head(Timeliness_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
timeliness_measures1$Timeliness_GroupScore <- (Timeliness_measures_fact$scores[,1]^2
                                               +Timeliness_measures_fact$scores[,2]^2
                                               +Timeliness_measures_fact$scores[,3]^2)/3

#Factor Analysis for Measure Group 6 - Effectiveness of Care

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(eff_care_measures1)
eff_care_measures1[] <- lapply(eff_care_measures1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
eff_care_measures1[, 2:7] <-  data.frame(sapply(eff_care_measures1[, 2:7], as.double))
eff_care_measures1[, 2:7] <- scale(eff_care_measures1[, 2:7])
sum(is.na(eff_care_measures1))

##Step 1: Determining Measure Group Score via PCA
EffectiveCare_measures<- princomp(eff_care_measures1[, 2:7],cor=TRUE)
summary(EffectiveCare_measures)
plot(EffectiveCare_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1  ## Rotation: varimax (default) ## Scores: Regression
EffectiveCare_measures_fact <- factanal (eff_care_measures1[, 2:7],
                                      factors = 2,
                                      rotation = "varimax",
                                      scores = "regression")
EffectiveCare_measures_fact
head(EffectiveCare_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
eff_care_measures1$EffectiveCare_measures_GroupScore <- (EffectiveCare_measures_fact$scores[,1]^2
                                                         +EffectiveCare_measures_fact$scores[,2]^2)/2
eff_care_measures1


#Factor Analysis for Measure Group 7 - Medical Imaging

#Prepare Measure group data set a. Replace Not Available  with 0 for Measure b.Convert all Measures to Numeric
str(eff_imaging_measure1)
eff_imaging_measure1[] <- lapply(eff_imaging_measure1, gsub, pattern = "Not Available", replacement = 0, fixed = TRUE)
eff_imaging_measure1[, 2:6] <-  data.frame(sapply(eff_imaging_measure1[, 2:6], as.double))
eff_imaging_measure1[, 2:6] <- scale(eff_imaging_measure1[, 2:6])
sum(is.na(eff_imaging_measure1))

##Step 1: Determining Measure Group Score via PCA
EffectiveImaging_measures<- princomp(eff_imaging_measure1[, 2:6],cor=TRUE)
summary(EffectiveImaging_measures)
plot(EffectiveImaging_measures)

##Step 2 : Determine the Measure Group Score Using Factor
## Factors: 1  ## Rotation: varimax (default) ## Scores: Regression
EffectiveImaging_measures_fact <- factanal (eff_imaging_measure1[, 2:6],
                                         factors = 2,
                                         rotation = "varimax",
                                         scores = "regression")
EffectiveImaging_measures_fact
head(EffectiveImaging_measures_fact$scores)
#Combining the n factors to come up with score(Sum of squares of each factor / Number of factors)
eff_imaging_measure1$EffectiveImaging_measures_GroupScore <- (EffectiveImaging_measures_fact$scores[,1]^2
                                                              +EffectiveImaging_measures_fact$scores[,2]^2)/2


#End of Factor Anavysis
# Group Scores are available in the Individual Sheet for Measures
# Extracting all Group scores in sheet

CMS_rating$Mortality_GroupScore <- mortalityMeasures1$Mortality_GroupScore
CMS_rating$SafetyCare_GroupScore <- safety_care_measures1$SafetyCare_GroupScore
CMS_rating$Readmission_GroupScore <- Readmission_measures1$Readmission_GroupScore
CMS_rating$PatientExperience_GroupScore <- patient_exp_measure1$PatientExperience_GroupScore
CMS_rating$Timeliness_GroupScore <- timeliness_measures1$Timeliness_GroupScore
CMS_rating$EffectiveCare_measures_GroupScore   <- eff_care_measures1$EffectiveCare_measures_GroupScore
CMS_rating$EffectiveImaging_measures_GroupScore   <- eff_imaging_measure1$EffectiveImaging_measures_GroupScore

CMS_rating

#Calculating Final Score by Weighted Average
#Based on various stakeholder surveys the weightage for each Measure Group is as below

#Mortality	22%
#Safety Of Care	22%
#Readmission	22%
#Patient Experience	22%
#Timeliness of care 4%
#Effectiveness of Care	4%
#Efficient Use of Medical Imaging	4%

#Calculating the final score as per the below weightage

CMS_rating <- CMS_rating %>% mutate( WeightedScore = 0.22*as.numeric(CMS_rating$Mortality_GroupScore)
                            + 0.22*as.numeric(CMS_rating$SafetyCare_GroupScore) 
                            + 0.22*as.numeric(CMS_rating$Readmission_GroupScore)
                            + 0.22*as.numeric(CMS_rating$PatientExperience_GroupScore)
                            + 0.04*as.numeric(CMS_rating$Timeliness_GroupScore)
                            + 0.04*as.numeric(CMS_rating$EffectiveCare_measures_GroupScore)
                            + 0.04*as.numeric(CMS_rating$EffectiveImaging_measures_GroupScore))



str(CMS_rating)

#Plot the Weighted score againts the Hospital ratings
plot(CMS_rating$Hospital.overall.rating,CMS_rating$WeightedScore,color=CMS_rating$Hospital.overall.rating)
hist(CMS_rating$WeightedScore)

#CLUSTERING on WEIGHTED SCORE FOR EACH HOSPITAL

#Remove the Provider ID - Collect the group Measures and Weighted average in a dataset

Summary_scores <- CMS_rating[,-1]
Summary_scores1 <- Summary_scores[,c("Mortality_GroupScore","SafetyCare_GroupScore","Readmission_GroupScore","PatientExperience_GroupScore","Timeliness_GroupScore","EffectiveCare_measures_GroupScore","EffectiveImaging_measures_GroupScore","WeightedScore")]
Summary_scores1[,1:8] <- scale(Summary_scores1[,1:8])
CMS_rating_cluster <- kmeans(Summary_scores1, centers = 6, iter.max = 50, nstart = 50)
CMS_rating_cluster$cluster

hist(CMS_rating_cluster$cluster)

## Finding the optimal value of K

r_sq<- rnorm(20)

for (number in 1:20){clus <- kmeans(Summary_scores1, centers = number, nstart = 50)
r_sq[number]<- clus$betweenss/clus$totss
}
plot(r_sq)

Summary_scores1$Provider.ID <- CMS_rating$Provider.ID
Summary_scores1$OriginalRating <- CMS_rating$Hospital.overall.rating
Summary_scores1$CalculatedRating <- CMS_rating_cluster$cluster
table(Summary_scores1$OriginalRating, Summary_scores1$CalculatedRating)





