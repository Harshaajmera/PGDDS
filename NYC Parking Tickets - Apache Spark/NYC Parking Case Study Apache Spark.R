
#install.packages("SparkR")

#load library
library(SparkR)
library(ggplot2)


#initialise sparkR session
sparkR.session(master = "local")

## Load data files and create data frames for each year parking violation

parking_violation_2015 <- read.df("s3://datascibkt/2015.csv",source = "csv", inferSchema = "true", header = "true")

parking_violation_2016 <- read.df("s3://datascibkt/2016.csv",source = "csv", inferSchema = "true", header = "true")

parking_violation_2017 <- read.df("s3://datascibkt/2017.csv",source = "csv", inferSchema = "true", header = "true")


#find total number of records in each data set
nrow(parking_violation_2015) 
#11809233

nrow(parking_violation_2016) 
#10626899

nrow(parking_violation_2017)  
#10803028




str(parking_violation_2015)
str(parking_violation_2016)
str(parking_violation_2017)




parking_violation_2015 <- withColumnRenamed(parking_violation_2015, 'House Number', 'House_Number')

parking_violation_2015 <- withColumnRenamed(parking_violation_2015, 'Street Name', 'Street_Name')

parking_violation_2016 <- withColumnRenamed(parking_violation_2016, 'House Number', 'House_Number')

parking_violation_2016 <- withColumnRenamed(parking_violation_2016, 'Street Name', 'Street_Name')

parking_violation_2017 <- withColumnRenamed(parking_violation_2017, 'House Number', 'House_Number')

parking_violation_2017 <- withColumnRenamed(parking_violation_2017, 'Street Name', 'Street_Name')



#creating temp view
createOrReplaceTempView(parking_violation_2015, "parking_violation_2015_view")

createOrReplaceTempView(parking_violation_2016, "parking_violation_2016_view")

createOrReplaceTempView(parking_violation_2017, "parking_violation_2017_view")



## Find total number of tickets for each year.
ticketcount_2015 <- SparkR::sql("select count(*) from parking_violation_2015_view")

head(ticketcount_2015)

## Tickets logged 11809233 in 2015



ticketcount_2016 <- SparkR::sql("select count(*) from parking_violation_2016_view")

head(ticketcount_2016)

## Tickets logged 10626899 in 2016


ticketcount_2017 <- SparkR::sql("select count(*) from parking_violation_2017_view")

head(ticketcount_2017)

## Tickets logged 10803028 in 2017













## Find out how many unique states the cars which got parking tickets came from.

uniquestates_2015 <- SparkR::sql("select distinct `Registration State` from parking_violation_2015_view")

nrow(uniquestates_2015)

## 69 unique states in 2015

uniquestates_2016 <- SparkR::sql("select distinct `Registration State` from parking_violation_2016_view")

nrow(uniquestates_2016)

## 68 unique states in 2016

uniquestates_2017 <- SparkR::sql("select distinct `Registration State` from parking_violation_2017_view")

head(uniquestates_2017)

nrow(uniquestates_2017)

## 67 unique states in 2017
















## Some parking tickets don’t have addresses on them, which is cause for concern. Find out how many such tickets there are.

numtickets_noadd_2015 <- SparkR::sql("select * from parking_violation_2015_view where House_Number is null or Street_Name is null")

nrow(numtickets_noadd_2015)

## 1992401 tickets with either House number not available or Street Name not available  

numtickets_noadd_2016 <- SparkR::sql("select * from parking_violation_2016_view where House_Number is null or Street_Name is null")

nrow(numtickets_noadd_2016)

## 2035232 tickets with either House number not available or Street Name not available  

numtickets_noadd_2017 <- SparkR::sql("select * from parking_violation_2017_view where House_Number is null or Street_Name is null")

nrow(numtickets_noadd_2017)

## 2289944 tickets with either House number not available or Street Name not available 

















## Aggregation Analysis

## How often does each violation code occur? (frequency of violation codes - find the top 5)

freq_viol_2015 <- SparkR::sql("select count(*) as freq, `Violation Code` as Violation_code from parking_violation_2015_view group by `Violation Code` order by freq desc")

head(freq_viol_2015)

## Top 5 are Code 21, 38, 14, 36, 37

freq_viol_2016 <- SparkR::sql("select count(*) as freq, `Violation Code` as Violation_code from parking_violation_2016_view group by `Violation Code`order by freq desc") 

head(freq_viol_2016)

##  Top 5 are Code 21, 38, 14, 36, 37

freq_viol_2017 <- SparkR::sql("select count(*) as freq, `Violation Code` as Violation_code from parking_violation_2017_view group by `Violation Code` order by freq desc")

head(freq_viol_2017)

## Top 5 are Code 21, 38, 14, 36, 20

## How often does each vehicle body type get a parking ticket? How about the vehicle make?

freq_park_ticket_2015_type <- SparkR::sql("select count(*) as freq, `Vehicle Body Type` from parking_violation_2015_view group by `Vehicle Body Type` order by freq desc")

freq_park_ticket_2015_make <- SparkR::sql("select count(*) as freq, `Vehicle Make` from parking_violation_2015_view group by `Vehicle Make` order by freq desc")

head(freq_park_ticket_2015_type)

## SUBN, 4DSD, VAN, DELV, SDN

head(freq_park_ticket_2015_make)

## FORD, TOYOT, HONDA, NISSA, CHEVR

freq_park_ticket_2016_type <- SparkR::sql("select count(*) as freq, `Vehicle Body Type` from parking_violation_2016_view group by `Vehicle Body Type` order by freq desc")

freq_park_ticket_2016_make <- SparkR::sql("select count(*) as freq, `Vehicle Make` from parking_violation_2016_view group by `Vehicle Make` order by freq desc")

head(freq_park_ticket_2016_type)

## SUBN, 4DSD, VAN, DELV, SDN

head(freq_park_ticket_2016_make)

## FORD, TOYOT, HONDA, NISSA, CHEVR

freq_park_ticket_2017_type <- SparkR::sql("select count(*) as freq, `Vehicle Body Type` from parking_violation_2017_view group by `Vehicle Body Type` order by freq desc")

freq_park_ticket_2017_make <- SparkR::sql("select count(*) as freq, `Vehicle Make` from parking_violation_2017_view group by `Vehicle Make` order by freq desc")

head(freq_park_ticket_2017_type)

## SUBN, 4DSD, VAN, DELV, SDN

head(freq_park_ticket_2017_make)

## FORD, TOYOT, HONDA, NISSA, CHEVR

## Find the (5 highest) frequencies of: Violating Precincts, Issuing Precincts

voil_prec_2015 <- SparkR::sql("select count(*) as freq_vio_prec, `Violation Precinct` from parking_violation_2015_view group by `Violation Precinct` order by freq_vio_prec desc")

issu_prec_2015 <- SparkR::sql("select count(*) as freq_issu_prec, `Issuer Precinct` from parking_violation_2015_view group by `Issuer Precinct` order by freq_issu_prec desc")

head(voil_prec_2015)

## precinct 0, 19, 18, 14, 1

head(issu_prec_2015)

## precinct 0, 19, 14, 1, 18

voil_prec_2016 <- SparkR::sql("select count(*) as freq_vio_prec, `Violation Precinct` from parking_violation_2016_view group by `Violation Precinct` order by freq_vio_prec desc")

issu_prec_2016 <- SparkR::sql("select count(*) as freq_issu_prec, `Issuer Precinct` from parking_violation_2016_view group by `Issuer Precinct` order by freq_issu_prec desc")

head(voil_prec_2016)

## precinct 0, 19, 18, 14, 1

head(issu_prec_2016)

## precinct 0, 19, 14, 1, 18

voil_prec_2017 <- SparkR::sql("select count(*) as freq_vio_prec, `Violation Precinct` from parking_violation_2017_view group by `Violation Precinct` order by freq_vio_prec desc")

issu_prec_2017 <- SparkR::sql("select count(*) as freq_issu_prec, `Issuer Precinct` from parking_violation_2017_view group by `Issuer Precinct` order by freq_issu_prec desc")

head(voil_prec_2017)

## precinct 0, 19, 18, 14, 1

head(issu_prec_2017)

## precinct 0, 19, 14, 1, 18

## Find the violation code frequency across 3 precincts which have issued the most number of tickets - do these precinct zones have an exceptionally high frequency of certain violation codes? Are these codes common across precincts?

voil_code_issue_2015 <- SparkR::sql("select count(*) as freq,`Violation Code`, `Issuer Precinct` from parking_violation_2015_view group by `Violation Code`, `Issuer Precinct` order by freq desc")

voil_code_prec_2015 <- filter(voil_code_issue_2015, voil_code_issue_2015$`Issuer Precinct` == 0 | voil_code_issue_2015$`Issuer Precinct` == 18 | voil_code_issue_2015$`Issuer Precinct` == 19) 

head(voil_code_prec_2015, 20)

## Voilation code 21 is common in issuer precint 18 and 19

voil_code_issue_2016 <- SparkR::sql("select count(*) as freq,`Violation Code`, `Issuer Precinct` from parking_violation_2016_view group by `Violation Code`, `Issuer Precinct` order by freq desc")

voil_code_prec_2016 <- filter(voil_code_issue_2016, voil_code_issue_2016$`Issuer Precinct` == 0 | voil_code_issue_2016$`Issuer Precinct` == 14 | voil_code_issue_2016$`Issuer Precinct` == 19) 

head(voil_code_prec_2016, 20)

## Voilation code 21 is common in issuer precint 0 and 19

voil_code_issue_2015 <- SparkR::sql("select count(*) as freq,`Violation Code`, `Issuer Precinct` from parking_violation_2017_view group by `Violation Code`, `Issuer Precinct` order by freq desc")

voil_code_prec_2015 <- filter(voil_code_issue_2015, voil_code_issue_2015$`Issuer Precinct` == 0 | voil_code_issue_2015$`Issuer Precinct` == 14 | voil_code_issue_2015$`Issuer Precinct` == 19) 

head(voil_code_prec_2015, 20)

## Voilation code 38 is common in issuer precint 0 and 19

















#5.	You’d want to find out the properties of parking violations across different times of the day:
#  (1)	The Violation Time field is specified in a strange format. 
#Find a way to make this into a time attribute that you can use to divide into groups.



#####################for 2015 data set####################
head(select(parking_violation_2015, parking_violation_2015$`Violation Time`))
#Violation Time
#1          0011A
#2          0942A
#3          1020A
#4          0318P
#5          0410P
#6          0839A



#get hour and minutes
parking_violation_2015_x <- SparkR::sql("select *,substring(`Violation Time`, 1, 2) as hour,substring(`Violation Time`, 3, 2) as mint from parking_violation_2015_view")
head(select(parking_violation_2015_x, parking_violation_2015_x$`Violation Time`))


#adding 12 to hours which are in PM
parking_violation_2015_x$extraHour <- ifelse(endsWith(parking_violation_2015_x$`Violation Time`,"P"),12,0)
parking_violation_2015_x$newHour <- parking_violation_2015_x$extraHour + parking_violation_2015_x$hour
parking_violation_2015_x$newHour <- ifelse(parking_violation_2015_x$newHour == 24,parking_violation_2015_x$newHour - 12, parking_violation_2015_x$newHour)

#creating new column sep
parking_violation_2015_new = withColumn(parking_violation_2015_x, "sep", lit(":"))
str(parking_violation_2015_new)


#casting newHour into string
parking_violation_2015_new$newHourChar <- cast(parking_violation_2015_new$newHour,'string')

#creating view
createOrReplaceTempView(parking_violation_2015_new, "parking_violation_2015_new_view")

#getting hour without decimal part
parking_violation_2015_new2 <- SparkR::sql("select *,substring(newHourChar, 1, length(newHourChar)-2)  as newHour2 from parking_violation_2015_new_view")



#concatenating hour , sep and minutes
parking_violation_2015_new2$violtime <- SparkR::concat(parking_violation_2015_new2$newHour2, parking_violation_2015_new2$sep, parking_violation_2015_new2$mint) 

head(select(parking_violation_2015_new2, parking_violation_2015_new2$violtime))

#violtime
#1     0:11
#2     9:42
#3    10:20
#4    15:18
#5    16:10
#6     8:39

#As seen from above results, the violation time has been separated into proper time format








#######################for 2016 dataset##############################


head(select(parking_violation_2016, parking_violation_2016$`Violation Time`))
#Violation Time
#1          1037A
#2          1206P
#3          0820A
#4          0918A
#5          0925A
#6          0948A
 


#get hour and minutes
parking_violation_2016_x <- SparkR::sql("select *,substring(`Violation Time`, 1, 2) as hour,substring(`Violation Time`, 3, 2) as mint from parking_violation_2016_view")
head(select(parking_violation_2016_x, parking_violation_2016_x$`Violation Time`))


#adding 12 to hours which are in PM
parking_violation_2016_x$extraHour <- ifelse(endsWith(parking_violation_2016_x$`Violation Time`,"P"),12,0)
parking_violation_2016_x$newHour <- parking_violation_2016_x$extraHour + parking_violation_2016_x$hour
parking_violation_2016_x$newHour <- ifelse(parking_violation_2016_x$newHour == 24,parking_violation_2016_x$newHour - 12, parking_violation_2016_x$newHour)

#creating new column sep
parking_violation_2016_new = withColumn(parking_violation_2016_x, "sep", lit(":"))
str(parking_violation_2016_new)


#casting newHour into string
parking_violation_2016_new$newHourChar <- cast(parking_violation_2016_new$newHour,'string')

#creating view
createOrReplaceTempView(parking_violation_2016_new, "parking_violation_2016_new_view")

#getting hour without decimal part
parking_violation_2016_new2 <- SparkR::sql("select *,substring(newHourChar, 1, length(newHourChar)-2)  as newHour2 from parking_violation_2016_new_view")



#concatenating hour , sep and minutes
parking_violation_2016_new2$violtime <- SparkR::concat(parking_violation_2016_new2$newHour2, parking_violation_2016_new2$sep, parking_violation_2016_new2$mint) 

head(select(parking_violation_2016_new2, parking_violation_2016_new2$violtime))

#violtime
#1    10:37
#2    12:06
#3     8:20
#4     9:18
#5     9:25
#6     9:48
#As seen from above results, the violation time has been separated into proper time format













##################for 2017 data set######################

head(select(parking_violation_2017, parking_violation_2017$`Violation Time`))
#Violation Time
#1          0143A
#2          0400P
#3          0233P
#4          1120A
#5          0555P
#6          0852P


#get hour and minutes
parking_violation_2017_x <- SparkR::sql("select *,substring(`Violation Time`, 1, 2) as hour,substring(`Violation Time`, 3, 2) as mint from parking_violation_2017_view")
head(select(parking_violation_2017_x, parking_violation_2017_x$`Violation Time`))


#adding 12 to hours which are in PM
parking_violation_2017_x$extraHour <- ifelse(endsWith(parking_violation_2017_x$`Violation Time`,"P"),12,0)
parking_violation_2017_x$newHour <- parking_violation_2017_x$extraHour + parking_violation_2017_x$hour
parking_violation_2017_x$newHour <- ifelse(parking_violation_2017_x$newHour == 24,parking_violation_2017_x$newHour - 12, parking_violation_2017_x$newHour)

#creating new column sep
parking_violation_2017_new = withColumn(parking_violation_2017_x, "sep", lit(":"))
str(parking_violation_2017_new)


#casting newHour into string
parking_violation_2017_new$newHourChar <- cast(parking_violation_2017_new$newHour,'string')

#creating view
createOrReplaceTempView(parking_violation_2017_new, "parking_violation_2017_new_view")

#getting hour without decimal part
parking_violation_2017_new2 <- SparkR::sql("select *,substring(newHourChar, 1, length(newHourChar)-2)  as newHour2 from parking_violation_2017_new_view")



#concatenating hour , sep and minutes
parking_violation_2017_new2$violtime <- SparkR::concat(parking_violation_2017_new2$newHour2, parking_violation_2017_new2$sep, parking_violation_2017_new2$mint) 

head(select(parking_violation_2017_new2, parking_violation_2017_new2$violtime))

#violtime
#1     1:43
#2    16:00
#3    14:33
#4    11:20
#5    17:55
#6    20:52
#As seen from above results, the violation time has been separated into proper time format












#(2)	Find a way to deal with missing values, if any.

######################3for 2015 data set
createOrReplaceTempView(parking_violation_2015_new2, "parking_violation_2015_new2_view")

miss_viol_time_2015<- SparkR::sql("select * from parking_violation_2015_new2_view where `Violation Time` is  NULL")

nrow(miss_viol_time_2015)
#number  of missing entries in Violation Time column is 1715
#total number of records in 2015 data set is 11809233
#percentage of missing entries= (1715*100)/11809233 =0.0145 percent

#As the percentage of missing values in 'Violation Time' column is very low, ignoring those values
# and creating a new dataframe with non null values of violation time

viol_timeNotNull_2015<- SparkR::sql("select * from parking_violation_2015_new2_view where `Violation Time` is   not NULL")

















##########################for 2016 data set#######################
createOrReplaceTempView(parking_violation_2016_new2, "parking_violation_2016_new2_view")

miss_viol_time_2016<- SparkR::sql("select * from parking_violation_2016_new2_view where `Violation Time` is  NULL")

nrow(miss_viol_time_2016)
#number  of missing entries in Violation Time column is 4280
#total number of records in 2016 data set is 10626899
#percentage of missing entries= (4280*100)/10626899 =0.040 percent

#As the percentage of missing values in 'Violation Time' column is very low, ignoring those values
# and creating a new dataframe with non null values of violation time

viol_timeNotNull_2016<- SparkR::sql("select * from parking_violation_2016_new2_view where `Violation Time` is   not NULL")











###########################for 2017 dataset#############################3
createOrReplaceTempView(parking_violation_2017_new2, "parking_violation_2017_new2_view")

miss_viol_time_2017<- SparkR::sql("select * from parking_violation_2017_new2_view where `Violation Time` is  NULL")

nrow(miss_viol_time_2017)
#number  of missing entries in Violation Time column is 63
#total number of records in 2017 data set is 10803028
#percentage of missing entries= (63*100)/10803028 =0.0005 percent

#As the percentage of missing values in 'Violation Time' column is very low, ignoring those values
# and creating a new dataframe with non null values of violation time

viol_timeNotNull_2017<- SparkR::sql("select * from parking_violation_2017_new2_view where `Violation Time` is   not NULL")






#Divide 24 hours into 6 equal discrete bins of time. 
#The intervals you choose are at your discretion. 
#For each of these groups, find the 3 most commonly occurring violations

#creating 6 bins as
#0-4 hrs -latenight
#4-8 hrs - earlyMor
#8-12 hrs -morning
#12-16 hrs -afternoon
#16-20 hrs -evening
#20-00 hrs -lateEve











##################2015#########################
viol_timeNotNull_2015$timeslot <- ifelse( viol_timeNotNull_2015$newHour >=0 & viol_timeNotNull_2015$newHour < 4,"latenight",ifelse(viol_timeNotNull_2015$newHour >=4 & viol_timeNotNull_2015$newHour < 8,"earlyMor",ifelse(viol_timeNotNull_2015$newHour >=8 & viol_timeNotNull_2015$newHour < 12,"morning",ifelse(viol_timeNotNull_2015$newHour >=12 & viol_timeNotNull_2015$newHour < 16,"afternoon",ifelse( viol_timeNotNull_2015$newHour >=16 & viol_timeNotNull_2015$newHour < 20,"evening","lateEve")))))

str(viol_timeNotNull_2015)




#############################2016#########################
viol_timeNotNull_2016$timeslot <- ifelse( viol_timeNotNull_2016$newHour >=0 & viol_timeNotNull_2016$newHour < 4,"latenight",ifelse(viol_timeNotNull_2016$newHour >=4 & viol_timeNotNull_2016$newHour < 8,"earlyMor",ifelse(viol_timeNotNull_2016$newHour >=8 & viol_timeNotNull_2016$newHour < 12,"morning",ifelse(viol_timeNotNull_2016$newHour >=12 & viol_timeNotNull_2016$newHour < 16,"afternoon",ifelse( viol_timeNotNull_2016$newHour >=16 & viol_timeNotNull_2016$newHour < 20,"evening","lateEve")))))

str(viol_timeNotNull_2016)




#############################2017#########################
viol_timeNotNull_2017$timeslot <- ifelse( viol_timeNotNull_2017$newHour >=0 & viol_timeNotNull_2017$newHour < 4,"latenight",ifelse(viol_timeNotNull_2017$newHour >=4 & viol_timeNotNull_2017$newHour < 8,"earlyMor",ifelse(viol_timeNotNull_2017$newHour >=8 & viol_timeNotNull_2017$newHour < 12,"morning",ifelse(viol_timeNotNull_2017$newHour >=12 & viol_timeNotNull_2017$newHour < 16,"afternoon",ifelse( viol_timeNotNull_2017$newHour >=16 & viol_timeNotNull_2017$newHour < 20,"evening","lateEve")))))

str(viol_timeNotNull_2017)







######finding top 3 violation for each timeslot#####################

#######################2015##############################
createOrReplaceTempView(viol_timeNotNull_2015, "viol_timeNotNull_2015_view")

viol_timeNotNull_2015_latenight<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='latenight' group by timeslot, `Violation Code` order by count_ desc" )

viol_timeNotNull_2015_earlyMor<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='earlyMor' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2015_morning<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='morning' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2015_afternoon<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='afternoon' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2015_evening<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='evening' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2015_lateEve<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_view where timeslot='lateEve' group by timeslot,`Violation Code` order by count_ desc" )




collect(select(viol_timeNotNull_2015_latenight,viol_timeNotNull_2015_latenight$timeslot, viol_timeNotNull_2015_latenight$violationCode, viol_timeNotNull_2015_latenight$count_))

#timeslot       violationCode	 count_
#1  latenight            21 	 69557
#2  latenight            40 	 40335
#3  latenight            78 	 37575


collect(select(viol_timeNotNull_2015_earlyMor,viol_timeNotNull_2015_earlyMor$timeslot, viol_timeNotNull_2015_earlyMor$violationCode, viol_timeNotNull_2015_earlyMor$count_))
#timeslot       violationCode	 count_
#1  earlyMor            14 	143264
#2  earlyMor            21	 118316
#3  earlyMor            40	  98135


collect(select(viol_timeNotNull_2015_morning,viol_timeNotNull_2015_morning$timeslot, viol_timeNotNull_2015_morning$violationCode, viol_timeNotNull_2015_morning$count_))
#timeslot       violationCode	 count_
#1    morning            21	 	1291540
#2    morning            38 	 480358
#3    morning            36 	 396838

collect(select(viol_timeNotNull_2015_afternoon,viol_timeNotNull_2015_afternoon$timeslot, viol_timeNotNull_2015_afternoon$violationCode, viol_timeNotNull_2015_afternoon$count_))
#timeslot       violationCode 	count_
#1   afternoon            38	 609616
#2   afternoon            37 	446482
#3   afternoon            36 	357310

collect(select(viol_timeNotNull_2015_evening,viol_timeNotNull_2015_evening$timeslot, viol_timeNotNull_2015_evening$violationCode, viol_timeNotNull_2015_evening$count_))
#timeslot       violationCode 	count_
#1   evening            38 		258838
#2   evening            37		 187186
#3   evening             7 		182347

collect(select(viol_timeNotNull_2015_lateEve,viol_timeNotNull_2015_lateEve$timeslot, viol_timeNotNull_2015_lateEve$violationCode, viol_timeNotNull_2015_lateEve$count_))

#timeslot       violationCode	 count_
#1   lateEve             7 		 89813
#2   lateEve            38 		 66023
#3   lateEve            40 		 49947









#######################2016##############################
createOrReplaceTempView(viol_timeNotNull_2016, "viol_timeNotNull_2016_view")

viol_timeNotNull_2016_latenight<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='latenight' group by timeslot, `Violation Code` order by count_ desc" )

viol_timeNotNull_2016_earlyMor<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='earlyMor' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2016_morning<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='morning' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2016_afternoon<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='afternoon' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2016_evening<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='evening' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2016_lateEve<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_view where timeslot='lateEve' group by timeslot,`Violation Code` order by count_ desc" )




collect(select(viol_timeNotNull_2016_latenight,viol_timeNotNull_2016_latenight$timeslot, viol_timeNotNull_2016_latenight$violationCode, viol_timeNotNull_2016_latenight$count_))

#timeslot       violationCode 	count_
#1  latenight            21 	 67798
#2  latenight            40 	 37261
#3  latenight            78 	 29473


collect(select(viol_timeNotNull_2016_earlyMor,viol_timeNotNull_2016_earlyMor$timeslot, viol_timeNotNull_2016_earlyMor$violationCode, viol_timeNotNull_2016_earlyMor$count_))
#timeslot       violationCode 	count_
#1  earlyMor            14 		140111
#2  earlyMor            21		 114029
#3  earlyMor            40		  91693

collect(select(viol_timeNotNull_2016_morning,viol_timeNotNull_2016_morning$timeslot, viol_timeNotNull_2016_morning$violationCode, viol_timeNotNull_2016_morning$count_))
#timeslot       violationCode		 count_
#1    morning            21 		1209244
#2    morning            36 		 586791
#3    morning            38 		 388099

collect(select(viol_timeNotNull_2016_afternoon,viol_timeNotNull_2016_afternoon$timeslot, viol_timeNotNull_2016_afternoon$violationCode, viol_timeNotNull_2016_afternoon$count_))
#timeslot       violationCode 	count_
#1  afternoon            36 545717
#2  afternoon            38 488363
#3  afternoon            37 383379

collect(select(viol_timeNotNull_2016_evening,viol_timeNotNull_2016_evening$timeslot, viol_timeNotNull_2016_evening$violationCode, viol_timeNotNull_2016_evening$count_))
#timeslot       violationCode 	count_
#1   evening            38 211267
#2   evening            37 161655
#3   evening            14 134976

collect(select(viol_timeNotNull_2016_lateEve,viol_timeNotNull_2016_lateEve$timeslot, viol_timeNotNull_2016_lateEve$violationCode, viol_timeNotNull_2016_lateEve$count_))
#timeslot       violationCode 	count_
#1   lateEve             7  60924
#2   lateEve            38  53174
#3   lateEve            40  44992
















#######################2017##############################
createOrReplaceTempView(viol_timeNotNull_2017, "viol_timeNotNull_2017_view")


viol_timeNotNull_2017_latenight<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='latenight' group by timeslot, `Violation Code` order by count_ desc" )

viol_timeNotNull_2017_earlyMor<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='earlyMor' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2017_morning<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='morning' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2017_afternoon<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='afternoon' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2017_evening<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='evening' group by timeslot,`Violation Code` order by count_ desc" )

viol_timeNotNull_2017_lateEve<- SparkR::sql("select timeslot,`Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_view where timeslot='lateEve' group by timeslot,`Violation Code` order by count_ desc" )




collect(select(viol_timeNotNull_2017_latenight,viol_timeNotNull_2017_latenight$timeslot, viol_timeNotNull_2017_latenight$violationCode, viol_timeNotNull_2017_latenight$count_))
#timeslot       violationCode count_
#1  latenight            21  73160
#2  latenight            40  45960
#3  latenight            14  29313



collect(select(viol_timeNotNull_2017_earlyMor,viol_timeNotNull_2017_earlyMor$timeslot, viol_timeNotNull_2017_earlyMor$violationCode, viol_timeNotNull_2017_earlyMor$count_))
#timeslot       violationCode	 count_
#1 earlyMor            14 		141276
#2  earlyMor            21 		119469
#3  earlyMor            40 		112187

collect(select(viol_timeNotNull_2017_morning,viol_timeNotNull_2017_morning$timeslot, viol_timeNotNull_2017_morning$violationCode, viol_timeNotNull_2017_morning$count_))
#timeslot       violationCode count_
#1   morning            21 1182691
#2   morning            36  751422
#3   morning            38  346518


collect(select(viol_timeNotNull_2017_afternoon,viol_timeNotNull_2017_afternoon$timeslot, viol_timeNotNull_2017_afternoon$violationCode, viol_timeNotNull_2017_afternoon$count_))
#timeslot       violationCode count_
#1  afternoon            36 588395
#2  afternoon            38 462859
#3  afternoon            37 337096


collect(select(viol_timeNotNull_2017_evening,viol_timeNotNull_2017_evening$timeslot, viol_timeNotNull_2017_evening$violationCode, viol_timeNotNull_2017_evening$count_))
#timeslot       violationCode count_
#1   evening            38 203232
#2   evening            37 145784
#3   evening            14 144749



collect(select(viol_timeNotNull_2017_lateEve,viol_timeNotNull_2017_lateEve$timeslot, viol_timeNotNull_2017_lateEve$violationCode, viol_timeNotNull_2017_lateEve$count_))
#timeslot       violationCode count_
#1   lateEve             7  65593
#2   lateEve            38  47032
#3   lateEve            14  44786



















#Now, try another direction. For the 3 most commonly occurring violation codes, 
#find the most common times of day (in terms of the bins from the previous part)


#############################2015############################
createOrReplaceTempView(viol_timeNotNull_2015, "viol_timeNotNull_2015_view")

viol_timeNotNull_2015_2 <- SparkR::sql("select `Violation Code` as violationCode, count(*) as count_ from viol_timeNotNull_2015_view  group by `Violation Code` order by count_ desc" )

collect(select(viol_timeNotNull_2015_2, viol_timeNotNull_2015_2$violationCode, viol_timeNotNull_2015_2$count_))
#top 3 violation codes for 2015
#violationCode  		count_
#1              21		 1630912
#2              38 		 1418627
#3              14 		  988463



viol_timeNotNull_2015_3<- SparkR::sql("select `Violation Code` as violationCode,  timeslot,count(*) as count_ from viol_timeNotNull_2015_view where `Violation Code` in (21,38,14) group by `Violation Code`, timeslot order by  `Violation Code`, count_ desc" )

collect(select(viol_timeNotNull_2015_3, viol_timeNotNull_2015_3$violationCode,viol_timeNotNull_2015_3$timeslot, viol_timeNotNull_2015_3$count_))

#violationCode  timeslot  count_
#1             14   morning  317009
#2             14 afternoon  289367
#3             14   evening  160432
#4             14  earlyMor  143264
#5             14   lateEve   49524
#6             14 latenight   28867
#7             21   morning 1291540
#8             21 afternoon  149872
#9             21  earlyMor  118316
#10            21 latenight   69557
#11            21   evening     905
#12            21   lateEve     722
#13            38 afternoon  609616
#14            38   morning  480358
#15            38   evening  258838
#16            38   lateEve   66023
#17            38  earlyMor    3050
#18            38 latenight     742




#############################2016############################
createOrReplaceTempView(viol_timeNotNull_2016, "viol_timeNotNull_2016_view")

viol_timeNotNull_2016_2 <- SparkR::sql("select `Violation Code` as violationCode, count(*) as count_ from viol_timeNotNull_2016_view  group by `Violation Code` order by count_ desc" )

collect(select(viol_timeNotNull_2016_2, viol_timeNotNull_2016_2$violationCode, viol_timeNotNull_2016_2$count_))
#top 3 violation codes for 2016
#    violationCode  		count_
#1              21 		1530787
#2              36 		1253511
#3              38 		1143438



viol_timeNotNull_2016_3<- SparkR::sql("select `Violation Code` as violationCode,  timeslot,count(*) as count_ from viol_timeNotNull_2016_view where `Violation Code` in (21,36,38) group by `Violation Code`, timeslot order by  `Violation Code`,  count_ desc" )


collect(select(viol_timeNotNull_2016_3, viol_timeNotNull_2016_3$violationCode,viol_timeNotNull_2016_3$timeslot, viol_timeNotNull_2016_3$count_))
                                                 
#violationCode  timeslot  count_
#1             21   morning 1209244
#2             21 afternoon  138641
#3             21  earlyMor  114029
#4             21 latenight   67798
#5             21   evening     601
#6             21   lateEve     474
#7             36   morning  586791
#8             36 afternoon  545717
#9             36  earlyMor   79797
#10            36   evening   41205
#11            36 latenight       1
#12            38 afternoon  488363
#13            38   morning  388099
#14            38   evening  211267
#15            38   lateEve   53174
#16            38  earlyMor    2211
#17            38 latenight     324








#############################2017############################
createOrReplaceTempView(viol_timeNotNull_2017, "viol_timeNotNull_2017_view")

viol_timeNotNull_2017_2 <- SparkR::sql("select `Violation Code` as violationCode, count(*) as count_ from viol_timeNotNull_2017_view  group by `Violation Code` order by count_ desc" )

collect(select(viol_timeNotNull_2017_2, viol_timeNotNull_2017_2$violationCode, viol_timeNotNull_2017_2$count_))
#top 3 violation codes for 2017
#violationCode  		count_
#1              21 1528588
#2              36 1400614
#3              38 1062304



viol_timeNotNull_2017_3<- SparkR::sql("select `Violation Code` as violationCode,  timeslot,count(*) as count_ from viol_timeNotNull_2017_view where `Violation Code` in (21,36,38) group by `Violation Code`, timeslot order by  `Violation Code`,  count_ desc" )


collect(select(viol_timeNotNull_2017_3, viol_timeNotNull_2017_3$violationCode,viol_timeNotNull_2017_3$timeslot, viol_timeNotNull_2017_3$count_))

#violationCode  timeslot  count_
#1             21   morning 1182691
#2             21 afternoon  152314
#3             21  earlyMor  119469
#4             21 latenight   73160
#5             21   evening     551
#6             21   lateEve     403
#7             36   morning  751422
#8             36 afternoon  588395
#9             36  earlyMor   33939
#10            36   evening   26858
#11            38 afternoon  462859
#12            38   morning  346518
#13            38   evening  203232
#14            38   lateEve   47032
#15            38  earlyMor    2300
#16            38 latenight     363


































#6.	Let’s try and find some seasonality in this data
#	First, divide the year into some number of seasons, and find frequencies of tickets for each season.


########################2015###############################
createOrReplaceTempView(viol_timeNotNull_2015, "viol_timeNotNull_2015_view")

#extract month from Issue Date  and store it in month column

createOrReplaceTempView(viol_timeNotNull_2015, "viol_timeNotNull_2015_view")

viol_timeNotNull_2015_4 <- SparkR::sql("select *, substring(`Issue Date`, 1, 2) as month  from viol_timeNotNull_2015_view" )
str(viol_timeNotNull_2015_4)

#set season as follows:
#month =04 or 05 or 06 or 07, or 08 or 09- season1
#otherwise - season2

viol_timeNotNull_2015_4$season <- ifelse(viol_timeNotNull_2015_4$month=="04" | viol_timeNotNull_2015_4$month=="05" | viol_timeNotNull_2015_4$month=="06" | viol_timeNotNull_2015_4$month=="07" | viol_timeNotNull_2015_4$month=="08" | viol_timeNotNull_2015_4$month=="09","season1","season2")


createOrReplaceTempView(viol_timeNotNull_2015_4, "viol_timeNotNull_2015_4_view")


viol_timeNotNull_2015_4_season <- SparkR::sql("select season, count(*) as count_ from viol_timeNotNull_2015_4_view  group by season order by count_ desc" )

collect(select(viol_timeNotNull_2015_4_season, viol_timeNotNull_2015_4_season$season, viol_timeNotNull_2015_4_season$count_))
#count of ticket for each season
#season   		count_
#1 season1 		6178679
#2 season2 		5628839










########################2016###############################
createOrReplaceTempView(viol_timeNotNull_2016, "viol_timeNotNull_2016_view")

#extract month from Issue Date  and store it in month column

createOrReplaceTempView(viol_timeNotNull_2016, "viol_timeNotNull_2016_view")

viol_timeNotNull_2016_4 <- SparkR::sql("select *, substring(`Issue Date`, 1, 2) as month  from viol_timeNotNull_2016_view" )
str(viol_timeNotNull_2016_4)

#set season as follows:
#month =04 or 05 or 06 or 07, or 08 or 09- season1
#otherwise - season2

viol_timeNotNull_2016_4$season <- ifelse(viol_timeNotNull_2016_4$month=="04" | viol_timeNotNull_2016_4$month=="05" | viol_timeNotNull_2016_4$month=="06" | viol_timeNotNull_2016_4$month=="07" | viol_timeNotNull_2016_4$month=="08" | viol_timeNotNull_2016_4$month=="09","season1","season2")


createOrReplaceTempView(viol_timeNotNull_2016_4, "viol_timeNotNull_2016_4_view")


viol_timeNotNull_2016_4_season <- SparkR::sql("select season, count(*) as count_ from viol_timeNotNull_2016_4_view  group by season order by count_ desc" )

collect(select(viol_timeNotNull_2016_4_season, viol_timeNotNull_2016_4_season$season, viol_timeNotNull_2016_4_season$count_))

#count of ticket for each season
#season   		count_
#1 season2 		5471979
#2 season1 		5150640


########################2017###############################

createOrReplaceTempView(viol_timeNotNull_2017, "viol_timeNotNull_2017_view")

#extract month from Issue Date  and store it in month column

createOrReplaceTempView(viol_timeNotNull_2017, "viol_timeNotNull_2017_view")

viol_timeNotNull_2017_4 <- SparkR::sql("select *, substring(`Issue Date`, 1, 2) as month  from viol_timeNotNull_2017_view" )
str(viol_timeNotNull_2017_4)

#set season as follows:
#month =04 or 05 or 06 or 07, or 08 or 09- season1
#otherwise - season2

viol_timeNotNull_2017_4$season <- ifelse(viol_timeNotNull_2017_4$month=="04" | viol_timeNotNull_2017_4$month=="05" | viol_timeNotNull_2017_4$month=="06" | viol_timeNotNull_2017_4$month=="07" | viol_timeNotNull_2017_4$month=="08" | viol_timeNotNull_2017_4$month=="09","season1","season2")


createOrReplaceTempView(viol_timeNotNull_2017_4, "viol_timeNotNull_2017_4_view")


viol_timeNotNull_2017_4_season <- SparkR::sql("select season, count(*) as count_ from viol_timeNotNull_2017_4_view  group by season order by count_ desc" )

collect(select(viol_timeNotNull_2017_4_season, viol_timeNotNull_2017_4_season$season, viol_timeNotNull_2017_4_season$count_))
#count of ticket for each season
#season   		count_
#1 season1 		5482742
#2 season2 		5320223

























#	Then, find the 3 most common violations for each of these season

###############################2015###################
createOrReplaceTempView(viol_timeNotNull_2015_4, "viol_timeNotNull_2015_4_view")


viol_timeNotNull_2015_4_season <- SparkR::sql("select season, `Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2015_4_view  group by season,`Violation Code` order by season,count_ desc" )

collect(select(viol_timeNotNull_2015_4_season, viol_timeNotNull_2015_4_season$season, viol_timeNotNull_2015_4_season$violationCode , viol_timeNotNull_2015_4_season$count_))


#season      violationCode count_
#1   season1            21 883664
#2   season1            38 699200
#3   season1            14 503344
#4   season1            36 443785
#5   season1             7 438182
#6   season1            37 408666
#7   season1            20 330757
#8   season1            46 309173
#9   season1            71 295099
#10  season1            40 263338
#11  season1            19 174354
#12  season1            69 155580
#13  season1             5 144484
#14  season1            70 133445
#15  season1            16 128425
#16  season1            31  87252
#17  season1            47  74135
#18  season1            74  60147
#19  season1            17  54794
#20  season1            42  51235
#21  season1            50  50373
#22  season1            78  41248
#23  season1            51  40479
#24  season1            48  40200
#25  season1            84  36603
#26  season1            98  25277
#27  season1            24  21398
#28  season1            67  20959
#29  season1            10  20342
#30  season1            82  20083
#31  season1            45  19677
#32  season1            13  18667
#33  season1            85  17342
#34  season1            53  17333
#35  season1            66  14686
#36  season1            18   9938
#37  season1            72   9788
#38  season1            68   9542
#39  season1            83   9268
#40  season1            64   9233
#41  season1            77   8866
#42  season1            61   8048
#43  season1             9   6984
#44  season1            27   6003
#45  season1            39   5993
#46  season1            80   4455
#47  season1            99   4357
#48  season1            62   4122
#49  season1            60   3937
#50  season1            23   3825
#51  season1            35   3733
#52  season1            75   3477
#53  season1            41   3395
#54  season1            73   3182
#55  season1            11   2189
#56  season1             8   2167
#57  season1            89   2043
#58  season1            79   1513
#59  season1            91   1166
#60  season1            63   1033
#61  season1             1    890
#62  season1            49    717
#63  season1            52    671
#64  season1             3    534
#65  season1             4    520
#66  season1            26    365
#67  season1            97    352
#68  season1            43    305
#69  season1            59    300
#70  season1            55    244
#71  season1            56    205
#72  season1            32    197
#73  season1            95    155
#74  season1            30    149
#75  season1             6    133
#76  season1            25    100
#77  season1            81     82
#78  season1            92     82
#79  season1            34     62
#80  season1            86     59
#81  season1            22     58
#82  season1            96     57
#83  season1            65     54
#84  season1             2     50
#85  season1            88     39
#86  season1            58     37
#87  season1            94     36
#88  season1            90     32
#89  season1            76     29
#90  season1            54     28
#91  season1            29     26
#92  season1            57     24
#93  season1            12     22
#94  season1            44     17
#95  season1            15     17
#96  season1            93     12
#97  season1            28     10
#98  season1            33      9
#99  season1            87      9
#100 season1             0      8
#101 season2            21 747248
#102 season2            38 719427
#103 season2            14 485119
#104 season2            36 395412
#105 season2            37 387252
#106 season2            20 332137
#107 season2            46 286536
#108 season2            71 281841
#109 season2             7 281571
#110 season2            40 271109
#111 season2            19 171467
#112 season2            69 129241
#113 season2            16 125947
#114 season2            70 118888
#115 season2            31  82599
#116 season2             5  80076
#117 season2            47  62521
#118 season2            50  56262
#119 season2            74  55045
#120 season2            17  53265
#121 season2            42  42211
#122 season2            51  39012
#123 season2            78  38035
#124 season2            48  37588
#125 season2            84  35387
#126 season2            98  28439
#127 season2            45  20063
#128 season2            10  19215
#129 season2            24  19210
#130 season2            67  18957
#131 season2            85  18191
#132 season2            53  16890
#133 season2            13  16581
#134 season2            82  15445
#135 season2            66  14930
#136 season2            77  10005
#137 season2            60   9472
#138 season2            18   9336
#139 season2            64   8429
#140 season2            68   8352
#141 season2            61   7390
#142 season2            83   6612
#143 season2            72   5763
#144 season2            39   5604
#145 season2            27   5402
#146 season2             1   4435
#147 season2            80   4082
#148 season2             9   3988
#149 season2             3   3947
#150 season2            23   3578
#151 season2            62   3440
#152 season2            99   3319
#153 season2            75   2996
#154 season2            41   2644
#155 season2            73   2628
#156 season2            11   1928
#157 season2             8   1660
#158 season2            35   1607
#159 season2            89   1542
#160 season2            91   1040
#161 season2            79   1028
#162 season2            49   1016
#163 season2            52    743
#164 season2             4    436
#165 season2            30    329
#166 season2            43    323
#167 season2            59    291
#168 season2            56    267
#169 season2            26    257
#170 season2            63    212
#171 season2            97    201
#172 season2            32    180
#173 season2            55    177
#174 season2            95    169
#175 season2            96    113
#176 season2             6    112
#177 season2             2     85
#178 season2            25     70
#179 season2            65     51
#180 season2            54     51
#181 season2            92     48
#182 season2            81     42
#183 season2            90     34
#184 season2            86     33
#185 season2            22     33
#186 season2            34     31
#187 season2            29     27
#188 season2            12     23
#189 season2            88     22
#190 season2            58     21
#191 season2            76     21
#192 season2            93     17
#193 season2            44     14
#194 season2            94     12
#195 season2            15     12
#196 season2            33      6
#197 season2            87      5
#198 season2            57      5
#199 season2            28      4
#200 season2             0      2



#here,3 most common violations for season 1 are: 
#season      violationCode count_
#1   season1            21 883664
#2   season1            38 699200
#3   season1            14 503344

#here,3 most common violations for season 2 are: 
#season      violationCode count_
#1 season2            21 747248
#1 season2            38 719427
#1 season2            14 485119







###############################2016###################
createOrReplaceTempView(viol_timeNotNull_2016_4, "viol_timeNotNull_2016_4_view")


viol_timeNotNull_2016_4_season <- SparkR::sql("select season, `Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2016_4_view  group by season,`Violation Code` order by season,count_ desc" )

collect(select(viol_timeNotNull_2016_4_season, viol_timeNotNull_2016_4_season$season, viol_timeNotNull_2016_4_season$violationCode , viol_timeNotNull_2016_4_season$count_))

#season violationCode count_
#1   season1            21 751394
#2   season1            38 560011
#3   season1            36 477759
#4   season1            14 437378
#5   season1            37 349046
#6   season1            20 294720
#7   season1            46 288582
#8   season1             7 259066
#9   season1            71 246919
#10  season1            40 222406
#11  season1            19 144536
#12  season1            69 122243
#13  season1            70 118541
#14  season1            16 102109
#15  season1            31  68167
#16  season1            47  60021
#17  season1            74  53374
#18  season1             5  50257
#19  season1            50  44313
#20  season1            42  43536
#21  season1            17  42134
#22  season1            48  37344
#23  season1            84  35619
#24  season1            51  32378
#25  season1            78  31082
#26  season1            24  26095
#27  season1            10  20168
#28  season1            98  19929
#29  season1            82  17578
#30  season1            67  15616
#31  season1            53  15477
#32  season1            85  14722
#33  season1            45  14710
#34  season1            66  13548
#35  season1            72  12172
#36  season1            13  12158
#37  season1            68   8867
#38  season1            77   7783
#39  season1            18   7457
#40  season1            61   6477
#41  season1            64   6451
#42  season1             9   5517
#43  season1            23   4978
#44  season1            83   4015
#45  season1            39   3857
#46  season1            27   3658
#47  season1            75   3577
#48  season1            35   3267
#49  season1            80   3230
#50  season1            62   3164
#51  season1            60   2958
#52  season1            73   2617
#53  season1            41   2384
#54  season1            99   2043
#55  season1            11   1846
#56  season1            89   1644
#57  season1             8   1554
#58  season1            63   1163
#59  season1            91   1074
#60  season1            79   1074
#61  season1             1    548
#62  season1            52    545
#63  season1            49    537
#64  season1            26    495
#65  season1             4    458
#66  season1             3    287
#67  season1            43    242
#68  season1            56    220
#69  season1            59    203
#70  season1            30    171
#71  season1            94    147
#72  season1            55    140
#73  season1             6    106
#74  season1            97     86
#75  season1            32     78
#76  season1            95     73
#77  season1            81     66
#78  season1            65     64
#79  season1            25     58
#80  season1            96     49
#81  season1            92     43
#82  season1            22     32
#83  season1             2     29
#84  season1            86     24
#85  season1            88     23
#86  season1            29     20
#87  season1            34     18
#88  season1            90     18
#89  season1            58     14
#90  season1            44     12
#91  season1            76     11
#92  season1            15     10
#93  season1            33     10
#94  season1            12     10
#95  season1            93      8
#96  season1            57      6
#97  season1            28      4
#98  season1            54      4
#99  season1            87      4
#100 season1             0      4
#101 season2            21 779393
#102 season2            36 775752
#103 season2            38 583427
#104 season2            14 437941
#105 season2            37 337461
#106 season2            20 316029
#107 season2            46 291572
#108 season2            71 241914
#109 season2            40 239863
#110 season2             7 233371
#111 season2            19 147310
#112 season2            70 109522
#113 season2            69 109085
#114 season2            16  94460
#115 season2            31  70923
#116 season2             5  62148
#117 season2            47  52753
#118 season2            74  52580
#119 season2            50  49497
#120 season2            17  42186
#121 season2            42  38562
#122 season2            48  33989
#123 season2            84  32326
#124 season2            51  31703
#125 season2            78  29499
#126 season2            24  26609
#127 season2            98  24567
#128 season2            67  19164
#129 season2            10  18128
#130 season2            53  16208
#131 season2            82  15610
#132 season2            45  14746
#133 season2            66  14523
#134 season2            85  13215
#135 season2            13  11713
#136 season2            72  10731
#137 season2            77   8668
#138 season2            18   7743
#139 season2             9   7602
#140 season2            68   7481
#141 season2            64   6542
#142 season2            61   6193
#143 season2            23   4987
#144 season2            83   4332
#145 season2            39   3821
#146 season2            60   3445
#147 season2            75   3220
#148 season2            27   3190
#149 season2            80   3121
#150 season2            62   2833
#151 season2            41   2358
#152 season2            73   2191
#153 season2            99   1741
#154 season2            11   1721
#155 season2            35   1453
#156 season2             8   1411
#157 season2            89   1397
#158 season2            91   1076
#159 season2            79   1068
#160 season2             1    638
#161 season2            52    625
#162 season2            49    574
#163 season2            56    479
#164 season2            26    443
#165 season2             3    389
#166 season2             4    366
#167 season2            63    286
#168 season2            43    258
#169 season2            94    243
#170 season2            30    237
#171 season2            55    236
#172 season2            59    179
#173 season2             6    113
#174 season2            25     84
#175 season2            95     82
#176 season2             2     77
#177 season2            65     62
#178 season2            88     62
#179 season2            32     60
#180 season2            97     59
#181 season2            81     58
#182 season2            96     47
#183 season2            90     31
#184 season2            22     28
#185 season2            92     26
#186 season2            76     26
#187 season2            58     19
#188 season2            15     19
#189 season2            12     14
#190 season2            34     14
#191 season2            86     13
#192 season2            33     13
#193 season2            93     11
#194 season2            44      9
#195 season2            54      7
#196 season2            29      5
#197 season2            57      5
#198 season2            87      3
#199 season2             0      3
#200 season2            28      2


#here,3 most common violations for season 1 are: 
#season      violationCode count_
#1   season1            21 751394
#2   season1            38 560011
#3   season1            36 477759

#here,3 most common violations for season 2 are: 
#season      violationCode count_
#1 season2            21 779393
#2 season2            36 775752
#3 season2            38 583427












###############################2017###################
createOrReplaceTempView(viol_timeNotNull_2017_4, "viol_timeNotNull_2017_4_view")


viol_timeNotNull_2017_4_season <- SparkR::sql("select season, `Violation Code` as violationCode,count(*) as count_ from viol_timeNotNull_2017_4_view  group by season,`Violation Code` order by season,count_ desc" )

collect(select(viol_timeNotNull_2017_4_season, viol_timeNotNull_2017_4_season$season, viol_timeNotNull_2017_4_season$violationCode , viol_timeNotNull_2017_4_season$count_))


#season violationCode count_
#1   season1            21 806958
#2   season1            36 609781
#3   season1            38 511894
#4   season1            14 465717
#5   season1            20 322624
#6   season1            46 309788
#7   season1             7 303573
#8   season1            37 297263
#9   season1            71 272059
#10  season1            40 261793
#11  season1            19 148566
#12  season1            70 146322
#13  season1            69 101807
#14  season1            16  81257
#15  season1             5  80384
#16  season1            31  76067
#17  season1            47  58901
#18  season1            74  58165
#19  season1            50  48923
#20  season1            48  41281
#21  season1            84  39577
#22  season1            17  38427
#23  season1            24  36138
#24  season1            42  35241
#25  season1            51  31462
#26  season1            78  28333
#27  season1            10  25224
#28  season1            98  23952
#29  season1            68  21763
#30  season1            82  19476
#31  season1             9  19048
#32  season1            53  18671
#33  season1            66  12363
#34  season1            13  11124
#35  season1            85  10305
#36  season1            18   9775
#37  season1            23   8512
#38  season1            45   7887
#39  season1            67   7592
#40  season1            64   6416
#41  season1            77   6211
#42  season1            61   5873
#43  season1            72   5483
#44  season1            79   5091
#45  season1            83   4583
#46  season1            75   4428
#47  season1            11   4355
#48  season1            27   3539
#49  season1            41   2888
#50  season1            62   2865
#51  season1            60   2846
#52  season1            35   2761
#53  season1            80   2231
#54  season1            73   1949
#55  season1            89   1899
#56  season1            99   1831
#57  season1             8   1429
#58  season1            39   1168
#59  season1            63    684
#60  season1            52    671
#61  season1            26    667
#62  season1            91    599
#63  season1             1    478
#64  season1            49    473
#65  season1             4    442
#66  season1            30    431
#67  season1             3    373
#68  season1            56    294
#69  season1            81    186
#70  season1            94    180
#71  season1            43    177
#72  season1             0    142
#73  season1             6    133
#74  season1            59    122
#75  season1            55    102
#76  season1            25     94
#77  season1             2     89
#78  season1            95     89
#79  season1            22     63
#80  season1            65     61
#81  season1            97     51
#82  season1            96     47
#83  season1            58     42
#84  season1            29     31
#85  season1            90     27
#86  season1            33     21
#87  season1            92     21
#88  season1            76     17
#89  season1            88     15
#90  season1            12     15
#91  season1            15     12
#92  season1            86     10
#93  season1            34      9
#94  season1            93      9
#95  season1            32      8
#96  season1            44      6
#97  season1            28      4
#98  season1            57      4
#99  season1            54      3
#100 season1            87      1
#101 season2            36 790833
#102 season2            21 721630
#103 season2            38 550410
#104 season2            14 427780
#105 season2            37 299506
#106 season2            20 295969
#107 season2            46 290224
#108 season2            40 257820
#109 season2            71 249249
#110 season2             7 212822
#111 season2            19 142354
#112 season2            70 127565
#113 season2            69  81702
#114 season2            16  79191
#115 season2            31  70613
#116 season2             5  65271
#117 season2            47  55026
#118 season2            74  54504
#119 season2            50  51192
#120 season2            48  42269
#121 season2             9  37585
#122 season2            17  36489
#123 season2            24  34030
#124 season2            84  32031
#125 season2            51  29927
#126 season2            42  29677
#127 season2            78  28411
#128 season2            98  27059
#129 season2            10  21535
#130 season2            68  21181
#131 season2            53  16652
#132 season2            82  13928
#133 season2            66  13533
#134 season2            13   9985
#135 season2            85   9197
#136 season2            18   8587
#137 season2            45   8373
#138 season2            67   8167
#139 season2            23   7391
#140 season2            77   5667
#141 season2            64   5148
#142 season2            61   5076
#143 season2            72   4347
#144 season2            60   3878
#145 season2            75   3833
#146 season2            83   3418
#147 season2            27   3328
#148 season2            62   2847
#149 season2            11   2772
#150 season2            41   2688
#151 season2            80   2150
#152 season2            89   1605
#153 season2            39   1602
#154 season2            73   1553
#155 season2            99   1485
#156 season2            79   1109
#157 season2             8    880
#158 season2            52    764
#159 season2            35    734
#160 season2             4    623
#161 season2            26    581
#162 season2             1    580
#163 season2            63    427
#164 season2            56    419
#165 season2            91    404
#166 season2            49    403
#167 season2             3    301
#168 season2            30    292
#169 season2            94    209
#170 season2            43    193
#171 season2             6    185
#172 season2            55    121
#173 season2            59    111
#174 season2             0     97
#175 season2            95     93
#176 season2            81     81
#177 season2            25     75
#178 season2            97     61
#179 season2            12     50
#180 season2            22     47
#181 season2            65     47
#182 season2             2     38
#183 season2            96     34
#184 season2            32     27
#185 season2            90     26
#186 season2            33     23
#187 season2            92     22
#188 season2            58     16
#189 season2            29     15
#190 season2            93     14
#191 season2            15     10
#192 season2            34      9
#193 season2            54      8
#194 season2            88      8
#195 season2            76      7
#196 season2            86      6
#197 season2            28      5
#198 season2            44      2
#199 season2            57      1






#here,3 most common violations for season 1 are: 

#season      violationCode count_
#1   season1            21 806958
#2   season1            36 609781
#3   season1            38 511894


#here,3 most common violations for season 2 are: 

#season      violationCode count_
#1 season2            36 790833
#2 season2            21 721630
#3 season2            38 550410




























##############################################################
#7.	The fines collected from all the parking violation constitute a revenue source for the NYC police department. 
#Let’s take an example of estimating that for the 3 most commonly occurring codes.

#	Find total occurrences of the 3 most common violation codes

createOrReplaceTempView(viol_timeNotNull_2015_4, "viol_timeNotNull_2015_4_view")

df_2015 <- SparkR::sql("select  `Violation Code` as violationCode  from viol_timeNotNull_2015_4_view  " )




createOrReplaceTempView(viol_timeNotNull_2016_4, "viol_timeNotNull_2016_4_view")

df_2016 <- SparkR::sql("select  `Violation Code` as violationCode  from viol_timeNotNull_2016_4_view  " )




createOrReplaceTempView(viol_timeNotNull_2017_4, "viol_timeNotNull_2017_4_view")

df_2017 <- SparkR::sql("select  `Violation Code` as violationCode  from viol_timeNotNull_2017_4_view  " )




allRecords <- rbind(df_2015,df_2016, df_2017)
nrow(allRecords)
#33233102





allRecords_vio_count <- SparkR::sql("select  `Violation Code` as violationCode, count(*) as count_  from viol_timeNotNull_2017_4_view  group by violationCode order by count_ desc" )

collect(select(allRecords_vio_count, allRecords_vio_count$violationCode , allRecords_vio_count$count_))

#violationCode  count_
#1              21 1528588
#2              36 1400614
#3              38 1062304
#4              14  893497
#5              20  618593
#6              46  600012
#7              37  596769
#8              71  521308
#9              40  519613
#10              7  516395
#11             19  290920
#12             70  273887
#13             69  183509
#14             16  160448
#15             31  146680
#16              5  145655
#17             47  113927
#18             74  112669
#19             50  100115
#20             48   83550
#21             17   74916
#22             84   71608
#23             24   70168
#24             42   64918
#25             51   61389
#26             78   56744
#27              9   56633
#28             98   51011
#29             10   46759
#30             68   42944
#31             53   35323
#32             82   33404
#33             66   25896
#34             13   21109
#35             85   19502
#36             18   18362
#37             45   16260
#38             23   15903
#39             67   15759
#40             77   11878
#41             64   11564
#42             61   10949
#43             72    9830
#44             75    8261
#45             83    8001
#46             11    7127
#47             27    6867
#48             60    6724
#49             79    6200
#50             62    5712
#51             41    5576
#52             80    4381
#53             89    3504
#54             73    3502
#55             35    3495
#56             99    3316
#57             39    2770
#58              8    2309
#59             52    1435
#60             26    1248
#61             63    1111
#62              4    1065
#63              1    1058
#64             91    1003
#65             49     876
#66             30     723
#67             56     713
#68              3     674
#69             94     389
#70             43     370
#71              6     318
#72             81     267
#73              0     239
#74             59     233
#75             55     223
#76             95     182
#77             25     169
#78              2     127
#79             97     112
#80             22     110
#81             65     108
#82             96      81
#83             12      65
#84             58      58
#85             90      53
#86             29      46
#87             33      44
#88             92      43
#89             32      35
#90             76      24
#91             93      23
#92             88      23
#93             15      22
#94             34      18
#95             86      16
#96             54      11
#97             28       9
#98             44       8
#99             57       5
#100            87       1



#top 3 violation codes along woth their occurrence are:
#			violationCode   count_
#1             21 			1528588
#2              36 			1400614
#3              38 			1062304
























###################################################################
#	Then, search the internet for NYC parking violation code fines. You will find a website (on the nyc.gov URL) that lists these fines. They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. For simplicity, take an average of the two

#I created a .csv file using the fine table present at nyc.gov URL
fines <- read.df("s3://datascibkt/fines.csv",source = "csv", inferSchema = "true", header = "true")




nrow(fines)
#91

head(fines)

#if null is present in Manhattan_96th_St_and_below or All_Other_Areas column, defaulting it to 0
fines$Manhattan_96th_St_and_below <- ifelse(isNotNull(fines$Manhattan_96th_St_and_below),fines$Manhattan_96th_St_and_below,0)

fines$All_Other_Areas <- ifelse(isNotNull(fines$All_Other_Areas),fines$All_Other_Areas,0)


head(select(allRecords_vio_count, allRecords_vio_count$violationCode))

#join fines and allRecords_vio_count on fine code
allRecords_vio_count2 <- join(fines,allRecords_vio_count,allRecords_vio_count$violationCode==fines$CODE , "inner")


str(allRecords_vio_count2)
#'SparkDataFrame': 5 variables:
# $ CODE                       : int 21 36 38 14 20 46
# $ Manhattan_96th_St_and_below: num 65 50 65 115 65 115
# $ All_Other_Areas            : num 45 50 35 115 60 115
# $ violationCode              : int 21 36 38 14 20 46
# $ count_                     : num 1528588 1400614 1062304 893497 618593 600012



#take the average  value of (Manhattan_96th_St_and_below + allRecords_vio_count2) as avgFine
allRecords_vio_count2$avgFine <- (allRecords_vio_count2$Manhattan_96th_St_and_below + allRecords_vio_count2$All_Other_Areas)/2

str(allRecords_vio_count2)
#  'SparkDataFrame': 6 variables:
# $ CODE                       : int 21 36 38 14 20 46
# $ Manhattan_96th_St_and_below: num 65 50 65 115 65 115
# $ All_Other_Areas            : num 45 50 35 115 60 115
# $ violationCode              : int 21 36 38 14 20 46
# $ count_                     : num 1528588 1400614 1062304 893497 618593 600012
# $ avgFine                    : num 55 50 50 115 62.5 115





 
 
 
 
 
 
 
 
 
##########################################################
#	Using this information, find the total amount collected for all of the fines. 
#State the code which has the highest total collection. 

allRecords_vio_count2$totalFine <- (allRecords_vio_count2$count_ * allRecords_vio_count2$avgFine)

str(allRecords_vio_count2)
# 'SparkDataFrame': 7 variables:
# $ CODE                       : int 21 36 38 14 20 46
# $ Manhattan_96th_St_and_below: num 65 50 65 115 65 115
# $ All_Other_Areas            : num 45 50 35 115 60 115
# $ violationCode              : int 21 36 38 14 20 46
# $ count_                     : num 1528588 1400614 1062304 893497 618593 600012
# $ avgFine                    : num 55 50 50 115 62.5 115
# $ totalFine                  : num 84072340 70030700 53115200 102752155 38662062.5 69001380


 

createOrReplaceTempView(allRecords_vio_count2, "allRecords_vio_count2_view")


allRecords_vio_count3 <- SparkR::sql("select violationCode, totalFine, count(*) as count_   from allRecords_vio_count2_view group by violationCode, totalFine order by totalFine desc" )


collect(select(allRecords_vio_count3, allRecords_vio_count3$violationCode ,allRecords_vio_count3$totalFine,allRecords_vio_count3$count_ ))



#    violationCode   totalFine 		count_
#1             14 102752155.0      1
#2             21  84072340.0      1
#3             36  70030700.0      1
#4             46  69001380.0      1
#5             40  59755495.0      1
#6             38  53115200.0      1
#7             20  38662062.5      1
#8             71  33885020.0      1
#9             19  33455800.0      1
#10            37  29838450.0      1
#11             7  25819750.0      1
#12            70  17802655.0      1
#13            31  16868200.0      1
#14             5  16750325.0      1
#15            16  15242560.0      1
#16            47  13101605.0      1
#17            69  11928085.0      1
#18            50  11513225.0      1
#19            48   9608250.0      1
#20            74   7323485.0      1
#21            17   7117020.0      1
#22            51   7059735.0      1
#23             9   6512795.0      1
#24            10   5377285.0      1
#25            98   4846045.0      1
#26            24   4385500.0      1
#27            53   4062145.0      1
#28            84   3938440.0      1
#29            82   3841460.0      1
#30            78   3688360.0      1
#31            42   3245900.0      1
#32            68   2791360.0      1
#33            67   2600235.0      1
#34            13   2427535.0      1
#35            18   2111630.0      1
#36            45   1869900.0      1
#37            66   1424280.0      1
#38            85   1267630.0      1
#39            27   1236060.0      1
#40            64   1098580.0      1
#41            23    993937.5      1
#42            11    819605.0      1
#43            79    713000.0      1
#44            77    653290.0      1
#45            72    638950.0      1
#46            61    602195.0      1
#47             1    544870.0      1
#48            75    536965.0      1
#49            83    520065.0      1
#50            89    402960.0      1
#51            30    372345.0      1
#52            60    369820.0      1
#53             3    347110.0      1
#54            62    314160.0      1
#55             8    265535.0      1
#56            80    230002.5      1
#57            73    227630.0      1
#58            35    174750.0      1
#59            39    173125.0      1
#60            52    165025.0      1
#61            26    143520.0      1
#62             6    124020.0      1
#63            63    105545.0      1
#64            49     83220.0      1
#65            56     81995.0      1
#66             2     65405.0      1
#67             4     61237.5      1
#68            94     58350.0      1
#69            91     55165.0      1
#70            59     26795.0      1
#71            55     25645.0      1
#72            81     25365.0      1
#73            29     23690.0      1
#74            25     19435.0      1
#75            43     18500.0      1
#76            65     10260.0      1
#77            96      7695.0      1
#78            22      6600.0      1
#79            12      6175.0      1
#80            97      6160.0      1
#81            58      3190.0      1
#82            92      2365.0      1
#83            33      2200.0      1
#84            86      1840.0      1
#85            32      1750.0      1
#86            93      1495.0      1
#87            34       900.0      1
#88            28       855.0      1
#89            44       400.0      1
#90            57       325.0      1
#91            99         0.0      1


#the code which has the highest total collection is as shown below:
#    violationCode   totalFine 	
#          14       102752155.0 






allRecords_vio_sum_all <- SparkR::sql("select sum(totalFine) as sum_of_total_fine   from allRecords_vio_count2_view " )


collect(select(allRecords_vio_sum_all, allRecords_vio_sum_all$sum_of_total_fine))

#sum_of_total_fine
#1         801464075


##sum_of_total_fine= $801464075


