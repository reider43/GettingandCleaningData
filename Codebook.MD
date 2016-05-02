# Code Book

This code outlines the steps taken to create Tidy data from the following referenced data set:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

The data description of the untidy data can be referenced at:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Transformation and Work

Please see the extremely well doucumented code in run_analysis.R

## Variables

* `activityLabelsDF` - A data frame for the Activity Labels, and title columns Index and Activity, as class integer and character
* `activityLabels` - A vector of just the Activity for assignment latter in the joined tables
* `featuresDF`- A data frame for all Features, and title columns Index and Feature, as class numeric and character
* `featuresDF`- Modify features data frame to only contain mean() and std() re: features_info.tx
* `featuresIndex` - A vector of the indexes of features
* `TrainTest` - Merge datasets
* `TrainTestDT` - A Data Table from the data frame TrainTest
* `TrainTestAvg` - The average of each column by activity and subject TrainTestDT

## Identifiers

* `Subject` - The ID of the test subject
* `Sctivity` - The type of activity performed when the corresponding measurements were taken

## Measurements

 "Mean-tBodyAcc-X"
 "Mean-tBodyAcc-Y"          
 "Mean-tBodyAcc-Z"          
 "Std-tBodyAcc-X"           
 "Std-tBodyAcc-Y"            
 "Std-tBodyAcc-Z"           
 "Mean-tGravityAcc-X"        
 "Mean-tGravityAcc-Y"       
 "Mean-tGravityAcc-Z"        
 "Std-tGravityAcc-X"        
 "Std-tGravityAcc-Y"         
 "Std-tGravityAcc-Z"        
 "Mean-tBodyAccJerk-X"       
 "Mean-tBodyAccJerk-Y"      
 "Mean-tBodyAccJerk-Z"       
 "Std-tBodyAccJerk-X"       
 "Std-tBodyAccJerk-Y"        
 "Std-tBodyAccJerk-Z"       
 "Mean-tBodyGyro-X"          
 "Mean-tBodyGyro-Y"         
 "Mean-tBodyGyro-Z"          
 "Std-tBodyGyro-X"          
 "Std-tBodyGyro-Y"           
 "Std-tBodyGyro-Z"          
 "Mean-tBodyGyroJerk-X"
 "Mean-tBodyGyroJerk-Y"     
 "Mean-tBodyGyroJerk-Z"      
 "Std-tBodyGyroJerk-X"      
 "Std-tBodyGyroJerk-Y"       
 "Std-tBodyGyroJerk-Z"      
 "Mean-tBodyAccMag"          
 "Std-tBodyAccMag"          
 "Mean-tGravityAccMag"       
 "Std-tGravityAccMag"       
 "Mean-tBodyAccJerkMag"      
 "Std-tBodyAccJerkMag"      
 "Mean-tBodyGyroMag"         
 "Std-tBodyGyroMag"         
 "Mean-tBodyGyroJerkMag"     
 "Std-tBodyGyroJerkMag"     
 "Mean-fBodyAcc-X"           
 "Mean-fBodyAcc-Y"          
 "Mean-fBodyAcc-Z"           
 "Std-fBodyAcc-X"           
 "Std-fBodyAcc-Y"            
 "Std-fBodyAcc-Z"           
 "Mean-fBodyAccJerk-X"       
 "Mean-fBodyAccJerk-Y"      
 "Mean-fBodyAccJerk-Z"       
 "Std-fBodyAccJerk-X"       
 "Std-fBodyAccJerk-Y"        
 "Std-fBodyAccJerk-Z"       
 "Mean-fBodyGyro-X"          
 "Mean-fBodyGyro-Y"         
 "Mean-fBodyGyro-Z"          
 "Std-fBodyGyro-X"          
 "Std-fBodyGyro-Y"           
 "Std-fBodyGyro-Z"          
 "Mean-fBodyAccMag"          
 "Std-fBodyAccMag"          
 "Mean-fBodyBodyAccJerkMag"  
 "Std-fBodyBodyAccJerkMag"  
 "Mean-fBodyBodyGyroMag"     
 "Std-fBodyBodyGyroMag"     
 "Mean-fBodyBodyGyroJerkMag" 
 "Std-fBodyBodyGyroJerkMag" 

## Activity Labels

* `WALKING` (value `1`): subject was walking during the test
* `WALKING_UPSTAIRS` (value `2`): subject was walking up a staircase during the test
* `WALKING_DOWNSTAIRS` (value `3`): subject was walking down a staircase during the test
* `SITTING` (value `4`): subject was sitting during the test
* `STANDING` (value `5`): subject was standing during the test
* `LAYING` (value `6`): subject was laying down during the test