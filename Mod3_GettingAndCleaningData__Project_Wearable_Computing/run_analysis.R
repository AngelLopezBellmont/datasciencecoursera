

# PORJECT Coursera Getting and Cleaning Data Course Project 
# Angel Lopez Bellmont 2015.07.22
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# 

#------------------------------------------------------------------------------------------------------
 setwd( paste("c:\\2015.01.12_CarpetaAngel\\00Angel\\SoftwareProgramsCursosIT\\Coursera\\",
           "2015.07.01-Mod3_GettingAndCleaningData\\Project_Wearable_Computing", sep=""))

#rm(list=ls())  # Clean up workspace


############################################################################################################
# 0. Download and unzip the file
# The unzip does not work I don´t know why :( 
# Anyway I copy manually the . zip and I unzip it. I create the file  getdata-projectfiles-UCI HAR Dataset.
# I comment all this code

############################################################################################################


# library(reshape2)
# 
# filename <- "getdata_dataset.zip"
# 
# if (!file.exists(filename))
#  {
#             fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip "
#             #download.file(fileURL, filename, method="curl")
#             download.file(fileURL, filename)
#  }  
# if (!file.exists("UCI HAR Dataset")) 
#  { 
#             unzip(filename) 
#  }

########################################################################################################

# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped


# Read in the data from files: (I manually write underscore between  all the names of the folder. Ex: UCI_HAR_Dataset, etc
# Well not at the end I dont do it. I let the folder´s names how they are :). The path names are long but it´s ok anyway )

#File Paths:
file_Xtrain              = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/X_train.txt"
file_ytrain              = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/y_train.txt"
file_subject_train       = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/train/subject_train.txt"
            
file_Xtest               =  "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/X_test.txt"
file_ytest               =  "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/y_test.txt"
file_subject_test        =  "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/test/subject_test.txt"

activity_labels          = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/activity_labels.txt"
features                 = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features.txt"
#features_info           = "./getdata-projectfiles-UCI HAR Dataset/UCI HAR Dataset/features_info.txt"


#Read the file Paths:
library(plyr)

if(FALSE) # So I dont have to read eacht time I test the program. If I write TRUE, the code will be executed.
{
Train_X                  = read.table( file_Xtrain, header=FALSE)                   #import Train_X
Train_y                  = read.table( file_ytrain, header=FALSE)                   #import Train_y 
Tsubject_train           = read.table( file_subject_train, header=FALSE)            #T is for table

Test_X                   = read.table( file_Xtest, header=FALSE)                    #import Test_X
Test_y                   = read.table( file_ytest, header=FALSE)                    #import Test_y
Tsubject_test            = read.table( file_subject_test, header=FALSE)             #T is for table

Tactivity_labels         = read.table( activity_labels, header=FALSE)               #T is for table
Tfeatures                = read.table( features, header=FALSE)                      #T is for table
#Tfeatures_info          = read.table( features_info, header=FALSE)                 #T is for table

}



#Giving NAMES to COLUMNS in the dataframes. I just imported
colnames(Tactivity_labels)  = c('activity_Id','activity_Description');

colnames(Tsubject_train)    = "subject_Id";
colnames(Train_X)           = Tfeatures[,2]; # I set the names in Train_X with Tfeatures[,2]
colnames(Train_y)           = "activity_Id";

colnames(Tsubject_test)     = "subject_Id";
colnames(Test_X)            = Tfeatures[,2]; # I set the names in Test_X with Tfeatures[,2]
colnames(Test_y)            = "activity_Id";


# Mergin trainingData, testData in singles Data Frame:
trainingDF = cbind(Train_y,Tsubject_train,Train_X)       #DF is for DataFrame
testDF     = cbind(Test_y,Tsubject_test,Test_X)

allData  = rbind(trainingDF, testDF)

#Vector with all the Column Names
vectNamesAllData  = colnames(allData)




# 2. Extracts only the measurements on the mean and standard deviation for each measurement.

# Create a logicalVector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
#logicalVector = (grepl("activity..",vectNamesAllData) | grepl("subject..",vectNamesAllData) | grepl("-mean..",vectNamesAllData) & !grepl("-meanFreq..",vectNamesAllData) & !grepl("mean..-",vectNamesAllData) | grepl("-std..",vectNamesAllData) & !grepl("-std()..-",vectNamesAllData))

#logicalVector = (grepl("activity..",vectNamesAllData) | grepl("subject..",vectNamesAllData) | grepl("-mean..",vectNamesAllData) |  grepl("mean..",vectNamesAllData)| grepl("-std..",vectNamesAllData))
logicalVector = (grepl("activity..",vectNamesAllData) | grepl("subject..",vectNamesAllData) | grepl("-mean..",vectNamesAllData) | grepl("-std..",vectNamesAllData))



# Subset finalData table based on the logicalVector to keep only desired columns
AllData_2 = allData[logicalVector==TRUE];

# here We get a 20 Columns Dataframe
vectNamesAllData_2 = names(AllData_2) 

# > vectNamesAllData_2
# [1] "activity_Id"                     "subject_Id"                      "tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"              
# [5] "tBodyAcc-mean()-Z"               "tBodyAcc-std()-X"                "tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"               
# [9] "tGravityAcc-mean()-X"            "tGravityAcc-mean()-Y"            "tGravityAcc-mean()-Z"            "tGravityAcc-std()-X"            
# [13] "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"             "tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"          
# [17] "tBodyAccJerk-mean()-Z"           "tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"           
# [21] "tBodyGyro-mean()-X"              "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"              "tBodyGyro-std()-X"              
# [25] "tBodyGyro-std()-Y"               "tBodyGyro-std()-Z"               "tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"         
# [29] "tBodyGyroJerk-mean()-Z"          "tBodyGyroJerk-std()-X"           "tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"          
# [33] "tBodyAccMag-mean()"              "tBodyAccMag-std()"               "tGravityAccMag-mean()"           "tGravityAccMag-std()"           
# [37] "tBodyAccJerkMag-mean()"          "tBodyAccJerkMag-std()"           "tBodyGyroMag-mean()"             "tBodyGyroMag-std()"             
# [41] "tBodyGyroJerkMag-mean()"         "tBodyGyroJerkMag-std()"          "fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"              
# [45] "fBodyAcc-mean()-Z"               "fBodyAcc-std()-X"                "fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"               
# [49] "fBodyAcc-meanFreq()-X"           "fBodyAcc-meanFreq()-Y"           "fBodyAcc-meanFreq()-Z"           "fBodyAccJerk-mean()-X"          
# [53] "fBodyAccJerk-mean()-Y"           "fBodyAccJerk-mean()-Z"           "fBodyAccJerk-std()-X"            "fBodyAccJerk-std()-Y"           
# [57] "fBodyAccJerk-std()-Z"            "fBodyAccJerk-meanFreq()-X"       "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"      
# [61] "fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"              "fBodyGyro-mean()-Z"              "fBodyGyro-std()-X"              
# [65] "fBodyGyro-std()-Y"               "fBodyGyro-std()-Z"               "fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"         
# [69] "fBodyGyro-meanFreq()-Z"          "fBodyAccMag-mean()"              "fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"         
# [73] "fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-std()"       "fBodyBodyAccJerkMag-meanFreq()"  "fBodyBodyGyroMag-mean()"        
# [77] "fBodyBodyGyroMag-std()"          "fBodyBodyGyroMag-meanFreq()"     "fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"     
# [81] "fBodyBodyGyroJerkMag-meanFreq()"




# 3. Use descriptive activity names to name the activities in the data set

# We put toguether (merge) subAllData with activity_labels.txt which we import in Tactivity_labels
# to give a  description  activity names
# Table [Tactivity_labels] contains this:
# 1 WALKING
# 2 WALKING_UPSTAIRS
# 3 WALKING_DOWNSTAIRS
# 4 SITTING
# 5 STANDING
# 6 LAYING

allData_3 = merge(AllData_2,Tactivity_labels,by='activity_Id',all.x=TRUE);

vectNamesAllData_3  = colnames(allData_3); 

# > vectNamesAllData_3
# [1] "activity_Id"                     "subject_Id"                      "tBodyAcc-mean()-X"               "tBodyAcc-mean()-Y"              
# [5] "tBodyAcc-mean()-Z"               "tBodyAcc-std()-X"                "tBodyAcc-std()-Y"                "tBodyAcc-std()-Z"               
# [9] "tGravityAcc-mean()-X"            "tGravityAcc-mean()-Y"            "tGravityAcc-mean()-Z"            "tGravityAcc-std()-X"            
# [13] "tGravityAcc-std()-Y"             "tGravityAcc-std()-Z"             "tBodyAccJerk-mean()-X"           "tBodyAccJerk-mean()-Y"          
# [17] "tBodyAccJerk-mean()-Z"           "tBodyAccJerk-std()-X"            "tBodyAccJerk-std()-Y"            "tBodyAccJerk-std()-Z"           
# [21] "tBodyGyro-mean()-X"              "tBodyGyro-mean()-Y"              "tBodyGyro-mean()-Z"              "tBodyGyro-std()-X"              
# [25] "tBodyGyro-std()-Y"               "tBodyGyro-std()-Z"               "tBodyGyroJerk-mean()-X"          "tBodyGyroJerk-mean()-Y"         
# [29] "tBodyGyroJerk-mean()-Z"          "tBodyGyroJerk-std()-X"           "tBodyGyroJerk-std()-Y"           "tBodyGyroJerk-std()-Z"          
# [33] "tBodyAccMag-mean()"              "tBodyAccMag-std()"               "tGravityAccMag-mean()"           "tGravityAccMag-std()"           
# [37] "tBodyAccJerkMag-mean()"          "tBodyAccJerkMag-std()"           "tBodyGyroMag-mean()"             "tBodyGyroMag-std()"             
# [41] "tBodyGyroJerkMag-mean()"         "tBodyGyroJerkMag-std()"          "fBodyAcc-mean()-X"               "fBodyAcc-mean()-Y"              
# [45] "fBodyAcc-mean()-Z"               "fBodyAcc-std()-X"                "fBodyAcc-std()-Y"                "fBodyAcc-std()-Z"               
# [49] "fBodyAcc-meanFreq()-X"           "fBodyAcc-meanFreq()-Y"           "fBodyAcc-meanFreq()-Z"           "fBodyAccJerk-mean()-X"          
# [53] "fBodyAccJerk-mean()-Y"           "fBodyAccJerk-mean()-Z"           "fBodyAccJerk-std()-X"            "fBodyAccJerk-std()-Y"           
# [57] "fBodyAccJerk-std()-Z"            "fBodyAccJerk-meanFreq()-X"       "fBodyAccJerk-meanFreq()-Y"       "fBodyAccJerk-meanFreq()-Z"      
# [61] "fBodyGyro-mean()-X"              "fBodyGyro-mean()-Y"              "fBodyGyro-mean()-Z"              "fBodyGyro-std()-X"              
# [65] "fBodyGyro-std()-Y"               "fBodyGyro-std()-Z"               "fBodyGyro-meanFreq()-X"          "fBodyGyro-meanFreq()-Y"         
# [69] "fBodyGyro-meanFreq()-Z"          "fBodyAccMag-mean()"              "fBodyAccMag-std()"               "fBodyAccMag-meanFreq()"         
# [73] "fBodyBodyAccJerkMag-mean()"      "fBodyBodyAccJerkMag-std()"       "fBodyBodyAccJerkMag-meanFreq()"  "fBodyBodyGyroMag-mean()"        
# [77] "fBodyBodyGyroMag-std()"          "fBodyBodyGyroMag-meanFreq()"     "fBodyBodyGyroJerkMag-mean()"     "fBodyBodyGyroJerkMag-std()"     
# [81] "fBodyBodyGyroJerkMag-meanFreq()" "activity_Description"   


       

# 4. Appropriately label the data set with descriptive activity names. 

allData_4 = allData_3  # So we have a consistency nomenclature

# I  change here the name to make it shorter. cn3: column names 3. Beacuse I find too big vectNamesAllData_3
cn3 = vectNamesAllData_3  

# Cleaning up the variable names
for (i in 1:length(vectNamesAllData_3)) 
{
            cn3[i] = gsub("-std","StdDev",cn3[i])
            cn3[i] = gsub("-mean","Mean",cn3[i])
            cn3[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",cn3[i])
            cn3[i] = gsub("[Gg]yro","Gyro",cn3[i])
            cn3[i] = gsub("AccMag","AccMagnitude",cn3[i])
            cn3[i] = gsub("JerkMag","JerkMagnitude",cn3[i])
            cn3[i] = gsub("GyroMag","GyroMagnitude",cn3[i])
            cn3[i] = gsub("\\()","",cn3[i])
            #cn3[i] = gsub("StdDev","StandarDesviation",cn3[i])
            
            
}

#here we rename again the vector names and we call it vectNamesAllData_4 because we are in the point4.
vectNamesAllData_4 = cn3

# We rename allData_3  with the new column names vectNamesAllData_4.
colnames(allData_4) = vectNamesAllData_4


# > vectNamesAllData_4  All the names of the DataFrame for the subset
# [1] "activity_Id"                    "subject_Id"                     "tBodyAccMean-X"                 "tBodyAccMean-Y"                
# [5] "tBodyAccMean-Z"                 "tBodyAccStdDev-X"               "tBodyAccStdDev-Y"               "tBodyAccStdDev-Z"              
# [9] "tGravityAccMean-X"              "tGravityAccMean-Y"              "tGravityAccMean-Z"              "tGravityAccStdDev-X"           
# [13] "tGravityAccStdDev-Y"            "tGravityAccStdDev-Z"            "tBodyAccJerkMean-X"             "tBodyAccJerkMean-Y"            
# [17] "tBodyAccJerkMean-Z"             "tBodyAccJerkStdDev-X"           "tBodyAccJerkStdDev-Y"           "tBodyAccJerkStdDev-Z"          
# [21] "tBodyGyroMean-X"                "tBodyGyroMean-Y"                "tBodyGyroMean-Z"                "tBodyGyroStdDev-X"             
# [25] "tBodyGyroStdDev-Y"              "tBodyGyroStdDev-Z"              "tBodyGyroJerkMean-X"            "tBodyGyroJerkMean-Y"           
# [29] "tBodyGyroJerkMean-Z"            "tBodyGyroJerkStdDev-X"          "tBodyGyroJerkStdDev-Y"          "tBodyGyroJerkStdDev-Z"         
# [33] "tBodyAccMagnitudeMean"          "tBodyAccMagnitudeStdDev"        "tGravityAccMagnitudeMean"       "tGravityAccMagnitudeStdDev"    
# [37] "tBodyAccJerkMagnitudeMean"      "tBodyAccJerkMagnitudeStdDev"    "tBodyGyroMagnitudeMean"         "tBodyGyroMagnitudeStdDev"      
# [41] "tBodyGyroJerkMagnitudeMean"     "tBodyGyroJerkMagnitudeStdDev"   "fBodyAccMean-X"                 "fBodyAccMean-Y"                
# [45] "fBodyAccMean-Z"                 "fBodyAccStdDev-X"               "fBodyAccStdDev-Y"               "fBodyAccStdDev-Z"              
# [49] "fBodyAccMeanFreq-X"             "fBodyAccMeanFreq-Y"             "fBodyAccMeanFreq-Z"             "fBodyAccJerkMean-X"            
# [53] "fBodyAccJerkMean-Y"             "fBodyAccJerkMean-Z"             "fBodyAccJerkStdDev-X"           "fBodyAccJerkStdDev-Y"          
# [57] "fBodyAccJerkStdDev-Z"           "fBodyAccJerkMeanFreq-X"         "fBodyAccJerkMeanFreq-Y"         "fBodyAccJerkMeanFreq-Z"        
# [61] "fBodyGyroMean-X"                "fBodyGyroMean-Y"                "fBodyGyroMean-Z"                "fBodyGyroStdDev-X"             
# [65] "fBodyGyroStdDev-Y"              "fBodyGyroStdDev-Z"              "fBodyGyroMeanFreq-X"            "fBodyGyroMeanFreq-Y"           
# [69] "fBodyGyroMeanFreq-Z"            "fBodyAccMagnitudeMean"          "fBodyAccMagnitudeStdDev"        "fBodyAccMagnitudeMeanFreq"     
# [73] "fBodyAccJerkMagnitudeMean"      "fBodyAccJerkMagnitudeStdDev"    "fBodyAccJerkMagnitudeMeanFreq"  "fBodyGyroMagnitudeMean"        
# [77] "fBodyGyroMagnitudeStdDev"       "fBodyGyroMagnitudeMeanFreq"     "fBodyGyroJerkMagnitudeMean"     "fBodyGyroJerkMagnitudeStdDev"  
# [81] "fBodyGyroJerkMagnitudeMeanFreq" "activity_Description"  





# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

library(dplyr)

#allData_5 = allData_4 %>% group_by(activity_Id,subject_Id) %>% summarise_each(funs(mean))
allData_5 = allData_4 %>% group_by(activity_Description,subject_Id) %>% summarise_each(funs(mean))
## Write data to file
write.table(allData_5,"tidyData.txt",row.name=FALSE)






if(FALSE)
{
#Test of function ddply. I know how to use for one Column but not for all DataFrame
#             allData_5 <- ddply(allData_4, c("Column_ID1", "Column_ID2"),summarise,
#                            N    = length(Column1),
#                            mean = mean(Column1),
#                            sd   = sd(Column1),
#                            se   = sd / sqrt(N) 
#             )
}

