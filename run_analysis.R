Coursera - Getting and cleaning data
# Course project

library(data.table)
library(plyr)

# Download zip file

dir.create("./data")


fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"



download.file(fileUrl, 
              "./data/getdata-projectfiles-UCI HAR Dataset.zip", 
              mode = "wb") # mode "wb" for binary files


unzip("./data/getdata-projectfiles-UCI HAR Dataset.zip", exdir = "./data")

# Read the labels for features
featureLabels <- read.table("./data/UCI HAR Dataset/features.txt")
names(featureLabels) <- c("ID_feature", "feature")

# Read the label for activities
activityLabels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")
names(activityLabels) <- c("ID_activity", "activity")

# Subject (test and train set)
subjectsTest <- read.table("./data/UCI HAR Dataset/test/subject_test.txt")
subjectsTest <- cbind(subjectsTest[, 1, drop = FALSE], measurement_type = "TEST")
subjectsTrain <- read.table("./data/UCI HAR Dataset/train/subject_train.txt")
subjectsTrain <- cbind(subjectsTrain[, 1, drop = FALSE], measurement_type = "TRAINING")
subjects <- rbind(subjectsTest, subjectsTrain)
names(subjects) <- c("ID_subject", "measurement_type")

# All values from X (test and train set)
allValues <- rbind(read.table("./data/UCI HAR Dataset/test/X_test.txt"), 
                   read.table("./data/UCI HAR Dataset/train/X_train.txt"))
names(allValues) <- featureLabels[["feature"]]

# All activities (test and train set)
allActivities <- rbind(read.table("./data/UCI HAR Dataset/test/y_test.txt"), 
                       read.table("./data/UCI HAR Dataset/train/y_train.txt"))
names(allActivities) <- c("ID_activity")

# Merge all and add the activity labels (in this step because merge() reorders)
mergedDataset <- cbind(subjects, allActivities, allValues)
mergedDataset <- merge(mergedDataset, activityLabels, 
                       by.x = "ID_activity", by.y = "ID_activity",
                       all = TRUE)


# Select "ID_subject", "ID_activity", "activity" and 
# all the features for "mean" and "std"
featuresFiltered <- 
  rbind(featureLabels[featureLabels$feature %like% "mean()" 
                      & !featureLabels$feature %like% "meanFreq()", ],
        featureLabels[featureLabels$feature %like% "std()", ])

fieldListSubset <- c("ID_subject",
                     "measurement_type",
                     "ID_activity", 
                     "activity",
                     as.vector(featuresFiltered[["feature"]]))

mergedDataset <- mergedDataset[fieldListSubset]

# Rename measurement variables 
mergedDataset <- rename(mergedDataset, 
                        c("tBodyAcc-mean()-X" = "bodyAccelerationMeanByTime_XAxis",
                          "tBodyAcc-mean()-Y" = "bodyAccelerationMeanByTime_YAxis",
                          "tBodyAcc-mean()-Z" = "bodyAccelerationMeanByTime_ZAxis",
                          "tGravityAcc-mean()-X" = "gravityAccelerationMeanByTime_XAxis",
                          "tGravityAcc-mean()-Y" = "gravityAccelerationMeanByTime_YAxis",
                          "tGravityAcc-mean()-Z" = "gravityAccelerationMeanByTime_ZAxis",
                          "tBodyAccJerk-mean()-X" = "bodyAccelerationJerkMeanByTime_XAxis",
                          "tBodyAccJerk-mean()-Y" = "bodyAccelerationJerkMeanByTime_YAxis",
                          "tBodyAccJerk-mean()-Z" = "bodyAccelerationJerkMeanByTime_ZAxis",
                          "tBodyGyro-mean()-X" = "bodyAngularVelocityMeanByTime_XAxis",
                          "tBodyGyro-mean()-Y" = "bodyAngularVelocityMeanByTime_YAxis",
                          "tBodyGyro-mean()-Z" = "bodyAngularVelocityMeanByTime_ZAxis",
                          "tBodyGyroJerk-mean()-X" = "bodyAngularVelocityJerkMeanByTime_XAxis",
                          "tBodyGyroJerk-mean()-Y" = "bodyAngularVelocityJerkMeanByTime_YAxis",
                          "tBodyGyroJerk-mean()-Z" = "bodyAngularVelocityJerkMeanByTime_ZAxis",
                          "tBodyAccMag-mean()" = "bodyAccelerationMagnitudeMeanByTime",
                          "tGravityAccMag-mean()" = "gravityAccelerationMagnitudeMeanByTime",
                          "tBodyAccJerkMag-mean()" = "bodyAccelerationJerkMagnitudeMeanByTime",
                          "tBodyGyroMag-mean()" = "bodyAngularVelocityMagnitudeMeanByTime",
                          "tBodyGyroJerkMag-mean()" = "bodyAngularVelocityJerkMagnitudeMeanByTime",
                          "fBodyAcc-mean()-X" = "bodyAccelerationMeanByFrequency_XAxis",
                          "fBodyAcc-mean()-Y" = "bodyAccelerationMeanByFrequency_YAxis",
                          "fBodyAcc-mean()-Z" = "bodyAccelerationMeanByFrequency_ZAxis",
                          "fBodyAccJerk-mean()-X" = "bodyAccelerationJerkMeanByFrequency_XAxis",
                          "fBodyAccJerk-mean()-Y" = "bodyAccelerationJerkMeanByFrequency_YAxis",
                          "fBodyAccJerk-mean()-Z" = "bodyAccelerationJerkMeanByFrequency_ZAxis",
                          "fBodyGyro-mean()-X" = "bodyAngularVelocityMeanByFrequency_XAxis",
                          "fBodyGyro-mean()-Y" = "bodyAngularVelocityMeanByFrequency_YAxis",
                          "fBodyGyro-mean()-Z" = "bodyAngularVelocityMeanByFrequency_ZAxis",
                          "fBodyAccMag-mean()" = "bodyAccelerationMagnitudeMeanByFrequency",
                          "fBodyBodyAccJerkMag-mean()" = "bodyAccelerationJerkMagnitudeMeanByFrequency",
                          "fBodyBodyGyroMag-mean()" = "bodyAngularVelocityMagnitudeMeanByFrequency",
                          "fBodyBodyGyroJerkMag-mean()" = "bodyAngularVelocityJerkMagnitudeMeanByFrequency",
                          "tBodyAcc-std()-X" = "bodyAccelerationStandardDeviationByTime_XAxis",
                          "tBodyAcc-std()-Y" = "bodyAccelerationStandardDeviationByTime_YAxis",
                          "tBodyAcc-std()-Z" = "bodyAccelerationStandardDeviationByTime_ZAxis",
                          "tGravityAcc-std()-X" = "gravityAccelerationStandardDeviationByTime_XAxis",
                          "tGravityAcc-std()-Y" = "gravityAccelerationStandardDeviationByTime_YAxis",
                          "tGravityAcc-std()-Z" = "gravityAccelerationStandardDeviationByTime_ZAxis",
                          "tBodyAccJerk-std()-X" = "bodyAccelerationJerkStandardDeviationByTime_XAxis",
                          "tBodyAccJerk-std()-Y" = "bodyAccelerationJerkStandardDeviationByTime_YAxis",
                          "tBodyAccJerk-std()-Z" = "bodyAccelerationJerkStandardDeviationByTime_ZAxis",
                          "tBodyGyro-std()-X" = "bodyAngularVelocityStandardDeviationByTime_XAxis",
                          "tBodyGyro-std()-Y" = "bodyAngularVelocityStandardDeviationByTime_YAxis",
                          "tBodyGyro-std()-Z" = "bodyAngularVelocityStandardDeviationByTime_ZAxis",
                          "tBodyGyroJerk-std()-X" = "bodyAngularVelocityJerkStandardDeviationByTime_XAxis",
                          "tBodyGyroJerk-std()-Y" = "bodyAngularVelocityJerkStandardDeviationByTime_YAxis",
                          "tBodyGyroJerk-std()-Z" = "bodyAngularVelocityJerkStandardDeviationByTime_ZAxis",
                          "tBodyAccMag-std()" = "bodyAccelerationMagnitudeStandardDeviationByTime",
                          "tGravityAccMag-std()" = "gravityAccelerationMagnitudeStandardDeviationByTime",
                          "tBodyAccJerkMag-std()" = "bodyAccelerationJerkMagnitudeStandardDeviationByTime",
                          "tBodyGyroMag-std()" = "bodyAngularVelocityMagnitudeStandardDeviationByTime",
                          "tBodyGyroJerkMag-std()" = "bodyAngularVelocityJerkMagnitudeStandardDeviationByTime",
                          "fBodyAcc-std()-X" = "bodyAccelerationStandardDeviationByFrequency_XAxis",
                          "fBodyAcc-std()-Y" = "bodyAccelerationStandardDeviationByFrequency_YAxis",
                          "fBodyAcc-std()-Z" = "bodyAccelerationStandardDeviationByFrequency_ZAxis",
                          "fBodyAccJerk-std()-X" = "bodyAccelerationJerkStandardDeviationByFrequency_XAxis",
                          "fBodyAccJerk-std()-Y" = "bodyAccelerationJerkStandardDeviationByFrequency_YAxis",
                          "fBodyAccJerk-std()-Z" = "bodyAccelerationJerkStandardDeviationByFrequency_ZAxis",
                          "fBodyGyro-std()-X" = "bodyAngularVelocityStandardDeviationByFrequency_XAxis",
                          "fBodyGyro-std()-Y" = "bodyAngularVelocityStandardDeviationByFrequency_YAxis",
                          "fBodyGyro-std()-Z" = "bodyAngularVelocityStandardDeviationByFrequency_ZAxis",
                          "fBodyAccMag-std()" = "bodyAccelerationMagnitudeStandardDeviationByFrequency",
                          "fBodyBodyAccJerkMag-std()" = "bodyAccelerationJerkMagnitudeStandardDeviationByFrequency",
                          "fBodyBodyGyroMag-std()" = "bodyAngularVelocityMagnitudeStandardDeviationByFrequency",
                          "fBodyBodyGyroJerkMag-std()" = "bodyAngularVelocityJerkMagnitudeStandardDeviationByFrequency"))


# It's important to load dplyr in this step because rename() from plyr is 
# the good function implementation which works ok on the previous step
library(dplyr)

# Compute the mean of each column grouped by subject and activity
# Columns "measurement_type" and "ID_activity" are irrelevant for this summary,
# so we use the special summarise_each_() function
subjectAndActivity <- group_by(mergedDataset, ID_subject, activity) 
summaryDataFrame <- 
  summarise_each_(subjectAndActivity, 
                  funs(mean), 
                  list(quote(-measurement_type), quote(-ID_activity)))

# Write the summary data frame without row names
write.table(summaryDataFrame, 
            file = "./data/tidy.txt", 
            row.names = FALSE)