# cleandata
Getting and cleaning data: Project


Create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.

Extracts only the measurements on the mean and standard deviation for each measurement.

Uses descriptive activity names to name the activities in the data set

Appropriately labels the data set with descriptive variable names.

From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.





Download the raw data files from the URL mentioned on the Introduction section, to the subdirectory Data
Read the labels for features and activities

Read and merge the test and training sets of subjects, measurements and activities

Select only the measurements for the mean and standard deviation. In this step, the meanFreq() measurements (as explained on the features_info.txt file of the original experiment) are not included here because the nature of those values is out of the current project scope.

Rename the variables for measurements with more descriptive names. At this step, the mergedDataset object is generated in R and can be used after the execution of the script (as described on step 2 of the Instruction list section)

Generate the summary data set and write the output to the file Summary.txt into the working directory.
