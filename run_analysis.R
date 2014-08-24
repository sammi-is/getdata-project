##
##  Getting And Cleaning Data
##
##  Course Project
##
##  File: run_analysis.R
##
##  Author: Sæmundur Melstað
##  Date..: 24.8.2014
##
##  Read datafiles for training dataset
##  First subject, then activity and then features
train_set = read.csv("data/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE, col.names=c("Subject"))
train_set = cbind(train_set, read.csv("data/UCI HAR Dataset/train/Y_train.txt", sep="", header=FALSE, col.names=c("Activity"),colClasses=c("numeric")))
train_set = cbind(train_set, read.csv("data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE))
##
##  Read datafiles for test dataset
##  First subject, then activity and then features
test_set  = read.csv("data/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE, col.names=c("Subject"))
test_set  = cbind(test_set, read.csv("data/UCI HAR Dataset/test/Y_test.txt", sep="", header=FALSE, col.names=c("Activity"),colClasses=c("numeric")))
test_set  = cbind(test_set, read.csv("data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE))
##
##  Read activity labels from datafile
activityLabels = read.csv("data/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE, col.names=c("Activity","ActivityName"))
##
##  Read the features from the features file 
features = read.csv("data/UCI HAR Dataset/features.txt", sep="", header=FALSE, col.names=c("Feature","FeatureName"))
##
##  Merge training and test sets together
merged_set = rbind(train_set, test_set)
##
##  Find all mean and standard deviation columns
columns_mean_std <- features[grep("mean\\(|std\\(",features$FeatureName),]
##
##  Select the columns from the features table for mean and std columns and
##  create list of selected columns. Adding subject and activity in front
sel_features <- features[columns_mean_std$Feature,]
sel_columns <- c(1,2,sel_features$Feature+2)
##
##  Reduce merged dataset down to selected columns
merged_set <- merged_set[,c(1,2,sel_features$Feature+2)]
##
##  Changing columns names off dataset
colnames(merged_set) <- c("Subject","Activity",as.character(sel_features$FeatureName))
##
##  Change activity codes to activity names
CurrAct <- 1
for (Label in activityLabels$ActivityName) {
    merged_set$Activity <- gsub(as.character(CurrAct), Label, merged_set$Activity)
    CurrAct <- CurrAct + 1
}
##
##  Aggregate dataset and write to file
##  Remove two columns (Activity and Subject which comes twice)
tidy_set = aggregate(merged_set, by=list(Activity = merged_set$Activity, Subject=merged_set$Subject), mean)
tidy_set[,3] <- NULL
tidy_set[,3] <- NULL
write.table(tidy_set, "tidy.txt", row.name=FALSE)