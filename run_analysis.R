### script run_analysis.R -- Course project for Getting and Cleaning Data
### REQUIREMENTS: You must have the UCI HAR Dataset in your working directory.
### OUTPUT: This script will write a data file called "tidy_data.txt" into
### the working directory.
library(plyr)
library(dplyr)
### QUESTION 1 - create merged data set
## question 1 step 1 - load raw datafiles
x_test<-read.table('UCI HAR Dataset/test/X_test.txt')
y_test<-read.table('UCI HAR Dataset/test/y_test.txt')
x_train<-read.table('UCI HAR Dataset/train/X_train.txt')
y_train<-read.table('UCI HAR Dataset/train/y_train.txt')
subject_train<-read.table('UCI HAR Dataset/train/subject_train.txt')
subject_test<-read.table('UCI HAR Dataset/test/subject_test.txt')
## question 1 step 2 - combine data sets
test_train<-rbind(x_test,x_train)
test_train_activity<- rbind(y_test,y_train)
test_train_subject<-rbind(subject_test,subject_train)
## question 1 step 3 - rename columns for activity data set as its
## column names overlap with the test and training set.
test_train_activity<-rename(test_train_activity,activity=V1)
test_train_subject<-rename(test_train_subject,subject=V1)
## question 1 step 4 - now combine the subject and activity columns.
test_train<-cbind(test_train,test_train_activity)
test_train<-cbind(test_train,test_train_subject)
### 
###QUESTION 2: Extract only the measurements on the mean and standard dev for
### measurement
## read features names
features<-read.table('UCI HAR Dataset/features.txt')
## I interpreted the phrase 
##"only the measurements on the mean and standard deviation for each measurement."
## to mean to look for any features that contain mean() or std()
## 
vv<-c()
for(i in 1:nrow(features)) {
  if(grepl('mean()',features[i,2])) {
    vv<-c(vv,i)
  }
  if(grepl('std()',features[i,2])) {
    vv<-c(vv,i)
  }
}
extract_names<-c(vv,562:563)  
## 562 and 563 are the two additional fields added (activity and subject)
## the mean and std columns and activity and subject are put into
## a new data frame - mean_std_test_train
mean_std_test_train<-select(test_train,extract_names)

##QUESTION 3: Uses descriptive activity names to name the activities in the data set
activity_labels<-read.table('UCI HAR Dataset/activity_labels.txt')
activity_labels<-rename(activity_labels,activity=V1, activity_label=V2)
merged_data<-merge(mean_std_test_train,activity_labels,by.x="activity",by.y="activity",all=TRUE)
# remove the "activity" column as it is now redundant with the "activity_label"
# column
merged_data$activity = NULL
##QUESTION 4: Appropriately labels the data set with descriptive variable names. 
for (i in 1:length(names(merged_data))) {
  if( substr(names(merged_data[i]),1,1)=="V" ) {
    #find the index number in features -- this would be the column name without "V"
    feature_index<-as.numeric(substr(names(merged_data)[i],2,nchar(names(merged_data)[i])))
    names(merged_data)[i] <-as.character(features[feature_index,2]) 
  }
  }

##QUESTION 5: From the data set in step 4, creates a second, independent tidy data set 
##with the average of each variable for each activity and each subject

tidy_data<-merged_data %>% group_by(activity_label,subject) %>% summarise_each(funs(mean))
write.table(tidy_data,"tidy_data.txt", row.names=FALSE)