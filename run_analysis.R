run_analysis <- function()
{
  library(dplyr)
  
  #Read train data
  xtrain = read.table("./UCI HAR Dataset/train/X_train.txt")
  ytrain = read.table("./UCI HAR Dataset/train/Y_train.txt")
  subject_train= read.table("./UCI HAR Dataset/train/subject_train.txt")
  
  #Read test data
  xtest = read.table("./UCI HAR Dataset/test/X_test.txt")
  ytest = read.table("./UCI HAR Dataset/test/Y_test.txt")
  subject_test= read.table("./UCI HAR Dataset/test/subject_test.txt")
  
  #Read features data
  features = read.table("./UCI HAR Dataset/features.txt")
  activity_labels= read.table("./UCI HAR Dataset/activity_labels.txt")
  
  #1 Merges the training and the test sets to create one data set.
  
  x_total <- rbind(xtrain, xtest)
  y_total <- rbind(ytrain, ytest)
  sub_total <- rbind(subject_train,subject_test)
  
  #2 Extracts only the measurements on the mean and standard deviation for each measurement.
  
  sel_measure <-  features[grep("mean\\(\\)|std\\(\\)",features[,2]),]
  x_total <- x_total[,sel_measure[,1]]
  
  #3 Uses descriptive activity names to name the activities in the data set
  colnames(y_total) <- "activity"
  y_total$act_lbl <- factor(y_total$activity, labels=as.character(activity_labels[,2]))
  descriptive_activity <- y_total[-1]
  
  #4 Appropriately labels the data set with descriptive variable names
  
  colnames(x_total) <- features[sel_measure[,1],2]
  
  #5 From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject
  colnames(sub_total) <- "subject"
  total <- cbind(x_total,descriptive_activity,sub_total)
  total_mean <- total %>% group_by(act_lbl,subject) %>% summarize_all(funs(mean))
  write.table(total_mean, file="./UCI HAR Dataset/tidydata.txt",row.names=FALSE,col.names=TRUE)
}
