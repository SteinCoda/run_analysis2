run_analysis <- function(){
        library(dplyr)
        
        x_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
        y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")
        x_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
        y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")
        features <- read.table("./UCI HAR Dataset/features.txt")
       
        ## step 1 Merges the training and the test sets to create one data set
        merge_x <- rbind(x_test, x_train)
        merge_y <- rbind(y_test, y_train)
        colnames(merge_x) <- features[, 2]
        
        ## step2 Extracts only the measurements on the mean and standard deviation for each measurement
        merge_x_mean <- merge_x[, grep("mean", colnames(merge_x))]
        merge_x_std  <- merge_x[, grep("std" , colnames(merge_x))]
        merge_x_mean_std <- cbind(merge_x_mean, merge_x_std)
        
        merge_yx_mean_std <- cbind(merge_y, merge_x_mean_std)
        colnames(merge_yx_mean_std) <- c("activity", colnames(merge_yx_mean_std)[-1]) ## name first column "activity"
        merge_yx_mean_std_ordered <- arrange(merge_yx_mean_std, activity)
        
        ## step 3 Uses descriptive activity names to name the activities in the data set
        for(i in 1:6){
                if(i == 1){
                        activity_names <- sub("1", "WALKING", merge_yx_mean_std_ordered[, 1])
                }
                if(i == 2){
                        activity_names <- sub("2", "WALKING_UPSTAIRS", merge_yx_mean_std_ordered[, 1])
                }
                if(i == 3){
                        activity_names <- sub("3", "WALKING_DOWNSTAIRS", merge_yx_mean_std_ordered[, 1])
                }
                if(i == 4){
                        activity_names <- sub("4", "SITTING", merge_yx_mean_std_ordered[, 1])
                }
                if(i == 5){
                        activity_names <- sub("5", "STANDING", merge_yx_mean_std_ordered[, 1])
                }
                if(i == 6){
                        activity_names <- sub("6", "LAYING", merge_yx_mean_std_ordered[, 1])
                }
                merge_yx_mean_std_ordered[, 1] <- activity_names
        }
                
        data_wiht_activity_names <- merge_yx_mean_std_ordered
        
        ## step 4 Appropriately labels the data set with descriptive variable names
        colnames <- colnames(data_wiht_activity_names)
        colnames <- gsub("-", "_", colnames)
        colnames <- gsub('\\(', '', colnames)
        colnames <- gsub(')', '', colnames)
        colnames <- gsub("Jerk", "_Jerk", colnames)
        colnames <- gsub("Acc", "_Acc", colnames)
        colnames <- gsub("Body", "_Body", colnames)
        colnames <- gsub("Gravity", "_Gravity", colnames)
        colnames <- gsub("Gyro", "_Gyro", colnames)
        colnames <- gsub("Mag", "_Mag", colnames)
        
        colnames(data_wiht_activity_names) <- colnames
        data_wiht_variable_names <- data_wiht_activity_names
        
        ## step 5 From the data set in step 4, creates a second, independent tidy data 
        ##set with the average of each variable for each activity and each subject
        
        activity <- c("WALKING", "WALKING_UPSTAIRS","WALKING_DOWNSTAIRS","SITTING","STANDING", "LAYING")
        tidy_data <- data.frame()
        
        for(i in 1:6){
                ## subset specific activity name and 
                ## remove the fist column which are activites labels ,using [, -1]
                sub_dataframe <- data_wiht_variable_names[data_wiht_variable_names$activity == activity[i], -1] 
                
                ## caculate means of each column of specific activity
                ## and save as data.frame
                sub_dataframe_mean <- as.data.frame(apply(sub_dataframe, 2, mean))
                
                ## join them together 
                if(i == 1){
                        tidy_data <- sub_dataframe_mean
                }
                else{
                        tidy_data <- cbind(tidy_data, sub_dataframe_mean)
                }
        }
        
        ## reset the column names of tidy_data
        colnames(tidy_data) <- activity
        
        ## save tidy_data as txt file
        write.table(tidy_data, file = "./run_analysis.txt", row.names = FALSE)
}

