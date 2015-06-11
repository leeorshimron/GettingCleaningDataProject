# GettingCleaningDataProject

This document describes the script for the Getting and Cleaning Data course project. 

First, I read the X_test, y_test, and subject_test files into R
X_test <- read.table("~/Desktop/R/UCI HAR Dataset/test/X_test.txt", quote="\"")
y_test <- read.table("~/Desktop/R/UCI HAR Dataset/test/y_test.txt", quote="\"")

subject_test <- read.table("~/Desktop/R/UCI HAR Dataset/test/subject_test.txt", quote="\"")
#dim(X_test) # yields 2947 rows and 561 columns
#dim(y_test) # yields 2947 rows and 1 column
# combine y_test and X_test, side by side, by columns
x_y_test <- cbind(subject_test, y_test,X_test)
#change first and third headers so that there are no duplicates
names(x_y_test)[3] = "V1.2"
names(x_y_test)[1] = "Subject"
# change the first column to corresponding activity labels
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 1, 'WALKING',V1))
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 2, 'WALKING_UPSTAIRS',V1))
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 3, 'WALKING_DOWNSTAIRS',V1))
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 4, 'SITTING',V1))
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 5, 'STANDING',V1))
x_y_test <- x_y_test %>% mutate(V1 = ifelse(V1 == 6, 'LAYING',V1))

X_train <- read.table("~/Desktop/R/UCI HAR Dataset/train/X_train.txt", quote="\"")
y_train <- read.table("~/Desktop/R/UCI HAR Dataset/train/y_train.txt", quote="\"")
subject_train <- read.table("~/Desktop/R/UCI HAR Dataset/train/subject_train.txt", quote="\"")
x_y_train <- cbind(subject_train, y_train, X_train)
#change first and third headers so that there are no duplicates
names(x_y_train)[3] = "V1.2"
names(x_y_train)[1] = "Subject"
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 1, 'WALKING',V1))
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 2, 'WALKING_UPSTAIRS',V1))
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 3, 'WALKING_DOWNSTAIRS',V1))
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 4, 'SITTING',V1))
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 5, 'STANDING',V1))
x_y_train <- x_y_train %>% mutate(V1 = ifelse(V1 == 6, 'LAYING',V1))

# Use rbind to place x_y_test dataset above x_y_train dataset (merge)
x_y_dataset <- rbind(x_y_test,x_y_train)

#Change second column header to descriptor "Activity"
names(x_y_dataset)[2] = "Activity"

#Import features to be used as headers
features <- read.table("~/Desktop/R/UCI HAR Dataset/features.txt", quote="\"")
#Convert features into character strings
featurenames <- as.character(features$V2)
#Change the dataset headers to the corresponding feature names (descriptive names)
names(x_y_dataset)[3:length(names(x_y_dataset))] <- featurenames
#Extract only the column headers that have either mean or standard deviation
x_y_dataset <- x_y_dataset[,grep('mean[^F]|std|Subject|Activity',names(x_y_dataset))]
#Store subsetted dataset headers into new variable newfeaturenames (without activity or subject)
newfeaturenames <- featurenames[grep('mean[^F]|std|Subject|Activity',featurenames)]

#Transform dataset to show mean of variables by Subject and Activity (task 5)
datasetMelt <- melt(x_y_dataset,id=c("Subject","Activity"),measure.vars=(newfeaturenames))
#Average of each activity for each subject
summary <- dcast(datasetMelt, Subject + Activity ~ variable, mean)
#Average across all activities for each subject
summary2 <- dcast(datasetMelt, Subject ~ variable, mean)