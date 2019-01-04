#1. Reading Data

#read training set 
X_train <- read.table("~/Documents/Data Science/UCI HAR Dataset/train/X_train.txt", quote="\"", comment.char="")
y_train <- read.table("~/Documents/Data Science/UCI HAR Dataset/train/y_train.txt", quote="\"", comment.char="")
subject_train <- read.table("~/Documents/Data Science/UCI HAR Dataset/train/subject_train.txt", quote="\"", comment.char="")

colnames(subject_train) = c("subject")
colnames(y_train) = c("activity")

#read training set 
X_test <- read.table("~/Documents/Data Science/UCI HAR Dataset/test/X_test.txt", quote="\"", comment.char="")
y_test <- read.table("~/Documents/Data Science/UCI HAR Dataset/test/y_test.txt", quote="\"", comment.char="")
subject_test <- read.table("~/Documents/Data Science/UCI HAR Dataset/test/subject_test.txt", quote="\"", comment.char="")

colnames(subject_test) = c("subject")
colnames(y_test) = c("activity")

#read the rest of the data
features <- read.table("~/Documents/Data Science/UCI HAR Dataset/features.txt", quote="\"", comment.char="", stringsAsFactors=FALSE)
activity_labels <- read.table("~/Documents/Data Science/UCI HAR Dataset/activity_labels.txt", quote="\"", comment.char="")

colnames(features) = c("no.", "feature")

#2. Merging Data
activityData <- rbind(
  cbind(subject_train, X_train, y_train),
  cbind(subject_test, X_test, y_test)
)

colnames(activityData) <- c("subject", features$feature, "activity")

#3. Extracting mean and standard deviation
# We want to keep the subject and activity, and the columns that correspond to the mean and std

keeps <- grepl("subject|activity|mean|std", colnames(activityData))
activityDataReduced <- activityData[, keeps]

#4. Label activity names
activityNumber = activity_labels$V1
activityName = activity_labels$V2


activityDataReduced$activity <- factor(activityDataReduced$activity, 
                                 levels = activityNumber , labels = activityName)

#5. Label dataset variables
featuresReduced = colnames(activityDataReduced) 

featuresReduced <- gsub("[\\(\\)-]", "", featuresReduced)
featuresReduced <- gsub("^f", "Freq ", featuresReduced)
featuresReduced <- gsub("^t", "Time ", featuresReduced)
featuresReduced <- gsub("Acc", " Accelerometer ", featuresReduced)
featuresReduced <- gsub("Gyro", " Gyroscope ", featuresReduced)
featuresReduced <- gsub("Mag", "Magnitude ", featuresReduced)
featuresReduced <- gsub("Freq", "Frequency ", featuresReduced)
featuresReduced <- gsub("mean", "Mean ", featuresReduced)
featuresReduced <- gsub("std", "StandardDeviation ", featuresReduced)
featuresReduced <- gsub("BodyBody", "Body", featuresReduced)

colnames(activityDataReduced) <- featuresReduced

#6. Create tidy dataset
library(dplyr)
activityMeans <- activityDataReduced %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

