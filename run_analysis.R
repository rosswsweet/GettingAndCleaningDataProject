library(tidyverse)

# Read in data sets and labels.
test <- read.table("UCI HAR Dataset/test/X_test.txt", header = FALSE)
train <- read.table("UCI HAR Dataset/train/X_train.txt", header = FALSE)
testAct <- read.table("UCI HAR Dataset/test/y_test.txt", header = FALSE)
trainAct <- read.table("UCI HAR Dataset/train/y_train.txt", header = FALSE)
actLabels <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, stringsAsFactors = FALSE)
features <- read.table("UCI HAR Dataset/features.txt", header = FALSE, sep = " ", stringsAsFactors = FALSE)
subjectTest <- read.table("UCI HAR Dataset/test/subject_test.txt", header = FALSE)
subjectTrain <- read.table("UCI HAR Dataset/train/subject_train.txt", header = FALSE)

# Define column names.
colnames(trainAct) <- c("activity")
colnames(testAct) <- c("activity")
colnames(subjectTest) <- c("subject")
colnames(subjectTrain) <- c("subject")
colnames(test) <- features[,2]
colnames(train) <- features[,2]

# Bind by column the subject id, activity, and measurements.
trainLabeled <- cbind(subjectTrain, trainAct, train)
testLabeled <- cbind(subjectTest, testAct, test)

# Bind test and train sets by row.
allData <- rbind(trainLabeled, testLabeled)

# Remove duplicate columns.
allDataUnique <- allData[, !duplicated(colnames(allData))]

# Convert activity to factor with lables, subject to factor, and standardize
# variable names.
allDataSubset <- allDataUnique %>% 
  select(subject, activity, matches("mean\\(\\)|std\\(\\)")) %>%
  mutate(activity = factor(activity, labels = actLabels[, 2])) %>%
  mutate(subject = factor(subject)) %>%
  rename_all(funs(str_replace_all(.,"\\(\\)",""))) %>%
  rename_all(funs(str_replace_all(.,"mean","Mean"))) %>%
  rename_all(funs(str_replace_all(.,"std","Std"))) %>%
  rename_all(funs(str_replace_all(.,"-","")))

# Group data set by subject and activity.
allDataSubGrouped <- group_by(allDataSubset, subject, activity)

# Compute means of grouped data.
DataMeans <- summarize_all(allDataSubGrouped, mean)

# Write summarized data frame to text file.
write.table(DataMeans, file = "DataMeans.txt", row.names = FALSE)



    