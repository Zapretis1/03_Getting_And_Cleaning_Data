## This R script called run_analysis.R does the following:
## 0) Loads the data into R
## 1) Merges the training and the test sets to create one data set.
## 2) Extracts only the measurements on the mean and standard deviation for each measurement.
## 3) Uses descriptive activity names to name the activities in the data set
## 4) Appropriately labels the data set with descriptive variable names.
## 5) From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.
## 6) Writes the independent tidy data set in a text file

## Custom parameters
data_dir_name <- "03_week4_peer_graded_assignment_zapretis1" #name of the directory to store the data
data_zip_name <- "UCI HAR Dataset.zip" #name of the zip file
data_unzip_name <- "UCI HAR Dataset" #name of the unzip location
script_name <- "run_anlysis.R"
data_tidy_name <- "averaged_data.txt"

## Location of the unzipped directory with data
my_destunzip <- paste(".", data_dir_name, data_unzip_name, sep="/")
## Location of the output file with new data
my_desttidy <- paste(".", data_dir_name, data_tidy_name, sep="/")

## Prerequisites
## Checks if package "dplyr" is installed and proceeds accordingly
if("dplyr" %in% rownames(installed.packages()) == FALSE) {install.packages("dplyr")}

## Checks if package "tidyr" is installed and proceeds accordingly
if("tidyr" %in% rownames(installed.packages()) == FALSE) {install.packages("tidyr")}

## Load relevant libraries
library(dplyr)
library(tidyr)

## 0) Loads the data into R
print("0) Loads the data into R")
activity_labels <- read.table(paste(".", my_destunzip, "activity_labels.txt", sep="/"))
features <- read.table(paste(".", my_destunzip, "features.txt", sep="/"))

subject_test <- read.table(paste(".", my_destunzip, "test/subject_test.txt", sep="/"))
X_test <- read.table(paste(".", my_destunzip, "test/X_test.txt", sep="/"))
y_test <- read.table(paste(".", my_destunzip, "test/y_test.txt", sep="/"))

subject_train <- read.table(paste(".", my_destunzip, "train/subject_train.txt", sep="/"))
X_train <- read.table(paste(".", my_destunzip, "train/X_train.txt", sep="/"))
y_train <- read.table(paste(".", my_destunzip, "train/y_train.txt", sep="/"))

## Assign names to the columns of test and training data with the features' list
## present in the features.txt file
names(X_test) <- features[,2]
names(X_train) <- features[,2]

## Assign appropriate names for the subject data and activity label data
names(subject_test) <- "subjectid"
names(subject_train) <- "subjectid"
names(y_test) <- "activitylabel"
names(y_train) <- "activitylabel"

## Creates new training and test data sets identifying the subject and activity 
## associated with each measurement (each entry corresponding to one row)
data_test <- cbind(subject_test, y_test, X_test)
data_train <- cbind(subject_train, y_train, X_train)

## 1) Merges the training and the test sets to create one data set.
print("1) Merges the training and the test sets to create one data set.")
data_merged <- rbind(data_test, data_train)

## 2) Extracts only the measurements on the mean and standard deviation for each measurement.
## Only -mean() and -meanFreq() measurements are extracted.
## Ultimate features corresponding to angle values where variables suffixed with
## "Mean" appear, are excluded from the extraction.
## They represent values computed based on means of gravity, gyro values.
print("2) Extracts only the measurements on the mean and standard deviation")
mean_idx <- grep("*-mean*",names(data_merged))

# This is a checking line
#if (!length(names(data_merged[,mean_idx]))==46) {stop("mean extraction failed")}

## All columns labelled with -std() are extracted
std_idx <- grep("*-std*",names(data_merged))

# This is a checking line
#if (!length(names(data_merged[,std_idx]))==33) {stop("std extraction failed")}

## Takes the union of indices to be extracted and adds the first and second
## column which represent subject_id and activity_label that we want to keep
extract_idx <- c(1:2, union(mean_idx,std_idx))
data_extracted <- data_merged[, extract_idx]

## 3) Uses descriptive activity names to name the activities in the data set
print("3) Uses descriptive activity names to name the activities in the data set")
data_extracted[,2] <- sapply(data_extracted[,2], function(x) activity_labels[x,2] )

## 4) Appropriately labels the data set with descriptive variable names.
## Make all names lower case
print("4) Appropriately labels the data set with descriptive variable names")
names(data_extracted) <- tolower(names(data_extracted))

## Parentheses of the names are removed because they cause issues in the next step
names(data_extracted) <- gsub("\\()", "", names(data_extracted))

## Remove also "-"
names(data_extracted) <- gsub("-", "", names(data_extracted))

## Anticipating the next action, prepend each name with "a" for average since
## in the next step we will take the mean for each activity and each subject
names(data_extracted)[3:81] <- paste("a", names(data_extracted[,3:81]), sep = "")

## 5) From the data set in step 4, creates a second, independent tidy data set
##    with the average of each variable for each activity and each subject.
print("5) Creates independent tidy data set")
data_averaged <-
    data_extracted %>%
    group_by(subjectid, activitylabel) %>%
    summarise(
        atbodyaccmeanx = mean(atbodyaccmeanx),
        atbodyaccmeany = mean(atbodyaccmeany),
        atbodyaccmeanz = mean(atbodyaccmeanz),
        atgravityaccmeanx = mean(atgravityaccmeanx),
        atgravityaccmeany = mean(atgravityaccmeany),
        atgravityaccmeanz = mean(atgravityaccmeanz),
        atbodyaccjerkmeanx = mean(atbodyaccjerkmeanx),
        atbodyaccjerkmeany = mean(atbodyaccjerkmeany),
        atbodyaccjerkmeanz = mean(atbodyaccjerkmeanz),
        atbodygyromeanx = mean(atbodygyromeanx),
        atbodygyromeany = mean(atbodygyromeany),
        atbodygyromeanz = mean(atbodygyromeanz),
        atbodygyrojerkmeanx = mean(atbodygyrojerkmeanx),
        atbodygyrojerkmeany = mean(atbodygyrojerkmeany),
        atbodygyrojerkmeanz = mean(atbodygyrojerkmeanz),
        atbodyaccmagmean = mean(atbodyaccmagmean),
        atgravityaccmagmean = mean(atgravityaccmagmean),
        atbodyaccjerkmagmean = mean(atbodyaccjerkmagmean),
        atbodygyromagmean = mean(atbodygyromagmean),
        atbodygyrojerkmagmean = mean(atbodygyrojerkmagmean),
        afbodyaccmeanx = mean(afbodyaccmeanx),
        afbodyaccmeany = mean(afbodyaccmeany),
        afbodyaccmeanz = mean(afbodyaccmeanz),
        afbodyaccmeanfreqx = mean(afbodyaccmeanfreqx),
        afbodyaccmeanfreqy = mean(afbodyaccmeanfreqy),
        afbodyaccmeanfreqz = mean(afbodyaccmeanfreqz),
        afbodyaccjerkmeanx = mean(afbodyaccjerkmeanx),
        afbodyaccjerkmeany = mean(afbodyaccjerkmeany),
        afbodyaccjerkmeanz = mean(afbodyaccjerkmeanz),
        afbodyaccjerkmeanfreqx = mean(afbodyaccjerkmeanfreqx),
        afbodyaccjerkmeanfreqy = mean(afbodyaccjerkmeanfreqy),
        afbodyaccjerkmeanfreqz = mean(afbodyaccjerkmeanfreqz),
        afbodygyromeanx = mean(afbodygyromeanx),
        afbodygyromeany = mean(afbodygyromeany),
        afbodygyromeanz = mean(afbodygyromeanz),
        afbodygyromeanfreqx = mean(afbodygyromeanfreqx),
        afbodygyromeanfreqy = mean(afbodygyromeanfreqy),
        afbodygyromeanfreqz = mean(afbodygyromeanfreqz),
        afbodyaccmagmean = mean(afbodyaccmagmean),
        afbodyaccmagmeanfreq = mean(afbodyaccmagmeanfreq),
        afbodybodyaccjerkmagmean = mean(afbodybodyaccjerkmagmean),
        afbodybodyaccjerkmagmeanfreq = mean(afbodybodyaccjerkmagmeanfreq),
        afbodybodygyromagmean = mean(afbodybodygyromagmean),
        afbodybodygyromagmeanfreq = mean(afbodybodygyromagmeanfreq),
        afbodybodygyrojerkmagmean = mean(afbodybodygyrojerkmagmean),
        afbodybodygyrojerkmagmeanfreq = mean(afbodybodygyrojerkmagmeanfreq),
        atbodyaccstdx = mean(atbodyaccstdx),
        atbodyaccstdy = mean(atbodyaccstdy),
        atbodyaccstdz = mean(atbodyaccstdz),
        atgravityaccstdx = mean(atgravityaccstdx),
        atgravityaccstdy = mean(atgravityaccstdy),
        atgravityaccstdz = mean(atgravityaccstdz),
        atbodyaccjerkstdx = mean(atbodyaccjerkstdx),
        atbodyaccjerkstdy = mean(atbodyaccjerkstdy),
        atbodyaccjerkstdz = mean(atbodyaccjerkstdz),
        atbodygyrostdx = mean(atbodygyrostdx),
        atbodygyrostdy = mean(atbodygyrostdy),
        atbodygyrostdz = mean(atbodygyrostdz),
        atbodygyrojerkstdx = mean(atbodygyrojerkstdx),
        atbodygyrojerkstdy = mean(atbodygyrojerkstdy),
        atbodygyrojerkstdz = mean(atbodygyrojerkstdz),
        atbodyaccmagstd = mean(atbodyaccmagstd),
        atgravityaccmagstd = mean(atgravityaccmagstd),
        atbodyaccjerkmagstd = mean(atbodyaccjerkmagstd),
        atbodygyromagstd = mean(atbodygyromagstd),
        atbodygyrojerkmagstd = mean(atbodygyrojerkmagstd),
        afbodyaccstdx = mean(afbodyaccstdx),
        afbodyaccstdy = mean(afbodyaccstdy),
        afbodyaccstdz = mean(afbodyaccstdz),
        afbodyaccjerkstdx = mean(afbodyaccjerkstdx),
        afbodyaccjerkstdy = mean(afbodyaccjerkstdy),
        afbodyaccjerkstdz = mean(afbodyaccjerkstdz),
        afbodygyrostdx = mean(afbodygyrostdx),
        afbodygyrostdy = mean(afbodygyrostdy),
        afbodygyrostdz = mean(afbodygyrostdz),
        afbodyaccmagstd = mean(afbodyaccmagstd),
        afbodybodyaccjerkmagstd = mean(afbodybodyaccjerkmagstd),
        afbodybodygyromagstd = mean(afbodybodygyromagstd),
        afbodybodygyrojerkmagstd = mean(afbodybodygyrojerkmagstd)
    )

## 6) Writes the independent tidy data set in a text file
print("6) Writes the independent tidy data set in a text file")

## Truncating function
trunc <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec;
## Truncate long values from the data
data_averaged[,3:81] <- lapply(data_averaged[,3:81], function(x) trunc(x, prec = 5))
data_write <- data_averaged

## Export created data to text file
write.table(data_averaged,my_desttidy, fileEncoding = "UTF-8")

## Check that the produced data when loaded into R matches the exported data
data_read<- read.table(my_desttidy, fileEncoding = "UTF-8")
if(prod(data_read==data_write)){print("Reloaded data matches created data")}

##EOF