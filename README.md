# Read Me
## Steps to tidy and analyze the Sensor data

A few steps were taken to transform the initial data set. The test and train sets have were merged and the subject identifiers and activity labels were pulled in to create a single data set. The activity identifiers were translated from identifiers into human-readable names. Only the mean and standard deviation variables were kept. Those variables were further summarized by taking their mean for each subject/activity pair. The data is in "wide" format; there is a single row for each subject/activity pair, and a single column for each measurement.

The final data set can be found in the tidyMeans.txt file, which can be read into R with read.table("tidyMeans.txt", header = TRUE). A detailed description of the variables can be found in CodeBook.md. The basic naming convention is:
Mean{timeOrFreq}{measurement}{meanOrStd}{XYZ}
Where timeOrFreq is either Time or Frequency, indicating whether the measurement comes from the time or frequency domain, measurement is one of the original measurement features, meanOrStd is either Mean or StdDev, indicating whether the measurement was a mean or standard deviation variable, and XYZ is X, Y, or Z, indicating the axis along which the measurement was taken, or nothing, for magnitude measurements.

## Get and extract data
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" 
downloadDir <- "data"  path <- function(...) { paste(..., sep = "/") }  
zipFile <- path(downloadDir, "dataset.zip") 
if(!file.exists(zipFile)) { download.file(url, zipFile, method = "curl") }  
dataDir <- path(downloadDir, "UCI HAR Dataset") 
if(!file.exists(dataDir)) { unzip(zipFile, exdir = downloadDir) }

###1. Merges the training and the test sets to create one data set

#### Merge and add in the feature names:

read <- function(path) { read.table(path(dataDir, path)) }  
if(!exists("XTrain")) { XTrain <- read("train/X_train.txt") } 
if(!exists("XTest"))  { XTest  <- read("test/X_test.txt") } 
merged <- rbind(XTrain, XTest)  
featureNames <- read("features.txt")[, 2] names(merged) <- featureNames

### 2. Extracts only the measurements on the mean and standard deviation for each measurement

#### Limit to columns with feature names matching mean() or std():
matches <- grep("(mean|std)\\(\\)", names(merged)) 
limited <- merged[, matches]

### 3. Uses descriptive activity names to name the activities in the data set

#### Get the activity data and map to nicer names:
yTrain <- read("train/y_train.txt") 
yTest  <- read("test/y_test.txt") 
yMerged <- rbind(yTrain, yTest)[, 1]  
activityNames <-   c("Walking", "Walking Upstairs", "Walking Downstairs", "Sitting", "Standing", "Laying") 
activities <- activityNames[yMerged]

### 4. Appropriately labels the data set with descriptive variable names

#### Change t to Time-, f to Frequency-, mean() to Mean and std() to Standard-Deviation
names(limited) <- gsub("^t", "Time", names(limited)) 
names(limited) <- gsub("^f", "Frequency", names(limited)) 
names(limited) <- gsub("-mean\\(\\)", "Mean", names(limited)) 
names(limited) <- gsub("-std\\(\\)", "StdDev", names(limited)) 
names(limited) <- gsub("-", "", names(limited))
names(limited) <- gsub("BodyBody", "Body", names(limited))

#### Add activities and subject with nice names
subjectTrain <- read("train/subject_train.txt") 
subjectTest  <- read("test/subject_test.txt") 
subjects <- rbind(subjectTrain, subjectTest)[, 1]  
tidy <- cbind(Subject = subjects, Activity = activities, limited)

### 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject

library(plyr) # Column means for all but the subject and activity columns 
limitedColMeans <- function(data) { colMeans(data[,-c(1,2)]) } 
tidyMeans <- ddply(tidy, .(Subject, Activity), limitedColMeans) 
names(tidyMeans)[-c(1,2)] <- paste0("Mean", names(tidyMeans)[-c(1,2)])
Write file
write.table(tidyMeans, "tidyMeans.txt", row.names = FALSE) tidyMeansCheck <- read.table("tidyMeans.txt", header = TRUE)

