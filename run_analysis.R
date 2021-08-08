library(tidyverse)


# 1. Merges the training and the test sets to create one data set.
features <- read.table("UCI HAR Dataset\\features.txt", col.names = c("n","features"))
activity_labels <- read.table("UCI HAR Dataset\\activity_labels.txt", col.names = c("label", "activity"))

X_test <- read.table("UCI HAR Dataset\\test\\X_test.txt", col.names = features$features)
y_test <- read.table("UCI HAR Dataset\\test\\y_test.txt", col.names = "label")
subject_test <- read.table("UCI HAR Dataset\\test\\subject_test.txt", col.names = "subject")

X_train <- read.table("UCI HAR Dataset\\train\\X_train.txt", col.names = features$features)
y_train <- read.table("UCI HAR Dataset\\train\\y_train.txt",col.names = "label")
subject_train <- read.table("UCI HAR Dataset\\train\\subject_train.txt",col.names = "subject")

x <- rbind(X_test,X_train)
y <- rbind(y_test, y_train)
subject <- rbind(subject_test,subject_train )

merged_data <- cbind(subject, y, x)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
data2 <- merged_data %>% 
        select(subject, label, contains("mean"), contains("std"))

# 3. Uses descriptive activity names to name the activities in the data set
data2$label <- activity_labels[data2$label,2]

# 4. Appropriately labels the data set with descriptive variable names.
names(data2)
name <- names(data2)
name[2] <- "activity"
name <- str_replace_all(string = name, pattern = "Acc", replacement = "Accelerometer")
name <- str_replace_all(string = name, pattern = "Gyro", replacement = "Gyroscope")
name <- str_replace_all(string = name, pattern = "BodyBody", replacement = "Body")
name <- str_replace_all(string = name, pattern = "Mag", replacement = "Magnitude")
name <- str_replace_all(string = name, pattern = "^t", replacement = "Time")
name <- str_replace_all(string = name, pattern = "^f", replacement = "Frequency")
name <- str_replace_all(string = name, pattern = "tBody", replacement = "TimeBody")
name <- str_replace_all(string = name, pattern = "angle", replacement = "Angle")
name <- str_replace_all(string = name, pattern = "gravity", replacement = "Gravity")

name

names(data2) <- name


# 5. From the data set in step 4, creates a second, independent tidy data set with
# the average of each variable for each activity and each subject.
data2 <- as_tibble(data2)
data2$subject <- factor(data2$subject)

data5 <- data2 %>% 
        group_by(subject, activity) %>% 
        summarise(across(where(is.numeric), ~ mean(.x, na.rm = TRUE))) %>% 
        ungroup()

str(data5)

write.table(x = data5, file = "step5.txt", row.names = FALSE)

