# ------------------------------------------------------------------------
# Getting and cleaning data course project - Coursera
# The script loads the wearable computing data set, perform a selection
# of the variable (only mean measures), mearges training and test subsets,
# and calculates the mean of all variables by subject and activity.
#
# Date: 13/03/2016
# ------------------------------------------------------------------------

library(dplyr)

# Load all the data from a specific folder. Modify path accordingly.
path <- "~/Desktop/Assignment 4/UCI HAR Dataset/"
features <- read.table(paste0(path,"features.txt"),
                       quote="\"", comment.char="", stringsAsFactors=FALSE)
X_train <- read.table(paste0(path,"train/X_train.txt"),
                      quote="\"", comment.char="", stringsAsFactors=FALSE)
X_test <- read.table(paste0(path,"test/X_test.txt"),
                     quote="\"", comment.char="", stringsAsFactors=FALSE)
y_train <- read.table(paste0(path,"train/y_train.txt"),
                      quote="\"", comment.char="", stringsAsFactors=FALSE)
y_test <- read.table(paste0(path, "test/y_test.txt"), 
                     quote="\"", comment.char="", stringsAsFactors=FALSE)
subject_train <- read.table(paste0(path,"train/subject_train.txt"),
                            quote="\"", comment.char="", stringsAsFactors=FALSE)
subject_test <- read.table(paste0(path,"test/subject_test.txt"),
                           quote="\"", comment.char="", stringsAsFactors=FALSE)
activity_labels <- read.table(paste0(path,"activity_labels.txt"),
                              quote="\"", comment.char="", stringsAsFactors=FALSE)

# Assigns names to variables in both sets
names(X_train) <- features$V2
names(X_test) <- features$V2
# Selects all variables that contain the "std", "mean" and "Mean".
# The last one is to include also the gravityMean, tBodyAccMean, etc... Variables.
X_train <- X_train[,grepl("[Mm]ean|std", names(X_train))]
X_test <- X_test[,grepl("[Mm]ean|std", names(X_test))]

# Renaming additional variables
names(y_train) <- "ID_activity"
names(y_test) <- "ID_activity"
names(activity_labels) <- c("ID_activity", "Activity")
names(subject_train) <- "Subject_ID"
names(subject_test) <- "Subject_ID"

# Combine subject ids, activity ids and union training and test datasets.
# Then extract averages for all the variables.
data <- rbind(
        cbind(subject_train,
              Activity = merge(y_train, activity_labels)$Activity, # This lines gives activities readable labels
              X_train),
        cbind(subject_test,
              Activity = merge(y_test, activity_labels)$Activity,
              X_test)
                                   ) %>% 
      group_by(Subject_ID, Activity) %>%
      summarise_each(funs(mean)) 

# Cleaning up variables names
## Since the variables represent averages by subject and activity, the names need to be
## alter to reflect that. The prefix "Avg_" is added to all variables names.
var_names <- names(data)[3:length(names(data))]
var_names <- paste0("Avg_", var_names)
names(data) <- c(names(data)[1:2], var_names)

# Save tidy dataset
write.table(data, "Cleaned_data.txt", sep = "\t", row.names = F)



