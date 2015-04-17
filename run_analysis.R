setwd("~/coursera/Get-and-clean/GAC-course-project/SP_Accel/UCI HAR Dataset")
basedir <- getwd()

##Get a list of directory paths
dirs <- list.dirs()

library(dplyr)

##read in activity labels and features
act.labels <- read.table("activity_labels.txt")
features <- read.table("features.txt")

#read in test dataset
setwd(basedir)
setwd(dirs[2])

fileList <- list.files()
subj_test <- read.table(fileList[2])
x_test <- read.table(fileList[3])
act_test <- read.table(fileList[4])

names(x_test) <- features[,2]
names(subj_test) <- "subject"
names(act_test) <- "Activity"

#give the activities descriptive names
act_test[,1] <- act.labels[act_test[,1],2]

##Combine TEST Data
test <- cbind(subj_test,act_test,x_test)

## Filter to only mean and std
mean <- grep("mean\\(\\)$",names(test))
std <- grep("std\\(\\)$",names(test))
test.filter <- test[,c(1,2,mean,std)]


#read in train dataset
setwd(basedir)
setwd(dirs[4])

fileList <- list.files()
subj_train <- read.table(fileList[2])
x_train <- read.table(fileList[3])
act_train <- read.table(fileList[4])

names(x_train) <- features[,2]
names(subj_train) <- "subject"
names(act_train) <- "Activity"

#give the activities descriptive names
act_train[,1] <- act.labels[act_train[,1],2]


##Combine TRAIN Data
train <- cbind(subj_train,act_train,x_train)

## Filter to only mean and std
train.filter <- train[,c(1,2,mean,std)]

##Combine data into one list
Total.filter <- rbind(train.filter,test.filter)

#clean up the Variable Names
names(Total.filter) <- gsub("-std\\(\\)","STD",names(Total.filter))
names(Total.filter) <- gsub("-mean\\(\\)","MEAN",names(Total.filter))

TF <- tbl_df(Total.filter)
Tidy <- TF %>%
  group_by(subject,Activity) %>%
  summarize(tBodyAccMagMEAN = mean(tBodyAccMagMEAN),
            tGravityAccMagMEAN = mean(tGravityAccMagMEAN),
            tBodyAccJerkMagMEAN = mean(tBodyAccJerkMagMEAN),
            tBodyGyroMagMEAN = mean(tBodyGyroMagMEAN),
            tBodyGyroJerkMagMEAN = mean(tBodyGyroJerkMagMEAN),
            fBodyAccMagMEAN = mean(fBodyAccMagMEAN),
            fBodyBodyAccJerkMagMEAN = mean(fBodyBodyAccJerkMagMEAN),
            fBodyBodyGyroMagMEAN = mean(fBodyBodyGyroMagMEAN),
            fBodyBodyGyroJerkMagMEAN = mean(fBodyBodyGyroJerkMagMEAN),
            tBodyAccMagSTD = mean(tBodyAccMagSTD),
            tGravityAccMagSTD = mean(tGravityAccMagSTD),
            tBodyAccJerkMagSTD = mean(tBodyAccJerkMagSTD),
            tBodyGyroMagSTD = mean(tBodyGyroMagSTD),
            tBodyGyroJerkMagSTD = mean(tBodyGyroJerkMagSTD),
            fBodyAccMagSTD = mean(tBodyGyroJerkMagSTD),
            fBodyBodyAccJerkMagSTD = mean(fBodyBodyAccJerkMagSTD),
            fBodyBodyGyroMagSTD = mean(fBodyBodyGyroMagSTD),
            fBodyBodyGyroJerkMagSTD = mean(fBodyBodyGyroJerkMagSTD))

setwd("~/coursera/Get-and-clean/GAC-course-project/")
write.table(Tidy, file = "Tidy.txt", row.name = F)