install.packages("plyr")
install.packages("reshape2")
library(plyr)
library(reshape2)
setwd ("/Users/lab/Desktop/Coursera/Getting and Cleaning the Data/UCI HAR Dataset")

# load files
d.test<-read.table("test/X_test.txt")
d.train<-read.table("train/X_train.txt")
d.ytest<-read.table("test/y_test.txt")
d.ytrain<-read.table("y_train.txt")
subject.train <- read.table("train/subject_train.txt")
subject.test <- read.table("test/subject_test.txt")
features <- read.table("features.txt",stringsAsFactors=FALSE)
activity.labels <- read.table("activity_labels.txt",stringsAsFactors=FALSE)
head(d.test)

# merge test and train set
d.all<-rbind(d.test, d.train)
subj <- rbind(subject.test,subject.train)
activity <- rbind(d.ytest, d.ytrain)

colnames(d.all) <- features$V2
#d.all<-cbind(d.all, subj, activity)
head(d.all)

#Extract only the measurements on the mean and standard deviation for each measurement
d.subset <- d.all[,grepl('mean\\(\\)|std\\(\\)',colnames(d.all))]
d.subset$subject <- subj
colnames(d.subset$subject) <- "Subject"
d.subset$activity <- act
colnames(d.subset$activity) <- "Activity"

head(d.subset)

#Uses descriptive activity names to name the activities in the data set

d.subset$activity_label = factor(d.subset$activity[[1]],labels=activity.labels$V2)
d.subset$activity = factor(d.subset$activity[[1]],labels=activity.labels$V2)

#Appropriately labels the data set with descriptive variable names
names(d.subset) <- gsub(pattern="^t",replacement="time",x=names(d.subset))
names(d.subset) <- gsub(pattern="^f",replacement="freq",x=names(d.subset))
names(d.subset) <- gsub(pattern="-?mean[(][)]-?",replacement="mean",x=names(d.subset))
names(d.subset) <- gsub(pattern="-?std[()][)]-?",replacement="std",x=names(d.subset))
names(d.subset) <- gsub(pattern="-?Freq[()][)]-?",replacement="freq",x=names(d.subset))
names(d.subset)
write.table(d.subset, file="d_subset.txt")

# Creates an independent tidy data set with the average of each variable for each activity and each subject.

tidy.frame = data.frame()

subjects = sort( unique(d.subset$subject[[1]]) )
activities = sort( unique(d.subset$activity) )

for (subj in subjects) {
  for (act in activities) {
    s1 = d.subset[ d.subset$subject==subj & d.subset$activity == act, ]
    d.tidy= as.data.frame( lapply( s1[,1:66], FUN=mean ) )
    d.tidy$subject = subj
    d.tidy$activity = act
    tidy.frame = rbind(tidy.frame, d.tidy)
  }
}

write.table(tidy.frame, file="d_tidy.txt",row.name=FALSE)
