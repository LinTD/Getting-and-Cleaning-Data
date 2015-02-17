
setwd("E:\\Coursera\\Getting and Cleaning Data\\project\\work")

# 1. Merge the training and the test sets to create one data set.
#################################################################################################(1)#####
# Read in the data of features(variables) and activityType
features      <- read.table('features.txt',header=FALSE); #imports features (varibles name)
activityType  <- read.table('activity_labels.txt',header=FALSE); #imports activity and its label number

# Read in the data of training set
subject_Train <- read.table('./train/subject_train.txt',header=FALSE); #imports subjects (subject:1~30)
x_Train     <- read.table('./train/x_train.txt',header=FALSE); #imports dataset
y_Train     <- read.table('./train/y_train.txt',header=FALSE); #imports dataset of activity label number

# Assigin column names to the training data imported above
colnames(activityType)   <- c('activityId','activityType');
colnames(subject_Train)  <- "subjectId";
colnames(x_Train)        <- features[,2]; 
colnames(y_Train)        <- "activityId";

# Create the final training set 
trainingData = cbind(y_Train,subject_Train,x_Train);


# Read in the data of test set
subject_Test <- read.table('./test/subject_test.txt',header=FALSE); #imports subjects (subject:1~30)
x_Test       <- read.table('./test/x_test.txt',header=FALSE); #imports dataset
y_Test       <- read.table('./test/y_test.txt',header=FALSE); #imports dataset of activity label number

# Assign column names to the test data imported above
colnames(subject_Test)  <- "subjectId";
colnames(x_Test)        <- features[,2]; 
colnames(y_Test)        <- "activityId";

# Create the final test set
testData = cbind(y_Test,subject_Test,x_Test);


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData)


#2.Extract only the measurements on the mean and standard deviation for each measurement
#################################################################################################(2)#####
colNames<- colnames(finalData)

logic<-(grepl("activity..",colNames) | 
        grepl("subject..",colNames) | 
        grepl("-mean..",colNames) & !grepl("-meanFreq..",colNames) |
        grepl("-std..",colNames))

finalData2<- finalData[logic==TRUE]
names(finalData2)


#3.Use descriptive activity names to name the activities in the data set
#################################################################################################(3)#####

finalData3<- merge(finalData2,activityType,by='activityId',all.x=TRUE);
head(finalData3)



#4.Appropriately labels the data set with descriptive names.
#################################################################################################(4)#####
colNames2<- colnames(finalData3)

for (i in 1:length(colNames2)) 
{
  colNames2[i] = gsub("\\()","",colNames2[i])
  colNames2[i] = gsub("-std","-StdDev",colNames2[i])
  colNames2[i] = gsub("-mean","-Mean",colNames2[i])
  colNames2[i] = gsub("^t","time",colNames2[i])
  colNames2[i] = gsub("^f","freq",colNames2[i])
  colNames2[i] = gsub("([Gg]ravity)","Gravity",colNames2[i])
  colNames2[i] = gsub("([Bb]ody[Bb]ody|[Bb]ody)","Body",colNames2[i])
  colNames2[i] = gsub("[Gg]yro","Gyro",colNames2[i])
  colNames2[i] = gsub("Mag","Magnitude",colNames2[i])
};

colNames2

# Assigning the new descriptive column names to the finalData2 set
colnames(finalData3) <- colNames2;

names(finalData3)


#5. Create an second independent tidy data set with the average of each variable for each activity 
#and each subject. 
################################################################################################(5)#####
# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData[,names(finalData) != 'activityType'];


tidyData1<-finalData3[,-1]

library(plyr)
tidyData2<- ddply(tidyData1, c("subjectId","activityType"), numcolwise(mean))


#another way: aggregate(finalData3[,3:68], list(finalData3$subjectId+finalData3$activityId),mean)



#6. Create the tidy data files in the form of .txt
###############################################################################################(end)#####
write.table(tidyData1, file = "tidy1_merged_labeled.txt")
write.table(tidyData2, file = "tidy2_avg_by_act_sub.txt")

