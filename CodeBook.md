#Data Resource
A full description of the data used in this project can be found at The [UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones).
The data set for this project can be found [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip).

#Data Set Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. 

Check the README.txt file for further details about this dataset. 

#Trainsfomation Details

###1. Merge the training and the test sets to create one data set by using following data files:

- 'features.txt': List of all features (variables/column names).
- 'train/X_train.txt': Training set (data for features).
- 'train/y_train.txt': Training labels (data for activity class labels).
- 'test/X_test.txt': Test set.
- 'test/y_test.txt': Test labels.

###2. Extract only the measurements on the mean and standard deviation for each measurement.

By the 'features_info.txt', we know the variables we interested in are either containing the string as follows:

- std(): Standard deviation
- mad(): Median absolute deviation 

###3. Use descriptive activity names to name the activities in the data set by using:

- 'activity_labels.txt': links the class labels with their activity name.

###4. Appropriately label the data set with descriptive activity names.

By the 'features_info.txt', we know that
- the prefix 't' to denote time domain signals 
- the prefix 'f' to indicate frequency domain signals
- the "Mag" indicate magnitude

Thus, we rename the columns by:
- simplify:  

        from "()" to ""
- replace:   

        from "std" to "StdDev"
        from "mean" to "Mean"
        from "t" to "time"
        from "f" to "freq"
        from "Mag" to "Magnitude"
- unifom: 
 
        from "[Gg]ravity" to "Gravity"
        from "[Gg]yro" to "Gyro"
        from "[Bb]ody)" to "Body"

###5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.
