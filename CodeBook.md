﻿Nature of variables
===================

There are 81 variables encompassing the id of the subject who performed the activity, the label of the activity and 79 averaged features for each combination of subject and activity.
The number of subjects is 30.
The number of activities is 6.
There are therefore 180 observations.

Feature Selection 
=================

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals). 

These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.

__Only features representing the measurements on the mean and standard deviation have been extracted.__
The average for each subject and each activity has been computed.
__A letter 'a' has been prepended to each variable name to remind that the variables represent an average over sujects and activities.__

List of variables
=================

1.	subjectid
	From 1 to 30
1.	activitylabel
	*	LAYING
	*	SITTING
	*	STANDING
	*	WALKING
	*	WALKING_DOWNSTAIRS
	*	WALKING_UPSTAIRS
1.	atbodyaccmeanx
1.	atbodyaccmeany
1.	atbodyaccmeanz
1.	atgravityaccmeanx
1.	atgravityaccmeany
1.	atgravityaccmeanz
1.	atbodyaccjerkmeanx
1.	atbodyaccjerkmeany
1.	atbodyaccjerkmeanz
1.	atbodygyromeanx
1.	atbodygyromeany
1.	atbodygyromeanz
1.	atbodygyrojerkmeanx
1.	atbodygyrojerkmeany
1.	atbodygyrojerkmeanz
1.	atbodyaccmagmean
1.	atgravityaccmagmean
1.	atbodyaccjerkmagmean
1.	atbodygyromagmean
1.	atbodygyrojerkmagmean
1.	afbodyaccmeanx
1.	afbodyaccmeany
1.	afbodyaccmeanz
1.	afbodyaccmeanfreqx
1.	afbodyaccmeanfreqy
1.	afbodyaccmeanfreqz
1.	afbodyaccjerkmeanx
1.	afbodyaccjerkmeany
1.	afbodyaccjerkmeanz
1.	afbodyaccjerkmeanfreqx
1.	afbodyaccjerkmeanfreqy
1.	afbodyaccjerkmeanfreqz
1.	afbodygyromeanx
1.	afbodygyromeany
1.	afbodygyromeanz
1.	afbodygyromeanfreqx
1.	afbodygyromeanfreqy
1.	afbodygyromeanfreqz
1.	afbodyaccmagmean
1.	afbodyaccmagmeanfreq
1.	afbodybodyaccjerkmagmean
1.	afbodybodyaccjerkmagmeanfreq
1.	afbodybodygyromagmean
1.	afbodybodygyromagmeanfreq
1.	afbodybodygyrojerkmagmean
1.	afbodybodygyrojerkmagmeanfreq
1.	atbodyaccstdx
1.	atbodyaccstdy
1.	atbodyaccstdz
1.	atgravityaccstdx
1.	atgravityaccstdy
1.	atgravityaccstdz
1.	atbodyaccjerkstdx
1.	atbodyaccjerkstdy
1.	atbodyaccjerkstdz
1.	atbodygyrostdx
1.	atbodygyrostdy
1.	atbodygyrostdz
1.	atbodygyrojerkstdx
1.	atbodygyrojerkstdy
1.	atbodygyrojerkstdz
1.	atbodyaccmagstd
1.	atgravityaccmagstd
1.	atbodyaccjerkmagstd
1.	atbodygyromagstd
1.	atbodygyrojerkmagstd
1.	afbodyaccstdx
1.	afbodyaccstdy
1.	afbodyaccstdz
1.	afbodyaccjerkstdx
1.	afbodyaccjerkstdy
1.	afbodyaccjerkstdz
1.	afbodygyrostdx
1.	afbodygyrostdy
1.	afbodygyrostdz
1.	afbodyaccmagstd
1.	afbodybodyaccjerkmagstd
1.	afbodybodygyromagstd
1.	afbodybodygyrojerkmagstd

See the `features_info.txt` of the original data for detailed explanation of what each feature represents and how it is computed.
See also the original README.txt provided in the zipped file with the original data.

==================================================================
Human Activity Recognition Using Smartphones Dataset
Version 1.0
==================================================================
Jorge L. Reyes-Ortiz, Davide Anguita, Alessandro Ghio, Luca Oneto.
Smartlab - Non Linear Complex Systems Laboratory
DITEN - Università degli Studi di Genova.
Via Opera Pia 11A, I-16145, Genoa, Italy.
activityrecognition@smartlab.ws
www.smartlab.ws
==================================================================

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 


Notes
=====
- Features are normalized and bounded within [-1,1].
- Each feature vector is a row on the text file.

For more information about this dataset contact: activityrecognition@smartlab.ws

License
=======
Use of this dataset in publications must be acknowledged by referencing the following publication [1] 

[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012

This dataset is distributed AS-IS and no responsibility implied or explicit can be addressed to the authors or their institutions for its use or misuse. Any commercial use is prohibited.

Jorge L. Reyes-Ortiz, Alessandro Ghio, Luca Oneto, Davide Anguita. November 2012.

[code book example]: https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FPUMSDataDict06.pdf

EOF