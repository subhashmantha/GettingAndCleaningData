## Step1: This step reads the test and train raw files and converts them to test and train datasets

## Step1a: Read the raw files for training data and create training data set
	## This code is in the root directory C:\\Users\\smantha\\Downloads\\RProgramming\\RCode

	subject.test<-read.table("UCI HAR Dataset\\test\\subject_test.txt",header=FALSE)
	X.test<-read.table("UCI HAR Dataset\\test\\X_test.txt",header=FALSE)
	Y.test<-read.table("UCI HAR Dataset\\test\\y_test.txt",header=FALSE)
	Test.data <- cbind(subject.test,X.test,Y.test)
	message("Finished reading and merging Test Data")

## Step1b: Read the raw files for training data and create training data set
	## This code is in the root directory C:\\Users\\smantha\\Downloads\\RProgramming\\RCode
	subject.train<-read.table("UCI HAR Dataset\\train\\subject_train.txt",header=FALSE)
	X.train<-read.table("UCI HAR Dataset\\train\\X_train.txt",header=FALSE)
	Y.train<-read.table("UCI HAR Dataset\\train\\y_train.txt",header=FALSE)
	Train.data <- cbind(subject.train,X.train,Y.train)
	message("Finished reading and merging Training Data")

## Step1c: Merge Test.data Train.data and store it in the working directory
	Test.n.Train.data <- rbind(Test.data,Train.data)
	message("Finished merging Training and Test Data")

## Step2a: Read and create variable names out of features 
	## Read Features file and create varnames out of it

	Feature.labels<-read.table("UCI HAR Dataset\\features.txt")
	colnames(Feature.labels)<-c("Colnum","Label")
	## Modify the string so that all the commas are replaced with "."
	Feature.labels$Colname<-gsub(",",".",Feature.labels$Label)

	colnames<-Feature.labels$Colname
	## Rename the dataset with column names derived above
	colnames(Test.n.Train.data)<-c("Subject_id",colnames,"Activity")
	## Save the dataframe to <Root>\data location. 
	## This is a combination of all rows of test and training data with all the columns
	

	## Start Filtering out the columns based on the occurrence of word mean and std
	colnames2<-c("Subject_id",colnames[grepl('mean',colnames)],colnames[grepl('std',colnames)],"Activity")
	## Drop  columns that has meanfreq in them
	colnames3<-colnames2[!grepl('Freq',colnames2)]
	message("Created Variable names")
	
	
## Step2b: Convert Activity to factors and assign labels from Activity labels
	## Convert the activity in the dataframe to a factor/level
	Test.n.Train.data$Activity<-as.factor(Test.n.Train.data$Activity)

	## Get the activity labels from Activity_labels
	Activity.labels<-read.table("UCI HAR Dataset\\Activity_labels.txt")
	level.Labels<-Activity.labels$V2

	## Assign labels to activity levels
	levels(Test.n.Train.data$Activity) <- level.Labels
	message("Converted Activity to a factor and labelled the Levels")
	save(Test.n.Train.data,file="data\\Test.n.Train.data.rda")
	write.table(Test.n.Train.data,file="data\\Test.n.Train.data.txt",sep="|",row.names=FALSE)
	
##Step3: Create a subset of data with required columns containing mean and standard deviation
	Test.n.Train.data.mean.std<-Test.n.Train.data[colnames3]
	message("Created a dataset with Subject Activity mean and Standard deviation only")
##Step4: Use reshape library to calculate mean of all the variables that are in Test.n.Train.data.mean.std
library(reshape2)
	## This step converts dataframe <idcol1 idcol2 feat1 feat2 ...>
	## <idcol1 idcol2 featurename value>
	molten<-melt(Test.n.Train.data.mean.std,id=c("Subject_id", "Activity"), na.rm=FALSE)

	## Calculate mean values of all the feature names by idcol1 and idcol2
	Mean.FinalData<-acast(molten,Subject_id+Activity ~ variable, mean)

	## Get the row names and convert them into Subject and activity columns
	Rownames.matrix<-rownames(Mean.FinalData)
	Subject<-matrix(c(substr(Rownames.matrix,1,regexpr("_",Rownames.matrix,)-1)),nrow=180,ncol=1)
	Activity<-matrix(c(substr(Rownames.matrix,regexpr("_",Rownames.matrix,)+1,length(Rownames.matrix))),nrow=180,ncol=1)
	
	## Convert the summary matrix into a data frame
	Mean.FinalDataFrame<-cbind(data.frame(Subject),data.frame(Activity),data.frame(Mean.FinalData)[,2:67])
	## Assign names to columns in the data frame
	colnames(Mean.FinalDataFrame)<-c("Subject","Activity",colnames3[2:67])
	## Save the dataframe to <Root>\data location
	save(Mean.FinalDataFrame,file="data\\Mean.FinalDataFrame.rda")
	write.table(Mean.FinalDataFrame,file="data\\Mean.FinalDataFrame.txt",sep="|",row.names=FALSE)
	message("Finished calculating mean and saved the data frame")


