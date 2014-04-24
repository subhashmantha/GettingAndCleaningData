##Step1: Load data from a pre-existing R- Object    
	load("Data\\Test.n.Train.data.rda")
	colnames<-names(Test.n.Train.data)

	## Start Filtering out the columns based on the occurrence of word mean and std
	colnames2<-c("Subject_id",colnames[grepl('mean',colnames)],colnames[grepl('std',colnames)],"Activity")
	## Drop  columns that has meanFreq in them
	colnames3<-colnames2[!grepl('meanFreq',colnames2)]
	message("Created Variable names")
	
	

##Step2: Create a subset of data with required columns containing mean and standard deviation
	Test.n.Train.data.mean.std<-Test.n.Train.data[colnames3]
	message("Created a dataset with Subject Activity mean and Standard deviation only")
	
##Step3: Use reshape library to calculate mean of all the variables that are in Test.n.Train.data.mean.std
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


