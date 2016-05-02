# lets load the stringer Library for paste0
library(stringer)

# Lets load the data.table Library for the final question
library(data.table)

# Already downloaded and extracted the zip file into WD/UCI-HAR-Dataset

# Create a data frame for the Activity Labels, and title columns Index and Activity
# Read.table with columns as class integer and character
activityLabelsDF <- read.table("UCI-HAR-Dataset/activity_labels.txt",col.names = c("Index","Activity"),colClasses=c("integer","character"))

# Need a vector of trust the Activity for assignment latter in the joined tables
activityLabels<-as.vector(activityLabelsDF$Activity)

# Create a data frame for all Features, and title columns Index and Feature
# Read.table with columns as class numeric and character
featuresDF <- read.table("UCI-HAR-Dataset/features.txt",col.names = c("Index","Feature"),colClasses=c("integer","character"))


# Modify features data frame to only contain mean() and std() re: features_info.txt
# Notice the \\ in front of (), () is reserved in Regex, thus needs to be escaped
featuresDF<- featuresDF[grepl("-mean\\()|-std\\()", featuresDF$Feature),]

# Create a vector of the features we are interested in
features<-as.vector(featuresDF$Feature)

#Create a vector of the indexes of features
featuresIndex<-as.vector(featuresDF$Index)

# Lets make the features a little bit more readable
features<-sapply(features,function(x) {
        if  (grepl("-mean\\()*",x)) 
                paste0("Mean-",sub("-mean\\()","",x))
         else 
                {
                paste0("Std-",sub("-std\\()","",x))
                
                }
})


# Load the Train dataset
train <- read.table("UCI-HAR-Dataset/train/X_train.txt")[featuresIndex]
trainActivities <- read.table("UCI-HAR-Dataset/train/Y_train.txt")
trainSubjects <- read.table("UCI-HAR-Dataset/train/subject_train.txt")
train <- cbind(trainSubjects, trainActivities, train)

# Add friendly column names to train
names(train)<-c("Subject","Activity",features)

# Load the Test dataset
test <- read.table("UCI-HAR-Dataset/test/X_test.txt")[featuresIndex]
testActivities <- read.table("UCI-HAR-Dataset/test/Y_test.txt")
testSubjects <- read.table("UCI-HAR-Dataset/test/subject_test.txt")
test <- cbind(testSubjects, testActivities, test)

# Add friendly column names to train
names(test)<-c("Subject","Activity",features)

# Merge datasets and add labels
TrainTest <- rbind(train, test)

# Replace Activity Index with descriptive text
# First lets make it easy with sapply to have Activity as a character
TrainTest$Activity<-as.character(TrainTest$Activity)

#This way, if the data set ever adds additional activities, they will be handled
TrainTest$Activity<-sapply(TrainTest$Activity,function(x){
        x<-activityLabels[as.numeric(x)]

})

# Replace Subject Index with descriptive text
# First lets make it easy with sapply to have Subject as a character
TrainTest$Subject<-as.character(TrainTest$Subject)

#Add the word Subject infrom of the Subject Index
TrainTest$Subject<-sapply(TrainTest$Subject,function(x){
        x<-paste(" Subject",x)
        
})

# Write the Tidy Data for the combined Test and Train
write.csv(TrainTest,file="Tidy-DataSet-Wearable",row.names = "FALSE")

# First need to create a Data Table from the data frame
TrainTestDT<-data.table(TrainTest)

# Need to create the average of each column by activity and subject
TrainTestAvg<-TrainTestDT[,lapply(.SD,mean),by="Subject,Activity"]

# Write the Average Tidy Data for the combined Test and Train
write.csv(TrainTestAvg,file="Tidy-Average-DataSet-Wearable",row.names = "FALSE")
