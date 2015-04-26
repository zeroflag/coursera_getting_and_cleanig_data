library(data.table)

setwd("c:/Users/zeroflag/development/coursera/getting_and_cleanig_data")

completeData <- function() {
  testSet <- read.table("test/X_test.txt")
  testLabels <- read.table("test/y_test.txt")
  testSubjects <- read.table("test/subject_test.txt")  
  trainingSet <- read.table("train/X_train.txt")
  trainingLabels <- read.table("train/y_train.txt")
  trainingSubjects <- read.table("train/subject_train.txt")
  
  features <- rbind(trainingSet, testSet)
  activities <- rbind(testLabels, trainingLabels)
  subjects <- rbind(trainingSubjects, testSubjects)
  
  featureNames <- read.table("features.txt")
  colnames(features) <- t(featureNames[2])
  
  colnames(activities) <- "Activity"
  colnames(subjects) <- "Subject"
  cbind(subjects, activities, features)
}

relevantData <- function(completeData) {
  stdAndMeanColumns <- grep(".*Std.*|.*Mean.*", names(completeData), ignore.case=TRUE)
  relevantColumns <- c(1,2, stdAndMeanColumns)
  completeData[,relevantColumns]
}

withDescriptiveActivityLabels <- function(data) {
  labels <- read.table("activity_labels.txt")  
  colnames(labels) = c("id", "name")  
  nameById <- function(id) {
    as.character(labels[labels[, "id"] == id,]$name[[1]])
  }  
  data$Activity <- sapply(data$Activity, nameById)
  data
}

withAverages <- function(data) {
  data$Subject <- as.factor(data$Subject)
  data <- data.table(data)
  aggregate(. ~Subject + Activity, data, mean)
}

tidy <- withAverages(withDescriptiveActivityLabels(relevantData(completeData())))
write.table(tidy, file = "tidy.txt", row.names = FALSE)
