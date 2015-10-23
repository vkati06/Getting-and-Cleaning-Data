setwd("C:/Users/Kati/Documents/Folyamatban_2015-07/Coursera Data Science Specialization_2015-08-03/3_Getting and Cleaning Data/Course Project/UCI HAR Dataset")

X.test <- read.table("./test/X_test.txt")
X.train <- read.table("./train/X_train.txt")
data <- rbind(X.test, X.train)

features <- read.table("features.txt")[, 2]
length(features)
meanvars <- features[grepl("mean", features)]
meanvars
length(meanvars)
sdvars <- features[grepl("std", features)]
sdvars
length(sdvars)

library(dplyr)
data <- select(data, c(meanvars, sdvars))
varnames <- c(as.character(meanvars), as.character(sdvars))
varnames
length(varnames)
varnames <- gsub("-|,", "_", varnames) # replace - and , with _
varnames <- gsub("\\()", "", varnames) # remove brackets
varnames 

names(data) <- make.names(varnames)

subject.test <- read.table("./test/subject_test.txt")[, 1]
subject.train <- read.table("./train/subject_train.txt")[, 1]
id <- c(as.numeric(subject.test), as.numeric(subject.train))
data <- cbind(as.data.frame(id), data)

y.test <- read.table("./test/y_test.txt")[, 1]
y.train <- read.table("./train/y_train.txt")[, 1]
activity.labels <- read.table("activity_labels.txt")
labels <- activity.labels[, 2]
activity.test <- factor(y.test, labels = labels)
activity.train <- factor(y.train, labels = labels)
activity <- c(as.character(activity.test), as.character(activity.train))
data <- cbind(activity, data)

test_train <- rep(c(1, 2), c(length(y.test), length(y.train)))
test_train <- factor(test_train, labels = c("test", "train"))
data <- cbind(test_train, data)

tiny <- data.frame(matrix(NA, nrow = 180))
names <- names(data)[4:82]

for (i in names){
        j = with(data, get(i))
        temp <- with(data, tapply(j, list(id, activity), mean, na.rm = TRUE))
        tiny[, i] <- as.vector(temp)
}

tiny_act_names <- rep(names(as.data.frame(temp)), each = 30)
tiny_ids <- rep(1:30, 6)

tiny <- tiny[, -1]
tiny <- cbind(tiny_ids, tiny_act_names, tiny)

# Megjegyzés: ez így nagyon kókányolt... Valahogy máshogy kellene csoportok
# szerint átlagot számolni... Úgy, hogy automatikusan fel tudjuk tüntetni, hogy
# mire vonatkozik az adott átlag, tehát ne kézzel kelljen felvinni az "activity"
# és az "id" címkéket!
library(plyr)
ddply(data, .(id,activity), summarise, mean = mean(fBodyBodyGyroJerkMag_std))
# Valami hasonló van a ddplyr könyvtárban is: "Getting and Cleaning Data_Cleaning.R"
# szkriptet megnézni!!!
library(dplyr)
grouped_data = group_by(data, id, activity)
summarise(grouped_data, mean = mean(fBodyBodyGyroJerkMag_std, na.rm = TRUE))
