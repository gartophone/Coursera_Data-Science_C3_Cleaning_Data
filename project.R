p <- function(path){
    
    # path and name of data files
    activities_data_file <- paste0(path, "activity_labels.txt")
    feature_data_file <- paste0(path, "features.txt")
    subject_test_file <- paste0(path, "test/subject_test.txt")
    test_set_file <- paste0(path, "test/X_test.txt") 
    test_labels_file <- paste0(path, "test/y_test.txt")          
    subject_train_file <- paste0(path, "train/subject_train.txt")
    training_set_file <- paste0(path, "train/X_train.txt")
    training_labels_file <- paste0(path, "train/y_train.txt")

    
    # Step 1: Merges the training and the test sets to create one data set.
    test_set <- read.table(test_set_file)
    train_set <- read.table(training_set_file)
    data_set <- rbind(test_set, train_set)
    print("Step1 done.")
    
    
    # Step 2: Extracts only the measurements on the mean and standard deviation 
    #           for each measurement.
    
    # Read the feature file 
    feature_data <- read.table(feature_data_file)
    names(feature_data) <- c("id", "name")
    
    # Set the names of the data_set cols
    names(data_set) <- feature_data$name
    
    # Create a vector to locate which feature should be kept 
    # Here we keep only the feature related to mean or standard deviation measurments
    colVectorFilter <- sapply(feature_data$name, function(featureName){ grepl("mean\\(\\)|std\\(\\)",featureName) })
    
    # Use this vector to filter the colummns of the data set
    data_set <- data_set[ ,as.logical(colVectorFilter)]
    print("Step2 done.")
    
    # Step 3: Uses descriptive activity names to name the activities in the data set

    # Read the acticvities file to get the id and name of each activity
    activities_data <- read.table(activities_data_file)
    names(activities_data) <- c("id", "name")
    
    # Merge the actitivies ids of the test and training set
    activities_test_id <- read.table(test_labels_file)
    activities_training_id <- read.table(training_labels_file)
    activities_id <- rbind(activities_test_id, activities_training_id)
    # Create a vector with the activities name from the list of actitivies ids present
    # in the test and training set
    activities_labels <- sapply(activities_id$V1, function(id){ activities_data$name[id] })
    # Add the activity label column to the data_set
    data_set <- cbind(data_set, activity_label=activities_labels)
    print("Step3 done.")
    
    # Step 4: Appropriately labels the data set with descriptive variable names.
    # names(data_set)
    
    names(data_set) <- sapply(names(data_set), function(n){
        n <- gsub("-", "_", n)
        n <- gsub("^t", "t_", n)
        n <- gsub("^f", "f_", n)
        n <- gsub("BodyBody", "Body", n)
        n <- gsub("()", "", n)
        n <- gsub("\\(\\)", "", n)
        n
    })
    print("Step4 done.")
    
    # Step 5: From the data set in step 4, creates a second, independent tidy 
    # data set with the average of each variable for each activity and each subject.
    
    # Add the subject_ids column to the data_set
    subject_test <- read.table(subject_test_file)
    subject_train <- read.table(subject_train_file)
    subject_data <- rbind(subject_test, subject_train)
    data_set <- cbind(data_set, subject_id=subject_data$V1)   
    
    tidy_data <- data_set %>% group_by(subject_id, activity_label) %>% summarise_all(mean)
    write.table(tidy_data, file="tidy_data_step5.csv", row.name=FALSE, sep=",")
    print("Step5 done.")
}