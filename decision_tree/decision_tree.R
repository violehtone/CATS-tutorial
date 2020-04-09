# LOAD DATA AND PACKAGES
# load packages
library('caret')
library(tidyverse)

# Set working directory
current_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
data_file_path <- paste(current_dir, '/data', sep = "")
setwd(data_file_path)

# Read data
train_call <- read.delim('Train_call.txt', header = TRUE, sep = "\t",
                         quote = "\"", dec = ".", fill = TRUE,
                         comment.char = "")

train_clinical <- read.delim('Train_clinical.txt', header = TRUE, sep = "\t",
                             quote = "\"", dec = ".", fill = TRUE,
                             comment.char = "")


# PRE PROCESSING
# Set working directory back
setwd("..")

# Pre-process train_call data
df_train_call <- t(as.data.frame(train_call[,-(1:4), drop=FALSE]))

# Pre-process train_clinical data
df_train_clinical <- as.data.frame(train_clinical)
row.names(df_train_clinical) <- df_train_clinical$Sample
df_train_clinical <- df_train_clinical[,-(1:1), drop=FALSE]

# Merge the dataframes
df_train_merged <- merge(df_train_clinical, df_train_call, by ="row.names")
rownames(df_train_merged ) <- df_train_merged$Row.names
df_train_merged$Row.names <- NULL


# FEATURE SELECTION
#Calculate the importance of features for the 3 classes
rocVarImp <- filterVarImp(df_train_merged[-1],
                          df_train_merged[[1]],
                          nonpara = FALSE)

#Calculate the total importance of predicting any cancer
rocVarImp$importance <- apply(rocVarImp, 1, mean)

#Sort features by importance
rocVarImp <- rocVarImp[order(-rocVarImp$importance),]

#Select top 20 features
selected_features <- rocVarImp[c(1:20),]

#Filter the selected features from the training data
df_train_filtered <- df_train_merged[, rownames(selected_features)]

#Add the cancer groups back to data
df_train_filtered <- merge(df_train_merged["Subgroup"],
                           df_train_filtered,
                           by = "row.names")



# TRAINING THE CLASSIFIER
#Separate the features and classes
train_data <- df_train_filtered[, 2:21]
train_classes <- df_final[, 1]

#Choose cross validation method (10FCV)
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10)

#Train the classifier with the CV method
dt_fit <- train(train_data,
                train_classes,
                method = "rpart",
                tuneLength = 10,
                trControl = ctrl)




