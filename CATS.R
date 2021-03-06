library('caret')
library(tidyverse)



### 1. LOADING THE DATA
setwd('/home/villelehtonen/CATS')
train_call <- read.delim('Train_call.txt', header = TRUE, sep = "\t",
                             quote = "\"", dec = ".", fill = TRUE,
                             comment.char = "")

train_clinical <- read.delim('Train_clinical.txt', header = TRUE, sep = "\t",
                             quote = "\"", dec = ".", fill = TRUE,
                             comment.char = "")

#inspect raw data
dim(train_call)
dim(train_clinical)
head(train_call[,1:20])
head(train_clinical)



### 2. PROCESSING THE DATA
#transform to data frame, remove irrelevant columns, and transpose
df_call <- t(as.data.frame(train_call[,-(1:4), drop=FALSE]))

#Set rownames for clinical data
df_clinical <- as.data.frame(train_clinical)
row.names(df_clinical) <- df_clinical$Sample
df_clinical <- df_clinical[,-(1:1), drop=FALSE]

#inspect the dataframes
head(df_call[,1:7])
head(df_clinical)

#merge the two tables
df_merged <- merge(df_clinical, df_call, by ="row.names")
rownames(df_merged) <- df_merged$Row.names
df_merged$Row.names <- NULL

#inspect that dataframe was indeed merged correctly
dim(df_call)
dim(df_merged)
head(df_merged[,1:10])

#### 3. Feature selection
#inspect features that distinct the clinical outcomes best
outcomes <- factor(as.vector(df_merged$Subgroup))
df_merged_filtered <- subset(df_merged, select = -Subgroup)

head(df_merged_filtered[,1:10])
head(outcomes)

rocVarImp <- filterVarImp(df_merged_filtered, outcomes, nonpara = FALSE)
head(rocVarImp)

#Calculate the total importance of the variable (for all subgroups)
rocVarImp$importance <- apply(rocVarImp, 1, mean)
head(rocVarImp)

#sort by importance
rocVarImp <- rocVarImp[order(-rocVarImp$importance),]
head(rocVarImp)

#Choose top 20 variables
rocVarImp_filtered <- rocVarImp[c(1:20),]

head(rocVarImp_filtered)
dim(rocVarImp_filtered)

#filter the variables from the original data
df_features_selected <- df_merged_filtered[, rownames(rocVarImp_filtered)]
head(df_features_selected[,1:10])

subgroups <- df_merged["Subgroup"]
head(subgroups)

df_final <- merge(subgroups, df_features_selected, by = "row.names")
rownames(df_final) <- df_final$Row.names
df_final$Row.names <- NULL

dim(df_final)
head(df_final[,1:5])

### 4. Training the classifier

#Train a classifier
TrainData <- df_final[, 2:21]
TrainClasses <- df_final[, 1]
ctrl <- trainControl(method = "repeatedcv",
                     repeats = 10)

knnFit <- train(TrainData, TrainClasses,
                method = "knn",
                tuneLength = 10,
                trControl = ctrl)


### 5. Making predictions
# best performing classification model, k=23

#Read validation samples
validation_samples <- read.delim('.txt', header = TRUE, sep = "\t",
                                 quote = "\"", dec = ".", fill = TRUE,
                                 comment.char = "")

#Preprocess data
df_validation <- t(as.data.frame(validation_samples[,-(1:4), drop=FALSE]))

#Select n features
df_validation_filtered <- df_validation_filtered[, rownames(rocVarImp_filtered)]


#Use predict function to generate a vector of class predictions
Output <- predict(knnFit, newdata = df_validation_filtered)



### 6. Generating output
#Create a data frame from the sample names and the predictions


#Use write.table() to write the results to a .txt file
write.table(Output,
            "myPredictions.txt",
            sep="_",
            row.names = FALSE)








