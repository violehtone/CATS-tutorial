library('caret')
library(tidyverse)

### LOADING THE DATA
setwd('/home/villelehtonen/CATS')
train_call <- read.delim('Train_call.txt', header = TRUE, sep = "\t",
                             quote = "\"", dec = ".", fill = TRUE,
                             comment.char = "")
train_clinical <- read.delim('Train_clinical.txt', header = TRUE, sep = "\t",
                             quote = "\"", dec = ".", fill = TRUE,
                             comment.char = "")


### PROCESSING THE DATA
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

#### Feature selection
#inspect features that distinct the clinical outcomes best
outcomes <- factor(as.vector(df_merged$Subgroup))
df_merged_filtered <- subset(df_merged, select = -Subgroup)

head(df_merged_filtered[,1:10])
head(outcomes)

rocVarImp <- filterVarImp(df_merged_filtered, outcomes, nonpara = FALSE)
head(rocVarImp)

#Calculate the total importance of the variable (for all subgroups)
rocVarImp_modified <- rocVarImp
rocVarImp_modified$importance <- apply(rocVarImp_modified, 1, mean)

head(rocVarImp_modified)

#Sort the df based on their importance
sorted_importance <- rocVarImp_modified[order(rocVarImp_modified$importance),]

#Check the mean of importance to decide which cutoff to use
mean(sorted_importance$importance) #mean -> 0.59
head(sorted_importance)

#Create a barplot to inspect the data
df_sorted <- sorted_importance
df_sorted$feature <- rownames(df_sorted)

ggplot(df_sorted, aes(x=feature, y=importance)) +
         geom_bar(stat="identity")


#filter out all the not relevant features (only leaving the ones with the highest importance)
