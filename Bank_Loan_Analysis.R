# Load necessary packages
install.packages("httpuv")
install.packages("naniar")
install.packages("forcats")
install.packages("ggpubr")
install.packages("ggthemes")
install.packages("gridExtra")
install.packages("corrplot")
install.packages("ROCR")
install.packages("pROC")
install.packages("ROSE")
install.packages("caTools")
install.packages("glmnet")
install.packages("Metrics")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("kableExtra")
install.packages("naivebayes")
install.packages("e1071")

library(httpuv)
library(e1071)
library(naivebayes)
library(kableExtra)
library(rpart.plot)
library(rpart)
library(Metrics)
library(glmnet)
library(naniar)
library(caTools)
library(pROC)
library(ROCR)
library(ROSE)
library(corrplot)
library(gridExtra)
library(naniar)
library(forcats)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggthemes)
library(gridExtra)

# Load the dataset
BankDF <- read.csv("C:/Bank Loan Data/application_data.csv")

dim(BankDF)

# Find and drop columns with too many missing values

miss <- c()  # Initialize vector to store columns with too many missing values
for (i in 1:ncol(BankDF)) {
  if (sum(is.na(BankDF[, i])) > 140000) {
    miss <- append(miss, i) 
  }
}

BankDF<- BankDF[,-miss]
miss2 <- c()  # Initialize vector to store rows with too many missing values
for (i in 1:nrow(BankDF)) {
  if (sum(is.na(BankDF[i, ])) > 0.5 * ncol(BankDF)) {
    miss2 <- append(miss2, i) 
  }
}

BankDF<- BankDF[-miss,]

BankDF<- BankDF[,-c(52:71)]

#check how many columns left
dim(BankDF)

# Data cleaning: Replace missing values in specific columns
# Replace missing values in specific columns with a specific value
BankDF$EMERGENCYSTATE_MODE[is.na(BankDF$EMERGENCYSTATE_MODE)] <- ""
BankDF$EMERGENCYSTATE_MODE= fct_explicit_na(BankDF$EMERGENCYSTATE_MODE, "Unknown")

BankDF$OCCUPATION_TYPE[is.na(BankDF$OCCUPATION_TYPE)] <- "Unknown"
BankDF$OCCUPATION_TYPE= fct_explicit_na(BankDF$OCCUPATION_TYPE, "Unknown")


BankDF$FONDKAPREMONT_MODE[is.na(BankDF$FONDKAPREMONT_MODE)] <- "Unknown"
BankDF <- replace_with_na(BankDF, replace = list(FONDKAPREMONT_MODE = "Unknown", HOUSETYPE_MODE = "Unknown", WALLSMATERIAL_MODE = "Unknown"))

BankDF$HOUSETYPE_MODE[is.na(BankDF$HOUSETYPE_MODE)] <- "Unknown"
BankDF$HOUSETYPE_MODE = fct_explicit_na(BankDF$HOUSETYPE_MODE, "Unknown")

BankDF$WALLSMATERIAL_MODE[is.na(BankDF$WALLSMATERIAL_MODE)] <- "Unknown"
BankDF$WALLSMATERIAL_MODE = fct_explicit_na(BankDF$WALLSMATERIAL_MODE, "Unknown")

# Extract numeric columns for replacement with mean
numeric_cols <- sapply(BankDF, is.numeric)

# Replace missing values with mean in numeric columns
for(i in 1:ncol(BankDF)) {                                   
  if(numeric_cols[i]) {
    BankDF[, i][is.na(BankDF[, i])] <- mean(BankDF[, i], na.rm = TRUE)
  }
}

# Transform specific columns
BankDF$DAYS_BIRTH<-BankDF$DAYS_BIRTH*-1
BankDF$DAYS_EMPLOYED<-BankDF$DAYS_EMPLOYED*-1
BankDF$DAYS_REGISTRATION<-BankDF$DAYS_REGISTRATION*-1
BankDF$DAYS_ID_PUBLISH<-BankDF$DAYS_ID_PUBLISH*-1
BankDF$DAYS_LAST_PHONE_CHANGE<-BankDF$DAYS_LAST_PHONE_CHANGE*-1
bins = c(0,350000,700000,1000000000)
slots = c('Low','Medium','High')

#Creating bins for Credit amount
BankDF['AMT_CREDIT_RANGE']=cut(BankDF$AMT_CREDIT,breaks=c(0,350000,700000,1000000000),labels=slots)
bins = c(0,200000,400000,10000000000)
slots = c('Low','Medium','High')

# Creating bins for income amount
BankDF['AMT_INCOME_RANGE']=cut(BankDF$AMT_INCOME_TOTAL,breaks=c(0,200000,400000,10000000000),labels=slots)
bins = c(0,7300,10950,14600,18250,21900,25500)
slots <- c('0-20','20-30','30-40','40-50','50-60','60-70')
BankDF$AGE_RANGE <- cut(BankDF$DAYS_BIRTH, breaks = c(0,7300,10950,14600,18250,21900,25500), labels = slots)
BankDF <- distinct(BankDF)

# Finding and counting the duplicated rows in the dataset
sum(duplicated(BankDF))

# Counting the total number of missing values in the dataset
sum(is.na(BankDF))

# Displaying the dimensions of the dataset (rows and columns)
dim(BankDF)

# Creating a table of proportions for the 'TARGET' variable
prop.table(table(BankDF$TARGET))

# Removing unnecessary columns and storing the modified dataset in 'df'
df= select(BankDF,-1,-31,)


#Checking for missing values
-------------------------------------------------------------------
  
  # Total number of missing values in the dataset
  cat("The total number of missing values in the dataset is" , sum(is.na(BankDF)))

colSums(is.na(BankDF))

View(BankDF)

#Outlier Analysis
-------------------------------------------------------------------
  # Extract the AGE variable
  BankDF1<- BankDF


# Assuming 'BankDF1' 
col_list <- c('AMT_INCOME_TOTAL', 'AMT_CREDIT', 'AMT_ANNUITY', 'AMT_GOODS_PRICE')

# Loop through the specified columns and divide each value by 100000
for (col in col_list) {
  BankDF1[[col]] <- BankDF1[[col]] / 100000
}



# Extract the AGE variable
age <- BankDF1$AGE

# Plotting boxplot
boxplot(age, main="Client's age", ylab="", col="lightblue", border="blue", horizontal=TRUE)


# Extract the Income Amount variable
income <- BankDF$AMT_INCOME_TOTAL

# Function for outlier plot


income <- BankDF1$AMT_INCOME_TOTAL

# Function for outlier plot
outlier_plot <- function(data, title, label) {
  # Plotting boxplot
  boxplot(data, main=title, ylab=label, col="lightblue", border="blue", horizontal=FALSE)
}

# Calling the function
outlier_plot(income, "Client's income", "Income in Lakhs")


credit <- BankDF1$AMT_CREDIT

# Function for outlier plot
outlier_plot <- function(data, title, label) {
  # Plotting boxplot
  boxplot(data, main=title, ylab=label, col="lightblue", border="blue", horizontal=FALSE)
}

# Calling the function
outlier_plot(credit, "Credit amount of the loan", "Amount in Lakhs")




#Univariate Analysis
------------------------------------------------------------------
  # Creating plots for analyzing individual variables in the dataset
  
  # Creating a bar plot to visualize the distribution of genders in the 'BankDF' dataset
  genderplot<-ggplot(data=BankDF,aes(x=CODE_GENDER,fill=CODE_GENDER)) +
  geom_bar(stat="count") + ggtitle('Gender')+
  theme(plot.title = element_text (hjust = 0.5)) + theme(legend.position="none") + xlab('') + ylab ('')

# Creating a bar plot to explore the distribution of credit amount ranges in 'BankDF'
AMT_CREDITRANGEplot<-ggplot(data=BankDF,aes(x=factor(AMT_CREDIT_RANGE, level=c('Low','Medium','High')),fill=AMT_CREDIT_RANGE))+
  geom_bar(stat="count") + ggtitle('Credit Amount (Range)') + 
  theme(plot.title = element_text (hjust = 0.5)) + theme(legend.position="none") + xlab('') + ylab ('')

# Creating a bar plot to analyze the distribution of income amount ranges in 'BankDF'
AMT_INCOME_RANGEplot<-ggplot(data=BankDF,aes(x=factor(AMT_INCOME_RANGE, level=c('Low','Medium','High')),fill=AMT_INCOME_RANGE))+
  geom_bar(stat="count") + ggtitle('Income Amount (Range)') + 
  theme(plot.title = element_text (hjust = 0.5)) + theme(legend.position="none") + xlab('') + ylab ('')

# Creating a bar plot to visualize the distribution of age ranges in 'BankDF'
AGE_RANGEplot<-ggplot(data=BankDF,aes(x=AGE_RANGE,fill=AGE_RANGE))+ 
  #scale_fill_manual(values = c("#F8766D","#A3A500","#00BF7D","#00B0F6","#E76BF3"))+
  geom_bar(stat="count") + ggtitle('Age Range') +
  theme(plot.title = element_text (hjust = 0.5)) + theme(legend.position="none") + xlab('') + ylab ('')

# Arranging individual plots in a grid layout for comparative analysis
p1 <- ggarrange(genderplot, AMT_INCOME_RANGEplot,
                ncol = 1, nrow = 2)
p2 <- ggarrange(AGE_RANGEplot, AMT_CREDITRANGEplot, 
                ncol = 1, nrow = 2)
ggarrange(p1, p2, ncol = 2, nrow = 1)

#INCOME TYPE,EDUCATION TYPE, FAMILY STATUS, HOUSING TYPE, OCCUPATION TYPE
------------------------------------------------------------------------------------
  # Analysis of Categorical Variables
  # Visualizing different categorical variables within the dataset
  
  # Creating a bar plot to analyze the distribution of income types in the 'BankDF' dataset
  incometypeplot <- ggplot(data = BankDF, aes(x = NAME_INCOME_TYPE, fill = NAME_INCOME_TYPE)) +
  geom_bar(stat = "count") + ggtitle('Types of Income') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") + xlab('') + ylab('')

# Creating a bar plot to examine the distribution of education types in 'BankDF'
edutypeplot <- ggplot(data = BankDF, aes(x = NAME_EDUCATION_TYPE, fill = NAME_EDUCATION_TYPE)) +
  geom_bar(stat = "count") + ggtitle('Types of Education') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") + xlab('') + ylab('')

# Creating a bar plot to explore the distribution of marital statuses in the dataset
marriagetypeplot <- ggplot(data = BankDF, aes(x = NAME_FAMILY_STATUS, fill = NAME_FAMILY_STATUS)) +
  geom_bar(stat = "count") + ggtitle('Marital Status') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") + xlab('') + ylab('')

# Creating a bar plot to visualize the distribution of housing types
housetypeplot <- ggplot(data = BankDF, aes(x = NAME_HOUSING_TYPE, fill = NAME_HOUSING_TYPE)) +
  geom_bar(stat = "count") + ggtitle('Types of Housing') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") + xlab('') + ylab('')

# Creating a bar plot to analyze the distribution of occupation types
worktypeplot <- ggplot(data = BankDF, aes(x = OCCUPATION_TYPE, fill = OCCUPATION_TYPE)) +
  geom_bar(stat = "count") + ggtitle('Types of Occupation') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") + xlab('') + ylab('')

# Arranging the plots for income, education, marital status, housing, and occupation types
p3 <- ggarrange(incometypeplot, edutypeplot,
                ncol = 1, nrow = 2)
p4 <- ggarrange(marriagetypeplot, housetypeplot,
                ncol = 1, nrow = 2)
ggarrange(p3, p4, ncol = 2, nrow = 1)

# Additional visualization: Plotting the occupation type with vertical categories for better display
worktypeplot + coord_flip()


# Bivariate analysis
-------------------------------------------------------------------------
  # Exploratory Data Analysis - Visualizing Relationships between Different Variables and the Target
  
  # AGE and Gender vs. TARGET
  # Plotting the distribution of 'AGE_RANGE' by 'TARGET' and 'CODE_GENDER' by 'TARGET' in two separate bar plots
  par(mfrow = c(2, 1))

# Visualization of 'AGE_RANGE' by 'TARGET'
BankDF1 <- with(BankDF, table(TARGET, AGE_RANGE))
barplot(BankDF1, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(-0.05, 0)),
        col = c("Red", "Green"), xlab = "AGE_RANGE", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Visualization of 'CODE_GENDER' by 'TARGET'
BankDF2 <- with(BankDF, table(TARGET, CODE_GENDER))
barplot(BankDF2, beside = TRUE, legend = TRUE, col = c("Red", "Green"), args.legend = list(x = "topright", inset = c(-0.05, 0)),
        xlab = "CODE_GENDER", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Amount of Credit (loan), Income type, and Amount of Income vs. TARGET
par(mfrow = c(3, 1))

# Visualizing 'AMT_CREDIT_RANGE' by 'TARGET'
BankDF3 <- with(BankDF, table(TARGET, AMT_CREDIT_RANGE))
barplot(BankDF3, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "Amount of Credit(Loan)", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Visualizing 'NAME_INCOME_TYPE' by 'TARGET'
BankDF4 <- with(BankDF, table(TARGET, NAME_INCOME_TYPE))
barplot(BankDF4, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "NAME_INCOME_TYPE", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Visualizing 'AMT_INCOME_RANGE' by 'TARGET'
BankDF5 <- with(BankDF, table(TARGET, AMT_INCOME_RANGE))
barplot(BankDF5, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "AMT_INCOME_RANGE", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Types of Education, Marital Status, and Types of Housing vs. TARGET
par(mfrow = c(3, 1))

# Visualizing 'NAME_EDUCATION_TYPE' by 'TARGET'
BankDF6 <- with(BankDF, table(TARGET, NAME_EDUCATION_TYPE))
barplot(BankDF6, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "Types of Education", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Visualizing 'NAME_FAMILY_STATUS' by 'TARGET'
BankDF7 <- with(BankDF, table(TARGET, NAME_FAMILY_STATUS))
barplot(BankDF7, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "Marital Status", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# Visualizing 'NAME_HOUSING_TYPE' by 'TARGET'
BankDF8 <- with(BankDF, table(TARGET, NAME_HOUSING_TYPE))
barplot(BankDF8, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green"), xlab = "Types of Housing", ylab = "Values",
        main = "Target=0: No Payment issue, Target=1: Payment issue")

# AMT_CREDIT, AMT_ANNUITY, AMT_GOODS_PRICE vs. TARGET
# Creating density plots for 'AMT_CREDIT', 'AMT_ANNUITY', 'AMT_GOODS_PRICE' based on the 'TARGET'
p1 <- ggplot(data = BankDF, aes(x = AMT_CREDIT, group = TARGET, fill = TARGET)) +
  geom_density(adjust = 1.5)

p2 <- ggplot(data = BankDF, aes(x = AMT_ANNUITY, group = TARGET, fill = TARGET)) +
  geom_density(adjust = 1.5)

p3 <- ggplot(data = BankDF, aes(x = AMT_GOODS_PRICE, group = TARGET, fill = TARGET)) +
  geom_density(adjust = 1.5)

# Arrange and display the density plots in a single column
grid.arrange(p1, p2, p3, ncol = 1)

# Exploring Relationships and Data Insights

# Correlation Matrix
# Creating a correlation matrix of numeric variables in BankDF using Spearman method and visualizing the correlation matrix using corrplot.
plot.new() # Creates a new plotting device
dev.off()  # Turns off the active plotting device
data = select_if(BankDF, is.numeric)  # Selecting only numeric columns
data.cor = cor(data, method = c("spearman"))  # Calculating the Spearman correlation matrix
corrplot(data.cor, tl.cex = 0.5)  # Visualizing the correlation matrix with adjustable text size

# Scatter Plots and Bar Plots

# AMT_INCOME_TOTAL vs AMT_CREDIT, AMT_GOODS_PRICE vs AMT_CREDIT
par(mfrow = c(1, 2))  # Divides the plotting area into a 1x2 grid

# Scatter plot: Amount of Income vs Amount of Credit (Range)
plot(data$AMT_INCOME_TOTAL, data$AMT_CREDIT, 
     main = "Amount of Income vs Amount of Credit (Range)",
     xlab = "AMT_INCOME_TOTAL", ylab = "AMT_CREDIT", pch = 19)  # Creates a scatter plot

# Scatter plot: Good Price vs Amount of Credit (Range)
plot(data$AMT_GOODS_PRICE, data$AMT_CREDIT, 
     main = "Good Price vs Amount of Credit (Range)",
     xlab = "AMT_GOODS_PRICE", ylab = "AMT_CREDIT", pch = 19)  # Creates a scatter plot

# Bar Plot: AMT_INCOME_RANGE vs CODE_GENDER
BankDF9 <- with(BankDF, table(AMT_INCOME_RANGE, CODE_GENDER))
barplot(BankDF9, beside = TRUE, legend = TRUE, args.legend = list(x = "topright", inset = c(0, 0)),
        col = c("Red", "Green", "Blue"), xlab = "CODE_GENDER", ylab = "values",
        main = "The relationship between income range and gender")

# Box Plot: AMT_CREDIT vs NAME_EDUCATION_TYPE
ggplot(BankDF, aes(x = NAME_EDUCATION_TYPE, y = AMT_CREDIT, fill = NAME_FAMILY_STATUS)) + 
  geom_boxplot()  # Creates a box plot to visualize the relationship between education type and credit amount

# Data Manipulation

# Dropping insignificant variables (REGION_RATING_CLIENT_W_CITY, SK_ID_CURR) and assigning the modified data to 'df'
df = select(BankDF, -1, -31)  # Drops columns based on their positions (-1, -31)

#Logistic regression
-----------------------------------------------------------
  
  # Setting a seed for reproducibility in random processes
  set.seed(123)

# Splitting the dataset into training and testing sets
split = sample.split(df$TARGET,SplitRatio = 0.70)
training_set = subset(df, split == TRUE)
test_set = subset(df, split == FALSE)

# Standardizing continuous columns in the training and testing sets
# List of continuous columns to standardize
continuous_column = c('AMT_INCOME_TOTAL',  'AMT_CREDIT',  'AMT_ANNUITY',  'AMT_GOODS_PRICE',  'REGION_POPULATION_RELATIVE',  'DAYS_REGISTRATION',  'CNT_FAM_MEMBERS',  'EXT_SOURCE_2', 'EXT_SOURCE_3',  'OBS_30_CNT_SOCIAL_CIRCLE',  'DEF_30_CNT_SOCIAL_CIRCLE',  'OBS_60_CNT_SOCIAL_CIRCLE',  'DEF_60_CNT_SOCIAL_CIRCLE', 'DAYS_LAST_PHONE_CHANGE','AMT_REQ_CREDIT_BUREAU_HOUR',  'AMT_REQ_CREDIT_BUREAU_DAY',  'AMT_REQ_CREDIT_BUREAU_WEEK',  'AMT_REQ_CREDIT_BUREAU_MON', 'AMT_REQ_CREDIT_BUREAU_QRT',  'AMT_REQ_CREDIT_BUREAU_YEAR')
training_set[continuous_column] = scale(training_set[continuous_column])
test_set[continuous_column] = scale(test_set[continuous_column])
table(training_set$TARGET)

# Balancing the training set using oversampling method to handle class imbalance
bal_training_set=ovun.sample(TARGET~., data = training_set, method = "over", N = 197849*2)$data
table(bal_training_set$TARGET)

# Defining a function to generate a confusion matrix
get_cm <- function(test_set, y_pred){
  cm <- as.matrix(table(Actual = test_set$TARGET, Predicted = y_pred))
}

# Defining a function to evaluate the performance of the model
simple_eval <- function (test_set, y_pred) {
  CM = get_cm(test_set, y_pred)
  n = sum(CM)  # number of instances
  nc = nrow(CM)  # number of classes, should be 2 in our case
  rowsums = apply(CM, 1, sum)  # number of instances per class
  colsums = apply(CM, 2, sum)  # number of predictions per class
  p = rowsums / n  # distribution of instances over the actual classes
  q = colsums / n  # distribution of instances over the predicted classes
  diag = diag(CM)  # get TP and TN using identity matrix
  
  accuracy = sum(diag) / n  # overall classification accuracy
  precision = diag / colsums  # fraction of correct predictions for a certain class
  recall = diag / rowsums  # fraction of instances of a class that were correctly predicted
  f1 = 2 * precision * recall / (precision + recall)  # harmonic mean (or a weighted average) of precision and recall
  
  # Ensuring all vectors have the same length
  accuracy = rep(accuracy, length.out = length(precision))
  f1 = rep(f1, length.out = length(precision))
  
  perf_df = data.frame(accuracy, precision, recall, f1) 
  AUC = auc(test_set$TARGET, factor(y_pred, ordered = TRUE))  # Area under ROC curve
  expAccuracy = sum(p * q)
  test = (accuracy - expAccuracy) / (1 - expAccuracy)
  macroPrecision = mean(precision)
  macroRecall = mean(recall)
  macroF1 = mean(f1)
  
  list(Accuracy = accuracy, ROC_area = AUC, test = test, Precision = macroPrecision, 
       Recall = macroRecall, Fscore = macroF1, performance_df = perf_df, confusion_matrix = CM)
}



# Building a logistic regression model with balanced training set
model_glm <- glm(TARGET ~ ., data = bal_training_set, family = "binomial")

# Predict test data based on model
LR.y_pred <- predict(model_glm, newdata = test_set, type = "response")
LR.y_pred <- ifelse(LR.y_pred >0.5, 1, 0)

# Confusion matrix
LR.eval <- simple_eval(test_set, LR.y_pred)

LR.eval$confusion_matrix

# Performance by output class
print(round(LR.eval$performance_df,2))

# Displaying confusion matrix and various performance metrics
cat('Accuracy: ', round(LR.eval$Accuracy,2),
    '\nMacro-precision: ', round(LR.eval$Precision,2),
    '\nMacro-recall: ', round(LR.eval$Recall,2),
    '\nMacro-F1: ', round(LR.eval$Fscore,2),
    '\ntest: ', round(LR.eval$test,2),
    '\nROC area: ', round(LR.eval$ROC_area,2)
)



#Decision Tree (DT) Classification
-----------------------------------------------------------
  
  # fit the decision tree classification
  model = rpart(TARGET ~., data = bal_training_set, method = "class")
# plot decision tree
rpart.plot(model, extra = 106)

#Evaluate Decision Tree (DT) Model
# make prediction
DT.y_pred = predict (model, test_set, type = 'class')

# Confusion Matrix
DT.eval <- simple_eval(test_set, DT.y_pred)
DT.eval$confusion_matrix

# Performance by output class
print(round(DT.eval$performance_df,2))

# Model Evaluation
cat('Accuracy: ', round(DT.eval$Accuracy,2),
    '\nMacro-precision: ', round(DT.eval$Precision,2),
    '\nMacro-recall: ', round(DT.eval$Recall,2),
    '\nMacro-F1: ', round(DT.eval$Fscore,2),
    '\nROC area: ', round(DT.eval$ROC_area,2)
)

#Naive Bayes (NB)
-----------------------------------------------------------
  # Fitting Naive Bayes Model to training dataset
  model_NB <- naiveBayes(TARGET ~ ., data = bal_training_set)

# Predicting on test data'
NB.y_pred <- predict(model_NB, newdata = test_set)

# Confusion Matrix
NB.eval <- simple_eval(test_set, NB.y_pred)
NB.eval$confusion_matrix

# Performance by output class
print(round(NB.eval$performance_df,2))

# Model Evaluation
cat('Accuracy: ', round(NB.eval$Accuracy,2),
    '\nMacro-precision: ', round(NB.eval$Precision,2),
    '\nMacro-recall: ', round(NB.eval$Recall,2),
    '\nMacro-F1: ', round(NB.eval$Fscore,2),
    '\nROC area: ', round(NB.eval$ROC_area,2)
)



#Comparison between DT, NB and LR models
-----------------------------------------------------------
compare <- data.frame(Method=c('Decision Tree', 'Naive Bayes', 'Logistic Regression'), Accuracy = NA, Precision = NA, Recall = NA, FScore = NA, 'ROC' = NA)
compare$Accuracy <- c(round(DT.eval$Accuracy[1],2),round(NB.eval$Accuracy[1],2),round(LR.eval$Accuracy[1],2))
compare$Precision <- c(round(DT.eval$Precision,2),round(NB.eval$Precision,2),round(LR.eval$Precision,2))
compare$Recall <- c(round(DT.eval$Recall,2),round(NB.eval$Recall,2),round(LR.eval$Recall,2))
compare$FScore <- c(round(DT.eval$Fscore,2),round(NB.eval$Fscore,2),round(LR.eval$Fscore,2))
compare$ROC <- c(round(DT.eval$ROC_area,2),round(NB.eval$ROC_area,2),round(LR.eval$ROC_area,2))
kable_styling(kable(compare),c("striped","bordered"), full_width = F) 




#Performance Evaluation:
-----------------------------------------------------------
  
# select rows where target = 0 (able to replay the loan)
df1 = subset(df, TARGET == 0)

# Drop unnecessary or duplicated columns
df1 = select (df1, -c('TARGET', 'AMT_CREDIT_RANGE'))

set.seed(123)
split = sample.split(df1$AMT_CREDIT,SplitRatio = 0.70)
training_set = subset(df1, split == TRUE)
test_set = subset(df1, split == FALSE)

continuous_column = c('AMT_INCOME_TOTAL', 'AMT_ANNUITY',  'AMT_GOODS_PRICE',  'REGION_POPULATION_RELATIVE',  'DAYS_REGISTRATION',  'CNT_FAM_MEMBERS',  'EXT_SOURCE_2', 'EXT_SOURCE_3',  'OBS_30_CNT_SOCIAL_CIRCLE',  'DEF_30_CNT_SOCIAL_CIRCLE',  'OBS_60_CNT_SOCIAL_CIRCLE',  'DEF_60_CNT_SOCIAL_CIRCLE', 'DAYS_LAST_PHONE_CHANGE','AMT_REQ_CREDIT_BUREAU_HOUR',  'AMT_REQ_CREDIT_BUREAU_DAY',  'AMT_REQ_CREDIT_BUREAU_WEEK',  'AMT_REQ_CREDIT_BUREAU_MON', 'AMT_REQ_CREDIT_BUREAU_QRT',  'AMT_REQ_CREDIT_BUREAU_YEAR')
training_set[continuous_column] = scale(training_set[continuous_column])
test_set[continuous_column] = scale(test_set[continuous_column])

x_train = data.matrix(select (training_set, -c('AMT_CREDIT'))) #select (training_set, -c('AMT_CREDIT'))
y_train = training_set$AMT_CREDIT

x_test = data.matrix(select (test_set, -c('AMT_CREDIT'))) #select (test_set, -c('AMT_CREDIT'))
y_test = test_set$AMT_CREDIT

regr_evaluation <- function(testDataset, predictionResult){
  MSE <- mse(testDataset$AMT_CREDIT, predictionResult)
  RMSE <- rmse(testDataset$AMT_CREDIT, predictionResult)
  MAE <- mae(testDataset$AMT_CREDIT, predictionResult)
  
  error <- testDataset$AMT_CREDIT - predictionResult
  R2 <- 1-sum(error^2)/sum((testDataset$AMT_CREDIT- mean(testDataset$AMT_CREDIT))^2)
  AD_R2 <- 1-(MSE/var(testDataset$AMT_CREDIT))
  list(MSE = MSE, RMSE = RMSE, MAE = MAE, R_square = R2, Adjusted_R_Square =AD_R2)
}


show_graph <- function(testDataset, predictionResult){
  plot(x=predictionResult, y= testDataset$AMT_CREDIT,
       xlab='Predicted Values',
       ylab='Actual Values',
       main='Predicted vs. Actual Values')
  abline(a=0, b=1)
}


# Visual Inspection and Data Details
View(BankDF)  
dim(BankDF) 


