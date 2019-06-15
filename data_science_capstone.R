library(tidyverse)
library(randomForest)
library(caret)

download.file(paste('http://archive.ics.uci.edu/ml/machine-learning-databases/',
		    'heart-disease/processed.cleveland.data', sep = ''), 'processed.cleveland.data')
data.1 <- read.csv('processed.cleveland.data',
		   col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs',
				 'restecg', 'thalach', 'exang', 'oldpeak', 'slope',
				 'ca', 'thal', 'target'))

download.file(paste('http://archive.ics.uci.edu/ml/machine-learning-databases/',
		    'heart-disease/reprocessed.hungarian.data', sep = ''), 'processed.hungarian.data')
data.2 <- read.csv('processed.cleveland.data',
		   col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs',
				 'restecg', 'thalach', 'exang', 'oldpeak', 'slope',
				 'ca', 'thal', 'target'))

download.file(paste('http://archive.ics.uci.edu/ml/machine-learning-databases/',
		    'heart-disease/processed.switzerland.data', sep = ''), 'processed.switzerland.data')
data.3 <- read.csv('processed.cleveland.data',
		   col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs',
				 'restecg', 'thalach', 'exang', 'oldpeak', 'slope',
				 'ca', 'thal', 'target'))

download.file(paste('http://archive.ics.uci.edu/ml/machine-learning-databases/',
		    'heart-disease/processed.va.data', sep = ''), 'processed.va.data')
data.4 <- read.csv('processed.cleveland.data',
		   col.names = c('age', 'sex', 'cp', 'trestbps', 'chol', 'fbs',
				 'restecg', 'thalach', 'exang', 'oldpeak', 'slope',
				 'ca', 'thal', 'target'))

data <- rbind(data.1, data.2, data.3, data.4) %>%
	mutate(target = as_factor(ifelse(target == 0, 0, 1)),
	       sex = as_factor(sex),
	       cp = as_factor(cp),
	       exang = as_factor(exang),
	       slope = as_factor(slope),
	       ca = as_factor(ca),
	       thal = as_factor(thal))

levels(data$target) <- c('not_desease', 'desease')
levels(data$sex) <- c('female', 'male')
levels(data$cp) <- c('typical_angina', 'atypical_angina', 'non-anginal_pain',
		     'asymptomatic')
levels(data$exang) <- c('no', 'yes')
levels(data$slope) <- c('upsloping', 'flat', 'downsloping')
levels(data$thal) <- c('?', 'normal', 'fixed_defect', 'reversable_defect')

head(data)
str(data)

### Age

data %>% ggplot(aes(y=age, x=target, color=target)) + geom_boxplot() + theme_minimal()

### Sex

data %>% ggplot(aes(x=sex, color=target)) + geom_bar(fill='#FFFFFF') + theme_minimal()

### Chest pain

data %>% ggplot(aes(x=cp, color=target)) + geom_bar(fill='#FFFFFF') + theme_minimal()

### Rest Blood Pressure

data %>% ggplot(aes(y=trestbps, x=target, color=target)) + geom_boxplot() + theme_minimal()

### Serum cholestoral in mg/dl

data %>% ggplot(aes(y=chol, x=target, color=target)) + geom_boxplot() + theme_minimal()

### Maximum heart rate achieved

data %>% ggplot(aes(y=thalach, x=target, color=target)) + geom_boxplot() + theme_minimal()

### Exercise induced angina

data %>% ggplot(aes(x=exang, color=target)) + geom_bar(fill='#FFFFFF') + theme_minimal()

### ST depression induced by exercise relative to rest vs Slope of the peak exercise

data %>% ggplot(aes(x=slope, y=oldpeak, color=target)) + geom_boxplot() + theme_minimal()

### Number of major vessels colored by flourosopy 

data %>% ggplot(aes(x=ca, color=target)) + geom_bar(fill='#FFFFFF') + theme_minimal()

### Thal

data %>% ggplot(aes(x=thal, color=target)) + geom_bar(fill='#FFFFFF') + theme_minimal()

## Data preaparation

### Splitting the data

set.seed(123)
train.index <- data$target %>% createDataPartition(p =0.8,list =F)
train.data <- data[train.index,]
test.data <- data[-train.index,]

## Machine learning model

### Random Forest

rf.model <- randomForest(target~., data=train.data, ntree=500)

rf.prediction <- predict(rf.model,newdata=test.data)


### Results

confusion.matrix <- table(test.data$target, rf.prediction)
print(confusion.matrix)
print(100*sum(diag(confusion.matrix))/sum(confusion.matrix))
