## Descriptive Analysis
install.packages("glmnet")
install.packages('clusterGeneration')
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("Hmisc")
install.packages("flexclust")
install.packages("NbClust")
install.packages("janitor")
install.packages("reshape")
install.packages("lares")

library(ggstatsplot)
library(reshape)
library(readxl)
library(tidyverse) 
library(ggcorrplot) # for the ggcorrplot function 
library(interactions) 
library(Hmisc)
library("purrr")
library(glmnet)
require(MASS)
require(clusterGeneration)
library(dplyr)
library(ggplot2)
library(stats)
library(ggfortify)
library(NbClust)
library(flexclust)
library(janitor)
library(lares)
library(gridExtra)
library(scales)
library(corrplot)


# Input data
library(readxl)

View(satisfaction)
glimpse(satisfaction)
summary(satisfaction)
#satisfaction <- read_excel("satisfaction.xlsx")
data1 <- satisfaction
customer_data <- satisfaction


## View data
View(satisfaction)
glimpse(satisfaction)

## Check Summary Statistics
summary(satisfaction)

#(1)MISSING VALUES

#total no of missing values in table
sum(is.na(satisfaction))

#Find the column names with missing values
colnames(satisfaction)[colSums(is.na(satisfaction)) > 0]
names(which(colSums(is.na(satisfaction)) > 0))

#count total missing values in 'Arrival Delay in Minutes' column
sum(is.na(satisfaction$`Arrival Delay in Minutes`))

#identify locations of missing values in 'Arrival Delay in Minutes' column
which(is.na(satisfaction$`Arrival Delay in Minutes`))

#Remove rows with NA (missing) values 
data = na.omit(satisfaction) # Method 1 - Remove NA
data1 = na.omit(satisfaction)
view(data)

#Convert character variables to factors
data <- data %>% 
  mutate(satisfaction_v2 = factor(satisfaction_v2))

data <- data %>% 
  mutate(Gender = factor(Gender))

data <- data%>% 
  mutate(`Customer Type` = factor(`Customer Type`))

data <- data %>% 
  mutate(`Type of Travel`= factor(`Type of Travel`))

data <- data %>% 
  mutate(Class = factor(Class))

#Convert non-numeric variables to numeric
data$satisfaction_v2<- as.numeric(as.factor(data$satisfaction_v2))
data$Gender <- as.numeric(as.factor(data$Gender))
data$`Customer Type`<- as.numeric(as.factor(data$`Customer Type`))
data$`Type of Travel` <- as.numeric(as.factor(data$`Type of Travel`))
data$Class <- as.numeric(as.factor(data$Class))

data <- as.data.frame(data)

# Creating new column titled Satisfaction similar to satisfaction_v2
data$Satisfaction <- data$satisfaction_v2

# Create new dataframe with only numeric variables by removing satisfaction_v2
data = select(data, -c(satisfaction_v2))
data = select(data, -c(3,5))

## Descriptive Analysis

# 2-way table 
data %>%                               # Summary by group using purrr
  split(.$Satisfaction) %>%
  map(summary)


# Correlation 
correlationMa
rix <- cor(data[,])
view(correlationMatrix)


## Create data1 where target is a factor
#Convert character variables to factors
data1 <- data1 %>% 
  mutate(satisfaction_v2 = factor(satisfaction_v2))

data1 <- data1 %>% 
  mutate(Gender = factor(Gender))

data1 <- data1%>% 
  mutate('Customer Type' = factor('Customer Type'))

data1 <- data1 %>% 
  mutate('Type of Travel'= factor('Type of Travel'))

data1 <- data1 %>% 
  mutate(Class = factor(Class))

#Convert non-numeric variables to numeric
data1$Gender <- as.numeric(as.factor(data1$Gender))
data1$'Customer Type' <- as.numeric(as.factor(data1$'Customer Type'))
data1$'Type of Travel' <- as.numeric(as.factor(data1$'Type of Travel'))
data1$Class <- as.numeric(as.factor(data1$Class))


control <- trainControl(method="repeatedcv", number=10, repeats=3)
data1 <- as.data.frame(data1)
model <- train(satisfaction_v2 ~ ., method = "glm", data = data1)
summary(model)

# Significance
model1 <- train(Satisfaction ~ ., method = "glm", data = data)
summary(model1)
varImp(model1, scale = TRUE)


rm(model)

model
varImp(model, scale = TRUE)

####3 use of variation inflation factor 

set.seed(2)
num.vars<-15
num.obs<-200
cov.mat<-genPositiveDefMat(num.vars,covMethod="unifcorrmat")$Sigma
rand.vars<-mvrnorm(num.obs,rep(0,num.vars),Sigma=cov.mat)

m<-lm(Satisfaction~.,data=data)
summary(m)
library(car)

vif(m)

##Lasso Regression
xtrain <- data1[ -c(2) ] #taking all columns without dependent variable  
ytrain <- data1 [c(2)] #taking the dependent variable
xtrain <- as.matrix(xtrain) #converting to matrix

#cross validation to find optimal lambda
lasreg = cv.glmnet(x= xtrain, y = ytrain,family = c("binomial"), aplha = 1,
                   nlambda = 100)

#fit values
fit = glmnet(x= xtrain,y = ytrain,family = c("binomial"), aplha = 1,
             lambda = lasreg$lambda.1se)

fit$beta[,1]   

#pos & neg correlated variables related to their coefficients
coef(lasreg, s = "lambda.1se")%>%
  tidy()%>%
  filter(row !="(Intercept)")%>%
  ggplot(aes(value, reorder(row,value),color = value>0))+
  geom_point(show.legend = FALSE)+
  ggtitle("feature variables")+
  xlab("Coefficient")+
  ylab(NULL)

# Welch's Two Sample T-Test
#T-Test
t.test(data$'Inflight wifi service' ~ data$Satisfaction)
t.test(data$Age ~ data$Satisfaction)
t.test(data$'Flight Distance' ~ data$Satisfaction)
t.test(data$'Departure/Arrival time convenient' ~ data$Satisfaction)
t.test(data$'Departure Delay in Minutes' ~ data$Satisfaction)
t.test(data$'Arrival Delay in Minutes' ~ data$Satisfaction)
t.test(data$'Inflight entertainmen' ~ data$Satisfaction)
t.test(data$'Online support' ~ data$Satisfaction)
t.test(data$'Ease of Online booking' ~ data$Satisfaction)
t.test(data$'On-board service' ~ data$Satisfaction)
t.test(data$'Gate location' ~ data$Satisfaction)
t.test(data$'Food and drink' ~ data$Satisfaction)
t.test(data$'Online boarding' ~ data$Satisfaction)
t.test(data$'Cleanliness' ~ data$Satisfaction)
t.test(data$'Baggage handling' ~ data$Satisfaction)
t.test(data$'Checkin service' ~ data$Satisfaction)
t.test(data$'Seat comfort' ~ data$Satisfaction)
t.test(data$'Inflight wifi service' ~ data$Satisfaction)
t.test(data$'Leg room service' ~ data$Satisfaction)

# Visualization

# Correlation Plot
corrplot(X, method = 'circle', type = "upper", order = 'AOE', tl.col = "black", tl.srt = 90, insig = 'p-value') ###Correlation matrix Graph 1

# Creating new Variables
satisfaction$Satisfaction <- satisfaction$`satisfaction_v2`
satisfaction$Customer.Type <- satisfaction$`Customer Type`
satisfaction$Type.of.Travel <- satisfaction$`Type of Travel`

# Taking out duplicate variables
satisfaction = select(satisfaction, -c(satisfaction_v2,`Type of Travel`,`Customer Type` ))

# Barcharts + Cramer's V and Chi-Square Test
BarChart(Gender, data=satisfaction, by=Satisfaction, theme=c("light"))

BarChart(Class, data=satisfaction, by= Satisfaction) 
BarChart(Customer.Type, data=satisfaction, by= Satisfaction,  ylab='Count of Customer Type', xlab='Customer Type')
BarChart(Type.of.Travel, data=satisfaction, by= Satisfaction,  ylab='Count of Type of Travel', xlab='Type of Travel')

#Barplots 
barplot1 <- ggplot(satisfaction, aes(x=Satisfaction, fill = Satisfaction)) + geom_bar(position="dodge") +  theme_dark()
barplot1

# Customer Type by Satisfaction

p1 <- ggplot(satisfaction, 
             aes(x = Customer.Type, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="dodge") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of Customers", 
       fill = "Satisfaction",
       x = "Customer Type",
       title = "Number of customer type by customer satisfaction") +
  theme_minimal()
p1

# Proportion of Customer Type
p2 <- ggplot(satisfaction, 
             aes(x = factor(Customer.Type,
                            labels = c("Disloyal", "Loyal Customer")), 
                 fill = satisfaction_v2)) + 
  geom_bar(position="fill")  +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Percentage", 
       fill = "Satisfaction",
       x = "Customer Type",
       title = "Proportions of customer types by customer satisfaction") +
  theme_minimal()
p2



# Gender by Satisfaction
p3 <- ggplot(satisfaction, 
             aes(x = Gender, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="dodge") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of Customers", 
       fill = "Satisfaction",
       x = "Gender",
       title = "Number of customers in each gender by satisfaction") +
  theme_minimal()
p3


# Proportion of Gender by Satisfaction
p4 <- ggplot(satisfaction, 
             aes(x = Gender, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="fill")  +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Percentage", 
       fill = "Satisfaction",
       x = "Gender",
       title = "Proportions of gender by customer satisfaction") +
  theme_minimal()
p4



# Type of Travel Vs. customer satisfaction
p5 <- ggplot(satisfaction, 
             aes(x = Type.of.Travel, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="dodge") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of Customers", 
       fill = "Satisfaction",
       x = "Type of Travel",
       title = "Number of customers in each travel type by satisfaction") 
p5


# Proportion
p6 <- ggplot(satisfaction, 
             aes(x = Type.of.Travel, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="fill")  +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Percentage", 
       fill = "Satisfaction",
       x = "Type of Business",
       title = "Proportions of business types by satisfaction") 
p6


# Class Type by Satisfaction Level
p7 <- ggplot(satisfaction, 
             aes(x = Class, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="dodge") +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") +
  labs(y = "Number of Customers", 
       fill = "Satisfaction",
       x = "Class",
       title = "Number of customers in each class by satisfaction") 
p7

# Proportion of Customer Type
p8 <- ggplot(satisfaction, 
             aes(x = Class, 
                 fill = satisfaction_v2)) + 
  geom_bar(position="fill")  +
  scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(y = "Percentage", 
       fill = "Satisfaction",
       x = "Class",
       title = "Proportions of class by satisfaction") +
  theme_minimal()

p8

## Correlation Plot (Visualisation)


cormat <- round(cor(cordata),2) # correlation matrix with only numeric variables

Upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

upper_tri <- Upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 14, hjust = 1))+
  coord_fixed()

# correlogram
plot <- ggcorrmat( data = cordata,
                   type = "parametric", # parametric for Pearson
                   colors = c("darkred", "white", "steelblue") #default colors
)
plot

# display top 10 couples of variables by correlation coefficient at 5% significant level
top_10 <- corr_cross(clean,
                     max_pvalue = 0.05, 
                     top = 10 )
top_10

## Correlation by Satisfaction.
satis_corr <- corr_var(data, 
                       Satisfaction)
satis_corr

##correlation 
cor_mat <-
  data %>% 
  select(where(is.numeric), -c(id)) %>% 
  cor(use = "pairwise.complete.obs") 

corrplot(
  title = "\n\nCorrelation Matrix",
  cor_mat,
  method = "number",
  order = "alphabet",
  type = "lower",
  diag = FALSE,
  number.cex = 0.7,
  tl.cex = 0.8,
  tl.col = "darkgreen",
  addgrid.col = "gray"
)

#Barplots
##1.Cleanliness
count1 = table(data$Cleanliness,data$Satisfaction)
barplot(count1,main = "Satisfaction over Cleanliness",
        names.arg = c("Unsatisfied","Satisfied"),
        xlab="Ratings",
        col =rainbow(6),
        density = 40,
        legend.text = rownames(count1),
        space = c(0.25, 2.5),
        beside=TRUE)

#2
count2 = table(data$`Inflight entertainment`,data$Satisfaction)
barplot(count2,main = "Satisfaction over Inflight entertainment",
        names.arg = c("Unsatisfied","Satisfied"),
        xlab="Ratings",
        col =rainbow(6),
        density = 40,
        legend.text = rownames(count2),
        space = c(0.25, 2.5),
        beside=TRUE)

#3.
count3 = table(data$`Leg room service`,data$Satisfaction)
barplot(count3,main = "Satisfaction over Leg room service",
        names.arg = c("Unsatisfied","Satisfied"),
        xlab="Ratings",
        col =rainbow(6),
        density = 40,
        legend.text = rownames(count3),
        space = c(0.25, 2.5),
        beside=TRUE)

#4.
count4 = table(data$`Seat comfort`,data$Satisfaction)
barplot(count4,main = "Satisfaction over Seat comfort",
        names.arg = c("Unsatisfied","Satisfied"),
        xlab="Ratings",
        col =rainbow(6),
        density = 40,
        legend.text = rownames(count4),
        space = c(0.25, 2.5),
        beside=TRUE)

#5.
count5 = table(data$`Checkin service`,data$Satisfaction)
barplot(count5,main = "Satisfaction over Checkin services",
        names.arg = c("Unsatisfied","Satisfied"),
        xlab="Ratings",
        col =rainbow(6),
        density = 40,
        legend.text = rownames(count5),
        space = c(0.25, 2.5),
        beside=TRUE)






# Density/ histogram plots
#1
hist(data$`Flight Distance`,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Flight Distance Distribution plot",
     xlab = "Flight Distance")
lines(density(data$`Flight Distance`),lwd = 2, col = "red")

#2.
hist(data$`Departure Delay in Minutes`,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Departure Delay Distribution plot",
     xlab = "Departure Delay")
lines(density(data$`Departure Delay in Minutes`),lwd = 2, col = "red")

#3.
hist(data$`Arrival Delay in Minutes`,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Arrival Delay Distribution plot",
     xlab = "Arrival Delay")
lines(density(data$`Arrival Delay in Minutes`),lwd = 2, col = "red")

#4.
hist(data$Age,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Age Distribution plot",
     xlab = "Age")
lines(density(data$Age),lwd = 2, col = "red")


#Barplot for outliers
#let's look at the plot of outliers in numerical variables
cleaned_num<-select_if(data,is.numeric)%>%select(-id)
cleaned_num_p<-cleaned_num %>% gather(variable,values,1:18 )
options(repr.plot.width = 14, repr.plot.height = 8)

ggplot(cleaned_num_p)+
  geom_boxplot(aes(x=variable,y=values),fill="cadetblue") + 
  facet_wrap(~variable,ncol=6,scales="free") + 
  theme(strip.text.x = element_blank(),
        text = element_text(size=14))

#Checking histograms/ density plots of numeric variables 
#1.
hist(data$`Departure Delay in Minutes`,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Departure Delay Distribution plot",
     xlab = "Departure Delay")
lines(density(data$`Departure Delay in Minutes`),lwd = 2, col = "red")

#2.
hist(data$`Arrival Delay in Minutes`,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Arrival Delay Distribution plot",
     xlab = "Arrival Delay")
lines(density(data$`Arrival Delay in Minutes`),lwd = 2, col = "red")

#3.
hist(data$Age,
     col = c("#009999"),
     border = "black",
     prob = TRUE,
     main = "Age Distribution plot",
     xlab = "Age")
lines(density(data$Age),lwd = 2, col = "red")


###############################Model building##########################################


###############################decision tree########################
library(readxl)

View(satisfaction)
glimpse(satisfaction)
summary(satisfaction)
data = satisfaction

xtrain = data %>%select(-c('Arrival Delay in Minutes','Departure Delay in Minutes','Flight Distance','id','Age'))


#unique categories in categorical 
unique(xtrain$satisfaction_v2)
xtrain$satisfaction_v2<-ifelse(xtrain$satisfaction_v2=="satisfied",1,0)

unique(xtrain$Gender)
xtrain$Gender<-ifelse(xtrain$Gender=='Female',0,1)


unique(xtrain$`Customer Type`)
xtrain$`Customer Type`<-ifelse(xtrain$`Customer Type`=='disloyal Customer',0,1)

unique(xtrain$`Type of Travel`)
xtrain$`Type of Travel`<-ifelse(xtrain$`Type of Travel`=='Personal Travel',0,1)

unique(xtrain$Class)
xtrain$Class <- as.numeric(factor(xtrain$Class))
unique(xtrain$Class)
######
set.seed(100)
training = createDataPartition(xtrain$satisfaction_v2,p=0.80,list=FALSE)
Train = xtrain[training,]
Test = xtrain[-training,]

Test = data.frame(Test)

#required libraries for decision tree
library(rpart)
library(rpart.plot)
library(caret)




#Running Decision trees
#running the model with significant variables


dtree = rpart(satisfaction_v2~., data=Train, method = 'class', minbucket = 25)
#Visualizing the Decision tree
#method 1
prp(dtree)
#method 2
rpart.plot(dtree)


#Optimization Part
# Defining cross-validation experiment
ctrl = trainControl( method = "cv", number = 10 )
cpGrid = expand.grid( .cp = seq(0.01,0.5,0.01))


FS = train(satisfaction_v2~.,data = Train,method = "rpart",trControl = ctrl, tuneLength = 25, tuneGrid = cpGrid )

#Analyzing the Importance of variable using the Variable Importance Plot
print(varImp(FS))

#plotting feature selection
plot(varImp(FS))

# Performing the cross validation
dtree2 = train(satisfaction_v2~., data = Train,method = 'rpart',trControl = ctrl, tuneLength = 25, tuneGrid = cpGrid )

print(dtree2)

#####Predicting the Values on the Train data
trainpredict = predict(dtree, Train)

#Confusion Matrix table to find accuracy for training data
table(Train$satisfaction_v2, trainpredict[,2]>0.7)

model_accuracy = (51370+36132)/(51370+36132+9834+6568)
model_accuracy #0.8421427

model_sensitivity = (36132)/(9834+36132)
model_sensitivity #0.7860593

model_precision = (36132)/(6568+36132)
model_precision #0.8461827

model_specificity = 51370/(51370+6568)
model_specificity # 0.8866374

model_F.measure = (2*model_sensitivity*model_precision)/(model_sensitivity+model_precision)
model_F.measure #0.8150136

#####Predicting the Values on the Test.Avg data
PredictROC = predict(dtree,Test)

#Confusion Matrix table to find accuracy for test data
table(Test$satisfaction_v2, PredictROC[,2] > 0.7)

accuracy = (2988+22133)/(2988+22133+348+507)
accuracy #0.967085

sensitivity = 22133/(2988+22133)
sensitivity #0.8810557

precision = 22133/(22133+507)
precision #0.977606

specificity = 348/(348+507)
specificity #0.4070175

F.measure = (2*sensitivity*precision)/(sensitivity+precision)
F.measure #0.9268231


############################logistic regression#############################################

log_model<- glm(satisfaction_v2~.,data=Train,family=binomial(link='logit'))
summary(log_model)

Train$new_pred_tr<- predict(log_model, Train,type='response')


Train<- Train %>% mutate(new_pred_trr= 1*(new_pred_tr > .53)+ 0)
Train <- Train %>% mutate(accurate =1*(new_pred_trr==satisfaction_v2))
sum(Train$accurate)/nrow(Train)
#0.809
h_train <- roc(Train$satisfaction_v2, Train$new_pred_trr)
h_train
plot(h_train)
#Area under the curve: 0.8023
#install.packages('InformationValue')
library(InformationValue)


optCutOff <- optimalCutoff(Train$satisfaction_v2, Train$new_pred_trr)[1] 
#sensitivity(Train$satisfaction_v2, Train$new_pred_trr, threshold = optCutOff)

##model evaluation
cm <- table(Train$new_pred_trr, Train$satisfaction_v2)
cm

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
accuracy ##0.8082
precision <- cm[4] / sum(cm[4], cm[2])
precision##0.8023
sensitivity <- cm[4] / sum(cm[4], cm[3])
sensitivity #0.7517
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
fscore #0.789
specificity <- cm[1] / sum(cm[1], cm[2])
specificity #0.8522


misClassError(Train$satisfaction_v2, Train$new_pred_trr, threshold = optCutOff)
#0.1906
plotROC(Train$satisfaction_v2, Train$new_pred_trr)



###test data evaluation

Test$model_prob<- predict(log_model,Test, type='response')


Test<- Test %>% mutate(model_pred= 1*(model_prob > .53)+ 0)
Test <- Test %>% mutate(accurate =1*(model_pred==satisfaction_v2))
sum(Test$accurate)/nrow(Test)
#0.93


#install.packages('InformationValue')
#library(InformationValue)


optCutOff <- optimalCutoff(Test$satisfaction_v2, Test$model_pred)[1] 
sensitivity(Test$satisfaction_v2, Test$model_pred, threshold = optCutOff)

##model evaluation
cm <- table(Test$model_pred, Test$satisfaction_v2)
cm

accuracy <- sum(cm[1], cm[4]) / sum(cm[1:4])
accuracy ##0.934
precision <- cm[4] / sum(cm[4], cm[2])
precision## 0.9774
sensitivity <- cm[4] / sum(cm[4], cm[3])
sensitivity #0.953
fscore <- (2 * (sensitivity * precision))/(sensitivity + precision)
fscore #0.965
specificity <- cm[1] / sum(cm[1], cm[2])
specificity #0.3532




misClassError(Test$satisfaction_v2, Test$model_pred, threshold = optCutOff)
plotROC(Test$satisfaction_v2, Test$model_pred)

h <- roc(Test$satisfaction_v2, Test$model_pred)
h #0.6535
plot(h)



############################Random Forest###############################

## LIBRARIES
library(readxl)
install.packages("ggcorrplot")
library(readxl)
library(tidyverse) 
library(ggcorrplot) # for the ggcorrplot function 
library(interactions) 
install.packages("caret")
library(caret)
install.packages("Hmisc") # forCorrelation between variables
library(Hmisc)

#Load Data
data <- data.frame(satisfaction)
View(data)
glimpse(satisfaction$satisfaction_v2)

#convert char to factor
data$satisfaction_v2 = as.factor(data$satisfaction_v2)
data$Gender = as.factor(data$Gender)
data$`Customer Type` = as.factor(data$`Customer Type`) 
data$`Type of Travel` = as.factor(data$`Type of Travel`)
data$Class = as.factor(data$Class)

#Convert non-numeric variables to numeric
#data$satisfaction_v2<- as.numeric(as.factor(data$satisfaction_v2))
#data$Gender <- as.numeric(as.factor(data$Gender))
#data$`Customer Type`<- as.numeric(as.factor(data$`Customer Type`))
#data$`Type of Travel` <- as.numeric(as.factor(data$`Type of Travel`))
#data$Class <- as.numeric(as.factor(data$Class))

#removing unwanted attributes
d1 <- data



#training and test
training = createDataPartition(d1$satisfaction_v2,p=0.80,list=FALSE)
Train = d1[training,]
Test = d1[-training,]


library(ggplot2)
library(cowplot)
library(randomForest)

set.seed(100)  # Setting seed

#On training data
r <- randomForest(satisfaction_v2 ~ Gender + Customer.Type +
                    Type.of.Travel+ Class+ Inflight.wifi.service + 
                    Departure.Arrival.time.convenient +Ease.of.Online.booking+
                    Gate.location + Food.and.drink + Online.boarding + Seat.comfort +
                    Inflight.entertainment +On.board.service + Leg.room.service +
                    Baggage.handling + Checkin.service + Cleanliness +Online.support, data=Train, importance=TRUE, do.trace=100, ntree=1000)

print(r)
confusionMatrix(predict(r, Train), Train$satisfaction_v2)
confusionMatrix(predict(r, Test), Test$satisfaction_v2)


#plotting feature selection
varImpPlot(r)
plot(r)

Imp<-data.frame(importance(r))
write.csv(Imp,'C:\\Users\\sharm\\Desktop\\BA Sem 2\\ML & AI\\Assessment\\imprf.csv')

#Checking for Data balancing
barplot(prop.table(table(data$satisfaction_v2)),
        col = rainbow(2),
        ylim = c(0, 0.7),
        main = "Class Distribution")

table(Train$satisfaction_v2)

barplot(prop.table(table(Train$satisfaction_v2)),
        col = "green",
        ylim = c(0, 0.7),
        main = "Class Distribution")

prop.table(table(Train$satisfaction_v2))









#########################################Unsupervised- Kmeans##############################

#############################K-means with all variables####################


# Install pre-requisite package #
install.packages("stats")
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("Hmisc")
install.packages("flexclust")
install.packages("NbClust")
install.packages("janitor")

# Load requyired libraries #
library(stats)
library(dplyr)
library(ggplot2)
library(ggfortify)
library(Hmisc)
library(tidyverse)
library(NbClust)
library(flexclust)
library(janitor)


# Unsupervised learning = Hence converting data to unlabbeled #

#total no of missing values in table
sum(is.na(satisfaction))
satisfactionn<-data.frame(satisfaction)
#Find the column names with missing values
colnames(satisfactionn)[colSums(is.na(satisfactionn)) > 0]
names(which(colSums(is.na(satisfactionn)) > 0))

#count total missing values in 'Arrival Delay in Minutes' column
sum(is.na(satisfactionn$`Arrival Delay in Minutes`))

#identify locations of missing values in 'Arrival Delay in Minutes' column
which(is.na(satisfactionn$`Arrival Delay in Minutes`))

#(2)Remove rows with NA (missing) values 
data1 = na.omit(satisfactionn) # Method 1 - Remove NA
View(data1)

#(3)Convert character variables to factors
data1 <- data1 %>% 
  mutate(satisfaction_v2 = factor(satisfaction_v2))

data1 <- data1 %>% 
  mutate(Gender = factor(Gender))

data1 <- data1%>% 
  mutate(`Customer.Type` = factor(`Customer.Type`))

data1 <- data1 %>% 
  mutate(`Type.of.Travel`= factor(`Type.of.Travel`))

data1 <- data1 %>% 
  mutate(Class = factor(Class))

#Convert non-numeric variables to numeric
data1$satisfaction_v2<- as.numeric(as.factor(data1$satisfaction_v2))
data1$Gender <- as.numeric(as.factor(data1$Gender))
data1$`Customer.Type`<- as.numeric(as.factor(data1$`Customer.Type`))
data1$`Type.of.Travel` <- as.numeric(as.factor(data1$`Type.of.Travel`))
data1$Class <- as.numeric(as.factor(data1$Class))

unique(data1$Type.of.Travel)

#Converting Customer type and Type of travel
data1$Customer.Type[data1$Customer.Type == "Loyal Customer" ]<- 1
data1$Customer.Type[data1$Customer.Type == "disloyal Customer" ]<- 0

data1$Type.of.Travel[data1$Type.of.Travel == "Personal Travel" ]<- 1
data1$Type.of.Travel[data1$Type.of.Travel == "Business travel" ]<- 0

#converting into numeric for standardising the data
data1$Type.of.Travel<-as.numeric(data1$Type.of.Travel)
data1$Customer.Type<-as.numeric(data1$Customer.Type)

#Now we will be normalising the data so it is not bias
data_std<- scale(data1)

#Data after removing the Id
data_std<-data1[-c(1)]

rcorr(as.matrix(data_std))

glimpse(data_std)

# WSS plot for choosing optimum number of clusters #
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2 :nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc, wss, type='b', xlab="Number of clusters",
       ylab="within groups sum of squares")
}

wssplot(data_std)

# K-means cluster #
km = kmeans(data_std,4)
km <- kmeans(data_std, centers = 4,iter.max = 1000, nstart = 100)
table(km$cluster)
print(km)

# Evaluating cluster analysis #

# cluster plot
autoplot(km,data_std,frame='True')

# cluster centers
km$centers




##################################K-means with selected variables####################

#total no of missing values in table
view(satisfaction)
sum(is.na(satisfaction))
satisfactionnn<-data.frame(satisfaction)

#Find the column names with missing values
colnames(satisfactionnn)[colSums(is.na(satisfactionnn)) > 0]
names(which(colSums(is.na(satisfactionnn)) > 0))

#count total missing values in 'Arrival Delay in Minutes' column
sum(is.na(satisfactionnn$`Arrival Delay in Minutes`))

#identify locations of missing values in 'Arrival Delay in Minutes' column
which(is.na(satisfactionnn$`Arrival Delay in Minutes`))

#(2)Remove rows with NA (missing) values 
dataa = na.omit(satisfactionnn) # Method 1 - Remove NA
View(dataa)

#(3)Convert character variables to factors
dataa <- dataa %>% 
  mutate(satisfaction_v2 = factor(satisfaction_v2))

dataa <- dataa %>% 
  mutate(Gender = factor(Gender))

dataa <- dataa%>% 
  mutate(`Customer.Type` = factor(`Customer.Type`))

dataa <- dataa %>% 
  mutate(`Type.of.Travel`= factor(`Type.of.Travel`))

dataa <- dataa %>% 
  mutate(Class = factor(Class))

#Convert non-numeric variables to numeric
dataa$satisfaction_v2<- as.numeric(as.factor(dataa$satisfaction_v2))
dataa$Gender <- as.numeric(as.factor(dataa$Gender))
dataa$`Customer.Type`<- as.numeric(as.factor(dataa$`Customer.Type`))
dataa$`Type.of.Travel` <- as.numeric(as.factor(dataa$`Type.of.Travel`))
dataa$Class <- as.numeric(as.factor(dataa$Class))

unique(dataa$Type.of.Travel)

#Converting Customer type and Type of travel
dataa$Customer.Type[dataa$Customer.Type == "Loyal Customer" ]<- 1
dataa$Customer.Type[dataa$Customer.Type == "disloyal Customer" ]<- 0

dataa$Type.of.Travel[dataa$Type.of.Travel == "Personal Travel" ]<- 1
dataa$Type.of.Travel[dataa$Type.of.Travel == "Business travel" ]<- 0

#converting into numeric for standardising the data
dataa$Type.of.Travel<-as.numeric(dataa$Type.of.Travel)
dataa$Customer.Type<-as.numeric(dataa$Customer.Type)

#Now we will be normalising the data so it is not bias
#data_std<- scale(data)
dataa
#Data after removing the Id
dataa <- dataa[-c(1,5,8,23,24)]
glimpse(dataa)
#Now we will be normalising the data so it is not bias
dataa_std<-scale(dataa)

rcorr(as.matrix(dataa_std))

#glimpse(dataa)

# WSS plot for choosing optimum number of clusters #
wssplot <- function(data, nc=15, seed=1234)
{
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for (i in 2 :nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)}
  plot(1:nc, wss, type='b', xlab="Number of clusters",
       ylab="within groups sum of squares")
}

wssplot(dataa_std)

# K-means cluster #
km = kmeans(dataa_std,7)
km <- kmeans(dataa_std, centers = 7,iter.max = 1000, nstart = 100)
table(km$cluster)
print(km)
# Evaluating cluster analysis #

# cluster plot
autoplot(km,dataa_std,frame='True')

# cluster centers
km$centers





