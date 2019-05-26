#Clearing R enviroment 
rm(list = ls(all = T))

#setting the working directory
setwd("C:/Users/Lenovo/Desktop/Edwisor/Project 3")

#data
read.csv("train_customer.csv", header =  TRUE , stringsAsFactors = FALSE) -> customer

#histogram 
hist(customer$target , col = "blue")

#summary of the data
summary(customer)
head(customer)
colnames(customer)
View(customer)

#removing one variable we removed variable "ID_Code"
customer <- customer[, -1, drop = FALSE]

#Loading the library
library(DMwR)
library(rpart)
install.packages("ggplot")
library(ggplot2)
library(corrgram)
install.packages("ggpubr")
library(ggpubr)
install.packages("devtools")
install.packages("dpylr")
install.packages("C50")
library(C50)
library(MASS)
library(dplyr)
install.packages("RODBC")
library(RODBC)
install.packages("outliers") 
library(outliers)
install.packages("car")
library(car)
install.packages("Boruta")
library(Boruta)
install.packages("Metrics")
library(Metrics)
library(randomForest)
install.packages("ggthemes")
library(ggthemes)


#histgram 
hist(customer$var_0)

#calculating the standard deviation pf the data 
class(customer$var_0)
apply(customer, 2, sd, na.rm = TRUE) -> sd

#checking the count of all the na's in data
sum(is.na(customer))
#we dont have any missing value in the data so it doesn't require to do the NA Removing process

#customer[] <- lapply(customer[], as.numeric)

# now, we can move further for our Second step outliers, we are using the cooks Distance
#first we will bliud the model and on that bases we will bliud the model 

mod <- lm(target ~., data = customer)
cooks.distance(mod) -> cooksd

plot(cooksd, pch = "*", cex = 2, main = "infulential points")
abline(h = 4*mean(cooksd, na.rm = T), col = "red")
text(x=1:length(cooksd)+1, y=cooksd, labels = ifelse(cooksd >4*mean(cooksd, na.rm = T), names(cooksd), ""), col = "red")

influential <- as.numeric(names(cooksd)[(cooksd >4*mean(cooksd, na.rm = T))])
head(customer[influential,])

#we doing outliers test using car package 
car:: outlierTest(mod)
#it show that 94184 in this row it has the most extreme values  

#imputation of the outliers we are using the capping function
x<- as.data.frame(customer)
caps <- data.frame(apply(customer,2, function(x){
  quantiles <- quantile(x, c(0.25, 0.75))
x[x < quantiles[1]] <- quantiles[1]
x[x > quantiles [2]] <- quantiles[2]
  }))

caps

#step 3 doing the correlation 
#checking the VIF  
library(usdm)
vif(customer[,-1])
VIF <- order(VIF)
View(VIF)
# it shows the 1.3/4 value for each variable

#feature selection of the data using the Boruta method
#Boruta method 
set.seed(123)
Boruta.train <- Boruta(target~., data = customer, doTrace = 2)
print(Boruta.train)

#Boruta performed 99 iterations in 7.526 hrs.
#2 attributes confirmed important: var_170, var_99;
#196 attributes confirmed unimportant: var_0, var_1, var_10, var_100, var_101 and 191 more;
#2 tentative attributes left: var_114, var_92;

#Ploting graph 
plot(Boruta.train, xlab = "", xaxt = "n")
lz<-lapply(1:ncol(Boruta.train$ImpHistory),function(i)
Boruta.train$ImpHistory[is.finite(Boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(Boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
 at = 1:ncol(Boruta.train$ImpHistory), cex.axis = 0.7)

#tentative variable test 
final.boruta <- TentativeRoughFix(Boruta.train)
print(final.boruta)
getSelectedAttributes(final.boruta, withTentative = F)
boruta.df <- attStats(final.boruta)

#selecting variables 
customer <- data.frame(customer$target, customer$var_99, customer$var_170)
colnames(customer)
View(customer)

#normalization method
cnames = c("customer.var_99", "customer.var_170")
for (i in cnames) {
  customer [,i] = (customer[,i] - min(customer[,i]))/ (max(customer[,i] - min(customer[,i])))
  
}
head(customer)

##Building model
#Decision Tree
#creating smaple for the data using simple sample method because all variables are continous
#we are using only Train data for our testing of data will use Test data later

library(caret)
library(C50)
library(DataCombine)

rmExcept("customer")

customer <- lapply(customer, as.factor)

customer <- as.data.frame(customer)
train_index = sample(1:nrow(customer), 0.8*nrow(customer))
train_data = customer[train_index,]
test_data = customer[-train_index,]

C50 <- C5.0(customer.target~., train_data, trails = 100, rules = TRUE)
write(capture.output(summary(C50)), "summary_customer.text")
prediction <- predict(C50, test_data[,-1], type = "class")

#error matrix 
install.packages("e1071")
library(e1071)

conmatrix <- table(test_data$customer.target, prediction)
confusionMatrix(conmatrix)

#false negative rate 
#FNR <- FN/(FN+TP)
FNR <- 88/(88+68) 
#tPR = tp+(tp+fn)
tpr = 893/(893+107)
tpr

#FNR = 56.41
#accuracy = 89.3%
#tpr = 89.3%

#Random forest 

library(DMwR)
install.packages("inTrees")
library(inTrees)
install.packages("caret")
library(caret)
library(randomForest)

rmExcept("customer")

customer$customer.target <- as.factor(customer$customer.target)
customer$customer.var_99 <- as.numeric(customer$customer.var_99)
customer$customer.var_170 <- as.numeric(customer$customer.var_170)

customer<- as.numeric(as.character(customer))
customer<- as.data.frame(customer)

train_index = sample(1:nrow(customer), 0.8*nrow(customer))
train_data = customer[train_index,]
test_data = customer[-train_index,]

RF <- randomForest(customer.target~., train_data, importance = TRUE)
treelist <- RF2List(RF)
rules <- extractRules(treelist, train_data[,-1])
rules[5,]

readrule <- presentRules(rules, colnames(train_data))
head(readrule)

rulematrix <- getRuleMetric(rules, train_data[,-1], train_data$customer.target)
head(rulematrix)

prediction <- predict(RF, test_data[,-1])

confmt <- table(test_data$customer.target, prediction)

confusionMatrix(confmt)

conmatrix(confusionmt)

summary(RF)

#accuracy = 88.4%
#true postive rate 
#tPR = tp+(tp+fn)
tpr = 884/(884+110)
tpr
#tpr = 88.93%
#fnr = 11.06%