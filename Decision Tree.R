heart_tidy1 <- read.csv("C:/Users/HP/Desktop/heart_tidy.csv", header=FALSE)
heart_dtree<-heart_tidy1
str(heart_dtree)
head(heart_dtree)
set.seed(4003)
pd<-sample(2,nrow(heart_dtree),replace =TRUE,prob = c(0.7,0.3))
dim(training);dim(testing)
anyNA(heart_dtree)
summary(heart_dtree)
heart_dtree$V14F<-factor(heart_dtree$V14)
set.seed(1234)
train1 <- heart_dtree[pd==1,] 
validate1<-heart_dtree[pd == 2,]
library(party)
#Training Before puring
treebp <- ctree(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=train1)
#training After Purning
tree <- ctree(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=train1, controls = ctree_control(mincriterion = 0.9,minsplit = 50))
#Testing Before puring
treetbp <- ctree(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=validate1)
#Testing After Purning
tree <- ctree(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=validate1, controls = ctree_control(mincriterion = 0.9,minsplit = 50))
tree
plot(tree)
predict(tree,validate1,type="prob")
##rpart 
library(rpart.plot)

#rpart after purning training

tree1<-rpart(formula = V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13, ,data=train1, control = rpart.control(xval = 10,
                                                                 minbucket = 9, cp = 0.01))

tree1
rpart.plot(tree1,2)
# rpart after purning testing
tree1<-rpart(formula = V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13, ,data=validate1, control = rpart.control(minsplit= 50,minbucket=round(50/3), xval = 10, cp = 0.01))

tree1
rpart.plot(tree1,2) 


#training Before Purning
tree1<-rpart(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=train1)
tree1
rpart.plot(tree1,2)
#testing Before Purning
tree1<-rpart(V14F~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13,data=validate1)
tree1
rpart.plot(tree1,2)
predict(tree1,validate1)
#misclassification
tab<-table(predict(treetbp),train1$V14F)
print(tab)
1-sum(diag(tab))/sum(tab)
# misclassification in test data
testpred<- predict(treetbp,newdata=validate1)
tab1<-table(testpred,validate1$V14F)
print(tab1)
1-sum(diag(tab))/sum(tab)

########

install.packages("caret")
library(caret)
library(rpart.plot)




heart_df1 <- heart_tidy1
str(heart_df1)
set.seed(3333)

str(heart_df1)
intrain <-createDataPartition(y=heart_df1$V14F,p=0.7,list=FALSE)
training <-heart_df1[intrain,]
testing <-heart_df1[-intrain,]
dim(training);dim(testing)
anyNA(heart_df1)
summary(heart_df1)


### Training the Decision Tree classifier with criterion as information gain


trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
set.seed(1927)
dtree_fit <- train(V14~.,data = training, method = "rpart",
                   parms = list(split = "information"),
                   trControl=trctrl,
                   tuneLength = 10)
dtree_fit
prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
testing[1,]
predict(dtree_fit, newdata = testing[1,])
test_pred <- predict(dtree_fit, newdata = testing)
confusionMatrix(test_pred, testing$V14)
###Training the Decision Tree classifier with criterion as gini index
set.seed(3333)
dtree_fit_gini <- train(V14 ~., data = training, method = "rpart",
                          parms = list(split = "gini"),
                          trControl=trctrl,
                          tuneLength = 10)
dtree_fit_gini
prp(dtree_fit_gini$finalModel, box.palette = "greens", tweak = 1.2)
test_pred_gini <- predict(dtree_fit_gini, newdata = testing)
confusionMatrix(test_pred_gini, testing$V14)
