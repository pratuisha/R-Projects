library(caret)
heart_tidy <- read.csv("C:/Users/HP/Desktop/heart_tidy.csv", header=FALSE)
heart_df<-heart_tidy
str(heart_df)
head(heart_df)
set.seed(4003)
intrain <-createDataPartition(y=heart_df$V14,p=0.7,list=FALSE)
training <-heart_df[intrain,]
testing <-heart_df[-intrain,]
dim(training);dim(testing)
anyNA(heart_df)
summary(heart_df)
training[["V14"]]=factor(training[["V14"]])
trctrl<-trainControl(method ="repeatedcv",number = 10,repeats =3)
set.seed(1969)
LVQ <- train(V14 ~., data = training, method = "lvq",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
LVQ
##training confusion matrix
train_pred <- predict(LVQ, newdata = training)
train_pred
confusionMatrix(train_pred, training$V14 )
##testing confusion matrix
test_pred <- predict(LVQ, newdata = testing)
test_pred
confusionMatrix(test_pred, testing$V14 )
library(ROCR)
tpr<-76/(76+24)
fpr<-10/(100+10)
roc <-performance(train_pred,tpr,fpr)
per