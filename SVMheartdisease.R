library(caret)
Cardiotocography_tidy <- read.csv("C:/Users/HP/Desktop/Cardiotocography.csv", header=FALSE)
Cardiotocography_df<- Cardiotocography_tidy
str(Cardiotocography_df)
head(Cardiotocography_df)
library(corrplot) 
C<- cor(Cardiotocography_df[,-22]) 
corrplot(C, method="circle")
set.seed(3002)
intrain <-createDataPartition(y=Cardiotocography_df$V22,p=0.7,list=FALSE)
training <-Cardiotocography_df[intrain,]
testing <-Cardiotocography_df[-intrain,]
dim(training);dim(testing)
anyNA(Cardiotocography_df)
summary(Cardiotocography_df)
training[["V22"]]=factor(training[["V22"]])
trctrl<-trainControl(method ="repeatedcv",number = 10,repeats =3)
set.seed(1969)
svm_Linear <- train(V22 ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Linear
test_pred <- predict(svm_Linear, newdata = trg)
test_pred
confusionMatrix(test_pred, testing$V22)
grid <- expand.grid(C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2,5))
set.seed(1969)
svm_Linear_Grid <- train(V22 ~., data = training, method = "svmLinear",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid,
                           tuneLength = 10)

svm_Linear_Grid
plot(svm_Linear_Grid)
test_pred_grid <- predict(svm_Linear_Grid, newdata = testing)
test_pred_grid
confusionMatrix(test_pred_grid, testing$V22 )
#SVM Classifier using Non-Linear Kernel
set.seed(2098)
svm_Radial <- train(V22 ~., data = training, method = "svmRadial",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
svm_Radial
plot(svm_Radial)
test_pred_Radial <- predict(svm_Radial, newdata = testing)

confusionMatrix(test_pred_Radial, testing$V22 )
# test & tune our classifier with different values of C & sigma
grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
set.seed(2090)
svm_Radial_Grid <- train(V22 ~., data = training, method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_radial,
                           tuneLength = 10)
svm_Radial_Grid
plot(svm_Radial_Grid)
 #check our trained models' accuracy on the test set.
test_pred_Radial_Grid <- predict(svm_Radial_Grid, newdata = testing)
confusionMatrix(test_pred_Radial_Grid, testing$V22 )
