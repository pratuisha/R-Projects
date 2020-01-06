Cardiotocography_tidy <- read.csv("C:/Users/HP/Desktop/Cardiotocography.csv", header=FALSE)
Cardiotocography_df<- Cardiotocography_tidy
str(Cardiotocography_df)
head(Cardiotocography_df)
set.seed(11)
ind <-sample(2,nrow(Cardiotocography_df),replace = TRUE,prob = c(0.8,0.2))
training <- Cardiotocography_df[ind == 1,]
testing <- Cardiotocography_df[ind == 2,]
library(psych)
pairs.panels(training[,-22],gap=0,bg=c("red","yellow","blue")[training$V22],pch=21)
library(GGally)
ggpairs(Cardiotocography_df[,-22])
#princpla component analysis
pc<-prcomp(training[,-22],center=TRUE,scale = TRUE)
print(pc)
summary(pc)
plot(pc)
pairs.panels(pc$x,gap=0,bg=c("red","yellow","blue")[training$V22],pch=21)
####################  PCA Bipolar Graph   #############################

library(devtools)

library("factoextra")
res.pca <- prcomp(training[, -22],  scale = TRUE)
fviz_pca_var(res.pca)
fviz_pca_var(res.pca, col.var="contrib")+
  scale_color_gradient2(low="blue", mid="red",high="red", midpoint=8.5) 
theme_minimal()
fviz_pca_var(res.pca, alpha.var="contrib") +
  theme_minimal()
###################  PCA Bipolar Graph with data points  #############################
install_github("ggbiplot","vqv")
library(ggbiplot)
g<- ggbiplot(pc,obs.scale = 1,var.scale = 1,groups = training$V22,ellipse = TRUE,circle = TRUE,ellipse.prob = 0.68)
g<- g + scale_color_discrete(name='')
g<- g + theme(legend.direction = 'horizontal',legend.position = 'top')
print(g)
library(caret)
####predict using PCA
trg<-predict(pc,training)
trg<-data.frame(trg,training[22])
tst<-predict(pc,testing)
tst<-data.frame(tst,testing$V22)

training[["V22"]]=factor(training[["V22"]])
trctrl<-trainControl(method ="repeatedcv",number = 10,repeats =3)

#Training
svm_Linear <- train(V22 ~., data = trg, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

svm_Linear
##Training
trg_pred <- predict(svm_Linear, newdata = trg)
trg_pred

confusionMatrix(trg_pred, testing$V22)
Conf<-confusionMatrix(trg_pred, training$V22)
##Testing
tst_pred <- predict(svm_Linear, newdata = tst)
tst_pred
confusionMatrix(tst_pred, testing$V22)


#SVM Classifier using Non-Linear Kernel
set.seed(2098)
svm_Radial <- train(V22 ~., data = trg, method = "svmsigmoid",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
svm_Radial
plot(svm_Radial)
trg_pred <- predict(svm_Radial, newdata = trg)
trg_pred
confusionMatrix(trg_pred, training$V22)
test_pred_Radial <- predict(svm_Radial, newdata = tst)
test_pred_Radial
confusionMatrix(test_pred_Radial, testing$V22 )
##sigmodal

model <- svm(V22 ~., data = trg,scale = TRUE,type = NULL, kernel =
               "sigmoid")
model
trg_pred <- predict(model, newdata = trg)
trg_pred
confusionMatrix(trg_pred, training$V22)
test_pred_sig <- predict(model, newdata = tst)
test_pred_sig
confusionMatrix(test_pred_sig, testing$V22 )




# Random Forest
library(randomForest)
set.seed(222)
random_fr <- train(V22 ~., data = trg, method = "randomForest",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)
rf <- randomForest(V22~., data=trg)
print(rf)
plot(rf)
attributes(rf)

# Prediction & Confusion Matrix - train data
library(caret)
p1 <- predict(rf, trg)
confusionMatrix(p1, training$V22)

# # Prediction & Confusion Matrix - test data
p2 <- predict(rf, tst)
confusionMatrix(p2, testing$V22)

# Error rate of Random Forest
plot(rf)


library(ROCR)

pred_Radial <- ROCR::prediction(test_pred_Radial, testing$V22)
ROCRperf = ROCR::performance(test_pred_Radial, "tpr", "fpr")
plot(ROCRperf)
