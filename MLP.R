
library(caret)
heart_tidy <- read.csv("C:/Users/HP/Desktop/PhD/heart_tidy2.csv", header=FALSE)
heart_df<-heart_tidy
str(heart_df)
head(heart_df)
set.seed(4003)

intrain <-createDataPartition(y=heart_df$V14,p=0.7,list=FALSE)
training <-heart_df[intrain,]
testing <-heart_df[-intrain,]
d <- model.matrix( 
   ~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14, 
  data = training )

dim(training);dim(testing)
anyNA(heart_df)
summary(heart_df)

set.seed(1968)
library(neuralnet)
MLP <- neuralnet(V1 ~ V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14, data = d, hidden=10,threshold=0.01,linear.output=FALSE)
plot(MLP)
output1<-compute(MLP,testing[,-1])
p2<-output1$net.result
p2
pred2<-ifelse(p2>0.5,1,0)
pred2
tab2<-table(pred2,testing$V1)
tab2
sum(diag(tab2))/sum(tab2)
1-sum(diag(tab2))/sum(tab2)
