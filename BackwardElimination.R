Fitall<- lm(age ~ sex+cp+trestbps+chol+fbs+restecg+thalach+e,data=Allheartdiseases)
summary(Fitall)
step(Fitall,direction="backward")
formula(Fitall)

