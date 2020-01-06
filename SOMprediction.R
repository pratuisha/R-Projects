#Load the kohonen package 
library(kohonen)
data("wines")
# Create a training data set (rows are samples, columns are variables
# Here I am selecting a subset of my variables available in "data"
data_train <- heart_tidy_removed

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(data_train))

# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible
som_grid <- somgrid(xdim = 5, ydim=5, topo="hexagonal")

# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE)

plot(som_model, type="changes")

som.prediction <- predict(som_grid, newdata =Xtraining ,
                          trainX = Xtraining,
                          trainY = factor(data_train.classes[training]))

data(data_train)
set.seed(7)

training <- sample(nrow(data_train), 300)
Xtraining <- scale(data_train[training, ])
Xtest <- scale(data_train[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))

som.data_train <- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))

som.prediction <- predict(som.data_train, newdata =Xtraining ,
                          trainX = Xtraining,
                          trainY = factor(data_train.classes[training]))
table(data_train.classes[-training], som.prediction$prediction)
data(yeast)
summary(yeast)
View(yeast)


library(kohonen)
data_train <- heart_tidy_removed
set.seed(7)

#create SOM grid
sommap <- som(scale(data_train), grid = somgrid(2, 2, "hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
groups<-2
som.hc <- cutree(hclust(dist(sommap$codes)), groups)

#plot
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(sommap, som.hc)




####Sir Code 



#create SOM grid
sommap <- som(scale(heart_tidy_removed), grid = somgrid(5, 5, "hexagonal"))

## use hierarchical clustering to cluster the codebook vectors
groups<-2
somdata<-as.numeric(unlist(sommap))
som.hc <- cutree(hclust(dist(somdata$codes)), groups)

#plot
plot(sommap, type="codes", bgcol=rainbow(groups)[som.hc])

#cluster boundaries
add.cluster.boundaries(sommap, som.hc)

require(kohonen)

# Create a training data set (rows are samples, columns are variables
# Here I am selecting a subset of my variables available in "data"
data_train <- heart_tidy_removed

# Change the data frame with training data to a matrix
# Also center and scale all variables to give them equal importance during
# the SOM training process. 
data_train_matrix <- as.matrix(scale(data_train))

# Create the SOM Grid - you generally have to specify the size of the 
# training grid prior to training the SOM. Hexagonal and Circular 
# topologies are possible


training <- sample(nrow(data_train), 210)
Xtraining <- scale(data_train[training, ])
Xtest <- scale(data_train[-training, ],
               center = attr(Xtraining, "scaled:center"),
               scale = attr(Xtraining, "scaled:scale"))
som_model<- som(Xtraining, grid = somgrid(5, 5, "hexagonal"))

som.prediction <- predict(som_model, newdata = Xtest)

table(som.prediction$prediction)
# Finally, train the SOM, options for the number of iterations,
# the learning rates, and the neighbourhood are available
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=100, 
                 alpha=c(0.05,0.01))
som_model
som.prediction <- predict(som_model, newdata = som_grid)
som.prediction                        
