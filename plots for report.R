library(tidyverse)
library(caret)
library(GGally)
library(stats4) #Load package stats
library(splines) #Load package splines
#To load package VGAM, need to load package stats4 and splines
library(VGAM) #Load package VGA

data("iris")

library(caret)
set.seed(3456)
trainIndex <- createDataPartition(iris$Species, p = .5, 
                                  list = FALSE, 
                                  times = 1)

irisTrain <- iris[ trainIndex,]
irisTest  <- iris[-trainIndex,]
model_lda <- train(Species~., 
                   data= irisTrain[,c("Species","Petal.Width","Petal.Length")], method="lda")
model_qda <- train(Species~., 
                   data= irisTrain[,c("Species","Petal.Width","Petal.Length")], method="qda")

model_knn <- train(Species~., 
                   data= irisTrain[,c("Species","Petal.Width","Petal.Length")], method="knn",  tuneGrid = expand.grid(k = 1))

train_knn_iris = 
  predict(model_knn, newdata =irisTrain , type = 'raw')
test_knn_iris = 
  predict(model_knn, newdata =irisTest , type = 'raw')
mean(train_knn_iris==irisTrain$Species)
mean(test_knn_iris==irisTest$Species)
############################# Logistic regression section
test_iris_qda <- predict(model_qda, irisTest[,3:4], type="raw")
test_iris_lda <- predict(model_lda, irisTest[,3:4], type="raw")
train_iris_qda <- predict(model_qda, irisTrain[,3:4], type="raw")
train_iris_lda <- predict(model_lda, irisTrain[,3:4], type="raw")

model_logist <- vglm( Species ~  Petal.Length + Petal.Width , family=multinomial, irisTrain)
predict(model_qda, irisTest[,3:4], type="raw")
logit_iris <- predict(model_logist, irisTest[,3:4], type="response")
table(predictions, irisTest$Species)


predictions <- apply(logit_iris, 1, which.max)
predictions[which(predictions=="1")] <- levels(iris$Species)[1]
predictions[which(predictions=="2")] <- levels(iris$Species)[2]
predictions[which(predictions=="3")] <- levels(iris$Species)[3]
########################################
# plot lda and qda
plot(model_lda)
ggpairs(iris,  ggplot2::aes(colour = Species), title = "Exploratory Data Analysis for Iris Data",
        diag = list(discrete = "barDiag", continuos = wrap("densityDiag", alpha = )))

ggdb <- ggplot(mapping = aes(y=Petal.Width, x= Petal.Length))+
  geom_point( aes(col = Species), data =iris)
ggdb
#########################################
iris_plot <- iris[, c("Petal.Width", "Petal.Length")]
#########################################
make1Dgrid = function(x) {
  rg = grDevices::extendrange(x)
  seq(from = rg[1], to = rg[2], length.out = 100)
}
iris_grid = with(iris_plot,
                     expand.grid(Petal.Width = make1Dgrid(Petal.Width),
                                 Petal.Length = make1Dgrid(Petal.Length)))
iris_grid$lda_species =
  predict(model_lda, newdata = iris_grid, type = "raw")

iris_grid$qda_species =
  predict(model_qda, newdata = iris_grid, type = "raw")

lostic_probs_iris =  predict(model_logist, newdata = iris_grid, type="response")
grid_predictions <- apply(lostic_probs_iris, 1, which.max)
grid_predictions[which(grid_predictions=="1")] <- levels(iris$Species)[1]
grid_predictions[which(grid_predictions=="2")] <- levels(iris$Species)[2]
grid_predictions[which(grid_predictions=="3")] <- levels(iris$Species)[3]
iris_grid$logitic_species= grid_predictions


# plots
ggdb + geom_raster(aes(fill = lda_species),
                   data = iris_grid, alpha = 0.25, interpolate = TRUE)+
  ggtitle("Linear Discriminant Classification regions for Iris Data")+
  theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())

ggdb + geom_raster(aes(fill = qda_species),
                   data = iris_grid, alpha = 0.25, interpolate = TRUE,
                   )+ ggtitle("Quadratic Discriminant Classification regions for Iris Data")+
  theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())

ggdb + geom_raster(aes(fill = logitic_species),
                   data = iris_grid, alpha = 0.25, interpolate = TRUE)+
  ggtitle("Logistic Regression Classification regions for Iris Data")+
   theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())
  
#####################################################################
set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 3)
model_knn <- train(Species~., 
                   data= iris[,c("Species","Petal.Width","Petal.Length")], method="knn",  tuneGrid = expand.grid(k = 1))

iris_grid$knn_species = 
  predict(model_knn, newdata = iris_grid, type = 'raw')

ggdb + geom_raster(aes(fill = knn_species),
                   data = iris_grid, alpha = 0.25, interpolate = TRUE)+
  ggtitle("K-Nearest Neighbour Classification regions for Iris Data")+
  theme_bw() + theme(plot.title = element_text(hjust=0.5))+
  theme(legend.title = element_blank())
