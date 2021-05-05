library(dslabs)
library(caret)
data("mnist_27")

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}

train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.5, len = 25)),
                     data = mnist_27$train)
Control=rpart.control(minsplit = 5, minbucket = 5, cp = 0.00, 
                      maxcompete = 2, maxsurrogate = 5, usesurrogate = 0, xval = 10,
                      surrogatestyle = 0, maxdepth = 30)
train_rpart <- rpart(y ~ .,
                     data = mnist_27$train, control=Control)
library(gridExtra)
p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p4 <- plot_cond_prob(predict(train_rpart, newdata = mnist_27$true_p, type = "prob")[,2]) +
  ggtitle("Decision Tree")

grid.arrange(p4, p2, nrow=1)

p3 <- ggplot(mnist_27$train, aes(x_1,x_2,col=y))+
  geom_point()+ggtitle("Training set")


tree_y <- predict(train_rpart, newdata = mnist_27$test)
