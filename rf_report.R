library(randomForest)
train_rf <- randomForest(y ~ ., data=mnist_27$train)


p1 <- plot_cond_prob() + ggtitle("True conditional probability")
p2 <- plot_cond_prob(predict(train_rf, newdata = mnist_27$true_p, type = "prob")[,2]) +
  ggtitle("Random Forest")
grid.arrange(p2, p1, nrow=1)
p1
p2
