library(e1071)
library(rpart)


svm.model <- svm(y ~ ., data=mnist_27$train, cost=1,scale=TRUE, kernel= "radial")

svm.pred <- predict(svm.model,mnist_27$true_p )

p2 <- plot_cond_prob(svm.pred) +
  ggtitle("SVM")

grid.arrange(p2, p1, nrow=1)
p1
p2
cbind(svm.pred, mnist_27$true_p) %>% as_tibble() %>% ggplot(aes(x_1,x_2, col=svm.pred))+
  geom_point()
