library(mltools)
Titanic %>% as.data.frame() %>% as.data.frame.table() %>% one_hot(cols="auto")
dummy <- dummyVars(" ~ .", data=Titanic, fullRank = T)
newdata <- data.frame(predict(dummy, newdata = Titanic)) 
newdata
tit_lr <- glm(SurvivedYes~., data= newdata, family = binomial)
tit_lda <- train(y=newdata$SurvivedYes %>% as.factor(),x=newdata[,1:6], method="lda")
