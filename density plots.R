library(tidyverse)
eq1 = function(x){
  -(x*log(x)+(1-x)*log(1-x))/(log(0.5)/-0.5)
}
eq2 = function(x){
  1-(x**2+(1-x)**2)
}
eq3 = function(x){
  ifelse(x < 0.5,x,1-x)
}




ggplot(data.frame(x=seq(0, 1,length.out = 100)), aes(x=x)) + 
  stat_function(fun=eq1,aes(col="Entropy"))+
  stat_function(fun=eq2,aes(col="Gini coefficient"))+
  stat_function(fun=eq3,aes(col="Misclassification rate"))+
  theme_fivethirtyeight()+
  theme(legend.title = element_blank())

  
  
X <- data.frame(x=c(0,0.6,1.3,0.6,2.9,3.6,2.8,4.2,4.4,5,6,7),
               y=c(0,3,5,9,1.5,4.5,8.5,9.8,1.4,5,1,5),
               class=c("?","B","B","A","A","B","A","A","A","B","B","B"))
ggplot(X,aes(y, x, group=class))+
  geom_point((aes(shape=class, color=class)), size=6)+
  xlim(c(0,10))+
  ylim(c(0,5))+
  coord_polar()+
  ggtitle("K-Nearest Neighbour for a new point")+
  theme_economist() +
  theme(axis.text.x = element_blank())
  
attach(iris)
plot(Sepal.Length, Sepal.Width, xlim = c(4,7.2), ylim = c(2,4))
f1 <- kde2d(Sepal.Length, Sepal.Width, n = 1000, lims = c(4, 7, 2, 4))
image(f1, zlim = c(0, 2))
