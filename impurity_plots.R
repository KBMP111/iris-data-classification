p <- seq(0, 1, 0.001)

# misclassification rate
miscl <- 1 - pmax(p, 1-p)

# Gini index
gini <- 2*p*(1-p)

# Cross entropy
ce <- (p * log((1-p)/p) - log(1-p)) / (2*log(2))
impurity_data <- rbind(cbind("Entropy", p, ce),cbind("Gini-Impurity", p, gini),cbind("Misclassification Rate", p, miscl)) %>% as_tibble()
colnames(impurity_data)= c("function_name","probability","impurity")

impurity_data$probability <- as.numeric(impurity_data$probability)
impurity_data$impurity <- as.numeric(impurity_data$impurity)
impurity_data$function_name <- as_factor(impurity_data$function_name)

matplot(p, cbind(miscl, gini, ce), col=c("red", "green", "blue"), ylab = "Impurity Measure")
legend("topright",pch=21,col=c("red", "green", "blue"), legend=c("Misclassification Rate","Gini Index","Cross-entropy"))
node_impurity_functions <- cbind(p,miscl,gini,ce) %>% as_tibble()
ggplot(data = impurity_data, aes(x=probability,y=impurity, col = function_name ))+
  geom_point()+
  theme_bw()+
  xlab("Estimated Class probability")+
  ylab("Impurity")+
  ggtitle( "Impurity Functions")+
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        axis.title = element_text(face="bold", hjust = 0.5))+
  theme(legend.title = element_blank())

  
  geom_point(col='blue',size= 0.5)+
  geom_point(y = gini, col = 'green',size= 0.5)+
  geom_point(y = ce, col = 'red',size= 0.5)+
  theme_bw()+
  xlab("Estimated Class probability")+
  ylab("Impurity")+
  ggtitle( "Impurity Functions")+
  theme(plot.title = element_text(face="bold", hjust = 0.5),
        axis.title = element_text(face="bold", hjust = 0.5))
