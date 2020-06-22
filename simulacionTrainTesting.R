  library(tidyverse)
  set.seed(10)
  x <- 2 - 3 * rnorm(20,0, 1)
  y <- x - 2 * (x^2) + 0.25*(x^3) + rnorm(20,-3, 3)
  df <- data.frame(x,y)
  df$grupo <- c(rep(c("Training","Testing"),each=10))
  
  reg1 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x)
  rmse <- sum( (predict(reg1)- df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
learn1<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x  ,se = FALSE,method='lm') +
    labs(title=" y ~ x",subtitle=paste("MSE:",round(rmse,3)," "))

rmseTest <- sum( (predict(reg1,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
test1<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
  geom_point() +
  geom_smooth(formula =  y ~ x  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
  labs(title=" y ~ x",subtitle=paste("MSE:",round(rmseTest,3)," "))


  
  reg2 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2))
  rmse <- sum( (predict(reg2)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn2<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2)  ,se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  rmseTest <- sum( (predict(reg2,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test2<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  
  reg3 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3))
  rmse <- sum( (predict(reg3)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn3<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3)  ,se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg3,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test3<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  reg4 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4))
  rmse <- sum( (predict(reg4)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn4<- ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4)  ,se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + I(x^4)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg4,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test4<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^4)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  reg5 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5))
  rmse <- sum( (predict(reg5)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn5<-ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)  ,se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + I(x^4) + I(x^5)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg5,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test5<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  reg6 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6))
  rmse <- sum( (predict(reg6)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn6<- ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6)  ,se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^6)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg6,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test6<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + ... + I(x^6)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  reg7 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7))
  rmse <- sum( (predict(reg7)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn7<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7),se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^7)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg7,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test7<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^7)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  
  reg8 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8))
  rmse <- sum( (predict(reg8)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn8<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8),se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^8)",
         subtitle=paste("MSE:",round(rmse,3)," "))
  
  rmseTest <- sum( (predict(reg8,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test8<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^8)",subtitle=paste("MSE:",round(rmseTest,3)," "))
  
  
  
  reg9 <- lm(data = df %>% filter(grupo %in% "Training"),formula=y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9))
  rmse <- sum( (predict(reg9)-df %>% filter(grupo=="Training") %>% pull(y))^2 ) / 10
  learn9<-  ggplot(df %>% filter(grupo=="Training"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9),se = FALSE,method='lm') +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^9)",
         subtitle=paste("MSE:",round(rmse,3)," "))

  rmseTest <- sum( (predict(reg9,newdata = df %>% filter(grupo=="Testing"))- df %>% filter(grupo=="Testing") %>% pull(y))^2 ) / 10
  test9<-  ggplot(df %>% filter(grupo=="Testing"),aes(y=y,x=x)) +
    geom_point() +
    geom_smooth(formula =  y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9)  ,se = FALSE,method='lm',data = df %>% filter(grupo %in% "Training")) +
    labs(title=" y ~ x + I(x^2) + I(x^3) + I(x^3) + ... + I(x^9)",subtitle=paste("MSE:",round(rmseTest,3)," "))

library(ggpubr)
library(ggthemes)
ggplot(df %>% filter(grupo %in% "Training")) +
  geom_point(aes(x=x,y=y), size=3) +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size=12))
ggsave(filename = "scatterTrainTest.png",dpi=300,scale=1)

grafico <- ggarrange(learn1,learn2,learn3,learn4,learn5,learn6,learn7,learn8,learn9)
ggsave(grafico,filename = "graficoTrainTest.png",dpi=300,scale = 2)
graficoTest <- ggarrange(test1,test2,test3,test4,test5,test6,test7,test8,test9)
ggsave(graficoTest,filename = "graficoOverfit.png",dpi=300,scale = 2)

    
