# STAT40790 Predictive Analytics I

# Project

# Brian Buckley


x <- c(59,45,75,72,89,70) # population density
y <- c(209,180,195,186,200,204) # robbery rate
plot(x, y, xlim=c(min(x)-5, max(x)+5), ylim=c(min(y)-10, max(y)+10), 
     main='Regression fit with 95% CI', xlab='Population density', ylab='Robbery rate')

mylm<-lm(y~x)
abline(mylm,col="red")
newx<-seq(20,90)
prd<-predict(mylm,newdata=data.frame(x=newx),interval = c("confidence"), 
             level = 0.95,type="response")
lines(newx,prd[,2],col="red",lty=2)
lines(newx,prd[,3],col="red",lty=2)

mod1.anova<-aov(y ~ x)
summary(mod1.anova)

#            Df Sum Sq Mean Sq F value Pr(>F)
#x            1   82.9   82.93   0.635   0.47
#Residuals    4  522.4  130.60


