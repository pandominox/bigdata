# TODO: Add comment
# 
# Author: Anna Matysik
###############################################################################

source("RTools/prepare_packages.R")

preparePackages(c("DAAG", "gsubfn", "ggplot2", "grid", "gridExtra","plyr","grDevices","RColorBrewer","scales","Cairo"))


d <- data.frame(x=c(1,4,5,7,9,11,3,4), y=c(0.8,4.2,4.7,8,7.5,6,8.0,-5.4))
formula <- y~ x+I(x^2) + I(x^3) + I(x^4) 

lm1 <- lm(formula,data=d)

summary(lm1)

lm1.s <- step(lm1)

p_conf1 <- predict(lm1,interval="confidence")
p_pred1 <- predict(lm1,interval="prediction")

nd <- data.frame(x=seq(0,8,length=25))
p_conf2 <- predict(lm1,interval="confidence",newdata=nd)
p_pred2 <- predict(lm1,interval="prediction",newdata=nd)

plot(y~x,data=d,ylim=c(-15,30),xlim=c(0,15)) ## data

dev.new()

#points(lm1) ## fit
#matlines(d$x,p_conf1[,c("fit","lwr","upr")],col="darkred",lty=1,type="b",pch="+")
#matlines(d$x,p_pred1[,c("fit","lwr","upr")],col="red",lty=2,type="b",pch=1)
#matlines(nd$x,p_conf2[,c("fit","lwr","upr")],col="darkblue",lty=1,type="b",pch=2)
#matlines(nd$x,p_pred2[,c("fit","lwr","upr")],col="blue",lty=2,type="b",pch=3)

ggplot(lm1$model, aes(x = x, y = y)) +
		geom_point(shape=1) +   
		geom_smooth(method=lm, formula=formula, col = "red")

dev.new()
# diagnostic plot
layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page 
plot(lm1)

dev.new()
# K-fold cross-validation
cv.lm(df=d, lm1, m=4) # 3 fold cross-validation


