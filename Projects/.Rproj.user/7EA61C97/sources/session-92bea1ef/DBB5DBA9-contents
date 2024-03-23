vecteur <- rnorm(3000,0,3)
data <- matrix(data = vecteur,1000,3)
dim(data)
data.simul <- as.data.frame(data)
colnames(data.simul) <- c("x.1","x.2","x.3")
data.simul$y = 0.5 + 3*data.simul$x.1 + data.simul$x.3
data.simul
library(tidyverse)
ggplot(data.simul,aes(x = x.1,y=y))+geom_smooth()

ggplot(data.simul,aes(x = x.2, y = y)) +geom_smooth()

ggplot(data.simul, aes(x = x.3, y = y)) + geom_smooth()

#x.2 n'est pratiquement lié à y

mod1 = lm(y ~ x.1,data.simul)
shapiro.test(mod1$residuals)
summary(mod1)


mod2 = lm(y ~ x.2,data.simul)
shapiro.test(mod2$residuals)
summary(mod2)

mod3 = lm(y ~ x.3,data.simul)
shapiro.test(mod3$residuals)
summary(mod3)

mod = lm(y ~ x.1 + x.3, data.simul)
shapiro.test(mod$residuals)
#PAS NORMAL et pourtant estime bien les données
summary(mod)

data1 = read.table("data1.txt", header = T)

mod1 = lm(y ~ x.1, data1)
ggplot(data1, aes(x = x.1)) +
  geom_histogram()

shapiro.test(mod1$residuals)
ggplot(data1,aes(x = x.1,y=y))+geom_smooth()
summary(mod1)
#X.1 n'influence pas y 

mod0 = lm(y ~ 1,data1)
shapiro.test(mod0$residuals)
anova(mod0,mod1)

rss0 = sum((mod0$residuals)^2)
rss1 = sum((mod1$residuals)^2)
ft = (rss0 - rss1)/(rss1/198)
ft

mod13 = lm(y ~ x.1 + x.3, data1)
shapiro.test(mod13$residuals)
summary(mod13)

#rejet de H0
anova(mod1,mod13)

modall = lm(y~., data = data1, x = TRUE)
shapiro.test(modall$residuals)
anova(mod13,modall)
summary(modall)
ggplot(data1,aes(x = x.3,y=y))+geom_smooth()
ggplot(data1,aes(x = x.5,y=y))+geom_smooth()
ggplot(data1,aes(x = x.7,y=y))+geom_smooth()
ggplot(data1,aes(x = x.9,y=y))+geom_smooth()
aic = 200*log(sum((mod13$residuals)^2)/200) + 200*log(2*pi) + 2 * 2 + 200
aicc = aic + ((4*3)/197)
aicc
install.packages("qpcR")
library(qpcR)
AICc(mod13)
install.packages("MASS")
library("MASS")
stepAIC(modall,trace = TRUE)

modridge = lm.ridge(y ~ .,data1,lambda=seq(0,500,by=0.1))
plot(modridge)
text(rep(450,10), coef(modridge)[length(seq(0,500,0.1)),-1], colnames(data1)[2:11])


install.packages("glmnet")
library("glmnet")

y = as.matrix(data1$y)
x = as.matrix(data1[,-1])
fit = glmnet(x,y)
plot(fit,xvar ="lambda", label = TRUE)

# Supposons que vous avez déjà effectué la validation croisée
cvfit <- cv.glmnet(x, y)
lambda_min <- cvfit$lambda.min
coefficients_min <- coef(cvfit, s = "lambda.min")
coefficients_min
data1$y_estimate = 3*data1$x.3 + 2*data1$x.5

library("tidyverse")
ggplot(data1, aes(x = x.3)) +
  geom_smooth(aes(y = y, color = "Variable réelle")) +
  geom_smooth(aes(y = y_estimate, color = "Variable estimée")) +
  labs(title = "Variables en fonction de x", x = "x", y = "Valeur")

datap = read.table("prostate-data.txt", header = T)
datap

y = as.matrix(datap$lpsa)
x = as.matrix(datap[,c('lcavol','lweight','age','lbph','lcp','gleason','pgg45')])

fit = glmnet(x,y)
plot(fit,xvar ="lambda", label = TRUE)

cvfit <- cv.glmnet(x, y)
lambda_min <- cvfit$lambda.min
coefficients_min <- coef(cvfit, s = "lambda.min")
coefficients_min
datap$y_estimate = 0.6*datap$lcavol + 0.7*datap$lweight

library("tidyverse")
ggplot(datap, aes(x = lcavol)) +
  geom_smooth(aes(y = lpsa, color = "Variable réelle")) +
  geom_smooth(aes(y = y_estimate, color = "Variable estimée")) +
  labs(title = "Variables en fonction de x", x = "x", y = "Valeur")
