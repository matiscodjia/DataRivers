data3 = read.table("data3.txt", header = T)
data3
library(tidyverse)
ggplot(data3,aes(x=x.1,y=x.2,col=gr))+ geom_point(aes(size = 2))
mod123 = lm(gr_quanti ~ x.1 + x.2 + x.3, data3)
shapiro.test(mod123$residuals)
summary(mod123)
data3$estimated = mod123$coefficients[2]*data3$x.1 + mod123$coefficients[3]*data3$x.2 + mod123$coefficients[4]*data3$x.3 + mod123$coefficients[1]
data3$estimated[4]
data3$estimated[5]
pred = predict(mod123) > 0.5
#Contient un vecteur qui permet de savoir dans quel groupe sont les observation
pred
table(pred)
table(data3$gr_quanti)

mod12 = lm (gr_quanti ~x.1 +x.2,data3)
summary(mod12)
ggplot(data3,aes(x.1,x.2,col=pred,shape = gr)) + geom_point(aes(size = 2))

 
ggplot(data3,aes(x.1,x.2,col=pred22,shape = gr)) + geom_point(aes(size = 2)) + geom_abline(intercept =((0.5 - mod12$coefficients[1])/mod12$coefficients[3]), slope = -(mod12$coefficients[2]/mod12$coefficients[3]), color = "black")  # Ajo

data3$x12 = data3$x.1^2
data3$x22 = data3$x.2^2
data3$x1x2 = data3$x.2 * data3$x.1
mod22 = lm (gr_quanti ~ x.1 + x.2 + x22 + x12 + x1x2, data3)
summary(mod22)
graphique <- ggplot(data3, aes(x.1, x.2, color = gr_quanti, shape = gr)) +
  geom_point()
print(graphique)
#18) Forme quadratique
pred22 = predict(mod22) > 0.5
table(pred22)
table(data3$gr_quanti)
indices_train <- sample(1:nrow(data3), size = 800, replace = F)
data.test <- data3[-indices_train,]
data.train <- data3[indices_train,]

mod.train <- lm(gr_quanti ~ x.1 + x.2 , data3)
predCV = predict(mod.train,data.test)
predCV
table(predCV > 0.5)
table(data.test$gr_quanti == 1)

mod.train_2 = lm (gr_quanti ~ x.1 + x.2 + x22 + x12 + x1x2, data.train)

predCV_2 = predict(mod.train_2,data.test)
predCV_2
table(predCV_2 > 0.5)
table(data.test$gr_quanti == 1)

datah = read.table("SAheart-data.txt", header = T, sep =",")
mod = lm (chd ~ sbp + tobacco + ldl + adiposity + famhist + typea + obesity + alcohol + age,datah)
summary(mod)

