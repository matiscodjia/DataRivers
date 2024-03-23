data = read.csv('breast_cancer.csv')
data$diagnosis = factor(data$diagnosis)
data = within(data,diagnosis <- relevel(diagnosis,ref = "B"))
data$diagnosis
col = colnames(data)
dim(data)
length(col)
selco = data[3:31]
data2 = sample(selco,5)
data2
data2$diagnosis = data$diagnosis
data2
indices_test <- sample(1:nrow(data2), size = 100, replace = F)
dat2.test = data2[indices_test,]
dat2.train = data2[-indices_test,]
dat2.test
library('glmnet')
mod = glm(diagnosis ~ .,
          control = glm.control(epsilon = 1e-8,maxit = 40, trace = TRUE),
          family=binomial(link = 'logit'), data = dat2.train)
coefficients(mod)
coefficients(mod)[1] + coefficients(mod)[2]* 0.014430 + coefficients(mod)[3]*87.44 + coefficients(mod)[4]*0.11380 + coefficients(mod)[5]*.015090 + coefficients(mod)[6]* 25.16
predict(mod,newdata = dat2.test)
exp(1.028558)/(1+exp(1.028558))
threshold = 0.5
predictions = plogis(predict(mod,newdata = dat2.test)) > threshold
actual = dat2.test$diagnosis == "B"
actual
predictions
table = table(predictions,dat2.test$diagnosis)
TN = table[1]
FP = table[2]
FN = table[3]
TP = table[4]
acc = 34/(34+5)
install.packages('ROCR')
library('ROCR')
predict_prob = plogis(predict(mod,newdata = dat2.test))
ROCRpred = prediction(predict_prob, dat2.test$diagnosis)
ROCRperf = performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(ROCRperf,colorize ="TRUE",text.adj = c(-0.2,1.7),print.cutoffs.at = seq(0,1,0.1))
