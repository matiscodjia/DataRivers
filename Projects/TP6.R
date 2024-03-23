data <- read.delim("data-RF.tsv",sep="\t",header = T)
data$y = factor(data$y)
head(data)
summary(data)

library(tidyverse)

ggplot(data, aes(x = x.1, y = x.2, col=y)) + 
  geom_point() 
indices_test <- sample(1:nrow(data), size = 150, replace = F)
data.test <- data[indices_test,]
data.train <- data[-indices_test,]

install.packages("tidymodels")
install.packages("tidyr")

library("tidymodels")
library("tidyr")

treeSpec = decision_tree(mode="classification", engine="rpart", tree_depth=NULL,min_n=NULL)

treeFit = fit(treeSpec, y ~ ., data = data.train)
install.packages("rpart")
install.packages("rpart.plot")
library("rpart")
library("rpart.plot")


rpart.plot(treeFit$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")

##Le point tombe du coté droit donc dans 1
predictions = predict(treeFit, data.test[,1:2])
predictions$.pred_class
table = table(predictions$.pred_class,data.test$y)
table



###Saheart
library("tidymodels")
library("tidyr")
library("rpart")
library("rpart.plot")

data2 = read.csv('Saheart-data.csv')
data2
data2$chd = factor(data2$chd)
indices_test <- sample(1:nrow(data2), size = 154, replace = F)
data2.test <- data2[indices_test,]
data2.train <- data2[-indices_test,]
treeSpec2 = decision_tree(mode="classification", engine="rpart", tree_depth=NULL,min_n=NULL)
treeFit2 = fit(treeSpec2, chd ~ ., data = data2.train)
rpart.plot(treeFit2$fit, type = 4, extra = 101, under = TRUE, cex = 0.8, box.palette = "auto")


predictions = predict(treeFit, data.test[,2:10])
predictions$.pred_class
table = table(predictions$.pred_class,data.test$y)
table

