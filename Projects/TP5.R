dataNN <- read.delim("dataNN.tsv",sep="\t",header = T)
head(dataNN)

hist(dataNN$x.1)
hist(dataNN$x.2)

library(tidyverse)
ggplot(dataNN, aes(x = x.1, y = x.2,col=out)) + 
  geom_point() 

indices_test <- sample(1:nrow(dataNN), size = 150, replace = F)
data.test = dataNN[indices_test,]
data.train = dataNN[-indices_test,]
dim(data.train)
install.packages("neuralnet", repos = "https://cran.r-project.org/")

library(neuralnet)

nn = neuralnet(out ~ x.1 + x.2, data.train,hidden = 1)
plot(nn)
neur1 = nn$weights[[1]][[1]]
coeffsortie = nn$weights[[1]][[2]]
neur1
coeffsortie
neurout= plogis(neur1[1][1] + dataNN$x.1[1]*neur1[2][1] + dataNN$x.2[1]*neur1[3][1])
neurout
neurfinal= neurout*coeffsortie[2][1] + coeffsortie[1][1]
neurfinal
compute(nn,dataNN[1,1:2])

actual = data.test$out == 1


table(compute(nn,data.test[,1:])$net.result > 0.5)
