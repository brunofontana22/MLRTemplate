##Atividade 4 -previsao de bebes abaixo do peso

install.packages("rpart")
library(rpart)
install.packages("rpart.plot")
library(rpart.plot)
install.packages("RColorBrewer")
library(RColorBrewer)


install.packages("RGtk2")
library(RGtk2)
install.packages("rattle")
library(rattle)

bw <- birth


##Our goal here is predict when a infant will born with low weight 
##(less than 2.5Kg). Marked by variable low in the dataset


str(bw)
hist(bw$bwt)

##Convertendo as variaveis categoricas em fatores
cols <- c('low', 'race', 'smoke', 'ht', 'ui')
bw[cols] <- lapply(bw[cols], as.factor)

table(bw$low)
prop.table(table(bw$low))

##Fix de seed to guarantee replicablility
set.seed(123)

train.index <- sample((nrow(bw)),0.7*nrow(bw))

##The training set should be used to build your machine learning models.
train <- bw[train.index,]

##The test set should be used to see how well your model performs on unseen data.
test  <- bw[-train.index,]

##Building a tree
fit <- rpart(low ~ lwt + age + smoke + ptl + ht +
               ui + ftv,
             data=train,
             method="class")
summary(fit)

##Let's check with test set
prediction <- predict(fit, test, type = "class")
table(prediction,test$low)

prop.table(table(prediction,test$low))

##Trying with a big tree
fit <- rpart(low ~ lwt + age + smoke + ptl + ht +
               ui + ftv,
             data=train, method="class",
             minsplit = 2, minibucket = 2, cp = -1 
)
fancyRpartPlot(fit)

##checking accuracy with training set
prediction <- predict(fit, train, type = "class")
table(prediction,train$low)

prop.table(table(prediction,train$low))

##Let's check with test set
prediction <- predict(fit, test, type = "class")
table(prediction,test$low)

prop.table(table(prediction,test$low))