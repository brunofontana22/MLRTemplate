## Atividade 5 - fazer com nascimento de bebe usado na arvore de decisao (atividade 4), 
##usar regressao em logistica 


##Our goal here is predict when a infant will born 
##with low weight (less than 2.5Kg). 
##Marked by variable low in the dataset

##Examine structure of dataframe
str(bw)

##Look at histogram
hist(bw$bwt)

##Some data preparation
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

##Buildind a model
fit <- glm (low ~ lwt + age + smoke + ptl + ht +
              ui + ftv, 
           data = train,
           family = binomial(link = "logit"))
summary(fit)

##To analyze the table of deviance
anova(fit, test ="Chisq")

##To build this model, use:
##Let's check with test set
prediction_prob <- predict(fit, newdata = test, 
                           type = 'response')
prediction <- ifelse(prediction_prob > 0.5,1,0)
table(prediction,test$low)

prop.table(table(prediction, test$low))

##Trying with small model
fit <- glm(low ~ lwt + age + smoke + ptl + ht +
             ui + ftv,
           data=train, 
           family=binomial(link="logit"))


##Let's check with test set
prediction_prob <- predict(fit, test, type='response')
prediction <- ifelse(prediction_prob > 0.5, 1, 0)
table(prediction,test$low)

prop.table(table(prediction,test$low))



