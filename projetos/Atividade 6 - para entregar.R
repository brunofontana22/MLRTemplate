##ATIVIDADE 6 - Wine Knn Cross Validation

"Temos um conjunto de dados com 13 atributos com valores contínuos e mais 
um atributo (V1) com rótulos de classe de origem do vinho (cultivares).
Usando o dataset ('wine_data.Rdata') nossa tarefa é construir um modelo 
para reconhecer a origem do vinho a partir das suas características, usando KNN."

#Carregando Libraries
install.packages("ggplot2")
library(ggplot2)
install.packages("caret")
library(caret)

##Carrregamento de dados

##Analisando e visualizando os dados
w <- wine_df
head(w)

str(w)

summary(w)


colnames (w) [1] <- "Cultivares"
colnames (w) [2] <- "Alcohol"

##Dividindo os dados em Treinamento e Test
set.seed(300)
indiceTrain <- createDataPartition(y = w$V7,w = 0.75,list = FALSE)
training <- w[indiceTrain,]
testing <- w[-indiceTrain,]


##Checando a distribuicao dos dados de treinamento e teste
w$grupo = "Treinamento"
w$grupo[-indiceTrain]="Teste"  

n <- ggplot(data = w, aes(x=V7,fill=grupo)) 
n + geom_density(alpha=0.4) 

##Preprocessamento dos dados - normalizacao dos dados
##A coluna cultivares não será considerada por ser uma categoria (fator)
trainX <- training[,names(training) != "cultivares"]
preProcValues <- preProcess(x = trainX,method = c("center", "scale"))
preProcValues

##Preenchendo espaços vazios
library(Amelia)
library(randomForest)
missmap(w, main = "Missing values vs observed")

V7fit <- rpart::rpart(V7 ~ V6 + V5 + V4 + V3,
               data=w[!is.na(w$V8),], 
               method="anova")
  
w$V7[is.na(w$V7)] <-  predict(w$V7, w[is.na(w$V7),])
        
        
##Treinamento
set.seed(400)
aka <- trainControl(method = "repeatedcv",repeats = 3) 

gridK <- expand.grid( k = c(2:14) )

knnFit <- train(V7 ~ V6 + V5,
                data = training, method = "knn", 
                trControl = aka, 
                preProcess = c("center","scale"),
                #Definindo a faixa de parametros a ser usada
                tuneGrid = gridK )

knnFit

##Variando os parâmetros automaticamenteusando
knnFit <- train(V7 ~ V6 + V5, data = training, method = "knn", 
          trControl = aka, preProcess = c("center","scale"), tuneLength = 20)

##Output de kNN fit
knnFit

##Plotando knnFit
plot(knnFit)

##fazer acuracia do modelo: predict

"nao esta dando certo, porque falta valores a serem preenchidos no V7"
