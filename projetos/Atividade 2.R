##Atividade 2


"O que é a correlacao R: como uma variavel varia/se comporta em relacao a outra
Ex:
R = 1, para cada valor de x que aumenta 1, y aumenta 

Avaliar se foi possivel captar o modelo"

A regressao eh avaliada pela correlacao R e pelos residuos. 
Formas de avaliar residuos: resuduals vs. fitted values (erros); 
histogram de residuals; Normal Q-Q Plot of Residuals


##instalando pacote
install.packages("ggplot2")
install.packages("caret")
library(caret)
library(ggplot2)

##carregando arquivo housingsmall em txt

###Verificando
colnames(hr)
colnames(hr)[2]

###Alterando
hr <- housingSmall
colnames(hr) [2] = "area"
colnames(hr) [3] = 'garagem'
colnames(hr) [4] = 'quartos'
colnames(hr) [5] = 'preco'


###Checando
colnames(hr)

## Visualizando os dados
library(ggplot2)
g <- ggplot(data = hr, aes(x = area, y = preco))
g <- g + geom_point() +
  labs( title = "Precos de Venda de imoveis", 
        y = "preco (x R$1000,00)", x = "área (x 100m2)" )
g


### Inserindo retas verticais ----
#### Quanto custa casa em relação aos vizinhos?
g <- g + geom_vline(xintercept = 2, colour = "green", linetype = 'longdash' size = 2) +
  geom_vline(xintercept = c(1.93,2.07), colour = "red", linetype = 'longdash', size = 1)


## Modelo Inicial - regressÃ£o linear ----
hr_lm <- lm(data = hr, preco~area)

###Plotando a reta da regressão sem ggplot ( x primeiro!)
plot(hr$area,hr$preco)
abline(hr_lm$coefficients)

### Regressão Linear (Direto no ggplot) ----
g <- ggplot(data = hr, aes(x=area,preco)) + geom_point()
g <- g + stat_smooth(method = lm, se = FALSE, formula = y ~ x, colour = "Black"), 
linetype = "solid")
g + geom_vline(xintercept = 2.0, size = 1, colour = "green", linetype = "longdash")


##fazer uma regressao com todos os valores da tabela. 
##fazer o grafico de residuos tambÃ©m
## Colocar no git hub como um projeto

##R² = eh a % de dados que o modelo consegue explicar. 





