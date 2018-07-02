##Atividade 3 - Bluesky


install.packages("readxl")
install.packages("tidyverse")
library(readxl)
library(tidyverse)
library(ggplot2)
install.packages("basicTrendline")
library(readxl)


library(readxl)
read_excel("C:/Users/User/Desktop/Arquivos/Documents/R/MLRTemplate/BlueSky_Single-Leg_demand.xls", 
                                        range = "A6:A371", col_types = c("date", 
                                                                         "text"))
dados <- BlueSky_Single_Leg_demand

colnames (dados) [2] <- "Demand"
colnames (dados) [1] <- "Date"

#plots

g <- ggplot(dados, aes(x=Date, y=Demand)) +
  geom_point() + 
  geom_line()
g

#valor critico

tarifa.full <- 174
tarifa.low <- 114

valor.critico <- (tarifa.full-tarifa.low)/tarifa.full

#modelo 1 - normal ###

#vamos considerar que dados seguem uma ditribuicao normal
#sem discriminar o dia da semana

media_l <- mean(dados$Demand)

m_int <- round(media_l,0)

g <- g + geom_hline(yintercept = media_l, color = "blue"),
linetype = "longdash",  size = 1)


#sd e RMSE (considerando o valor fracionÃ¡rio)
rmse_1 <- sd (dados$Demand)

proction_Level_1 <- qnorm(p=valor.critico,mean = media_l, sd = rmse_1)

proction_Level_1 <- round(proction_Level_1,0)


# Modelo 2 - Empírico ####

#Vamos considerar que dados seguem uma distribuição empírica
#sem discrimar o dia da semana

q.model2 <- quantile(dados$demand, c(valor.critico,0.5)) 

q.model2

protection_Level_2 <-  q.model2[[1]]

mediana_2 <-  q.model2[[2]]

g <- g + geom_hline(yintercept =  mediana_2, color="red",
                    linetype = "longdash", size = 1)
g

# RMSE

rmse_2 <- sqrt(mean((dados$demand - mediana_2)^2))


#modelo 3 - split por dias da semana (normal)

dados$day <- weekdays(dados$Date)

#primieira maneira de fazer (para cada dia...)

mean.days.normal <- dados $>% 
group_by(day) %>% 
  summarise(media.N = mean(demand),
            sd.N = sd(demand)
            prot.Level.3 = qnorm(p=valor.critico, mean = media.N)
          
            
            
            # RMSE modelo 3
            
            rmse_3 <- sqrt(mean((mean.days.normal$demand - mean.days.normal$media.N)^2))
            
            
            # Modelo 4 # Empírica por dia)
            
            #Usando mutate (anexa as variáveis criadas aos dados):
            median.days.empirica <- dados %>%
              group_by(day) %>%
              mutate(mediana = median(demand),
                     prot.Level.4 = as.integer(quantile(demand,valor.critico))
              )
            
            rmse_4 <- sqrt(mean((median.days.empirica$demand - median.days.empirica$mediana)^2))
            
            # Modelo 5 - considerando mês REGRESSÃO
            
            dados$month <- months(dados$date)
            
            # Para regressão: FATORES
            
            dados$day <- factor(dados$day)
            
            dados$month <- factor(dados$month)
            
            summary(dados$day)
            is.factor(dados$day)
            levels(dados$day)
            
            #Regressão
            
            dados.fit <- lm(data=dados, demand ~ day + month)
            
            summary(dados.fit)
            
            
            # Incluindo um termo de interação dia-mês 
            dados.fit <- lm(data=dados, demand ~ day + month + day:month)
            
            summary(dados.fit)
            
            #ou formula: demand ~ day*month gera a mesma formula
            
            dados.fit <- lm(data=dados, demand ~ day*month)
            
            summary(dados.fit)
            
            #Plot residuals
            
            plot(dados.fit)
            
            #plotando os valores previstos
            
            prediction <- predict(dados.fit,newdata = dados )
            
            g <- ggplot(dados, aes(x=date,y=demand)) +
              geom_point() +
              geom_line() 
            
            #use fitted values or prediction
            g + geom_point(aes(x=dados$date,y=dados.fit$fitted.values), 
                           color = "red")
            
            
            g + geom_line(aes(x=dados$date,y=prediction), 
                          linetype="dotdash",
                          color = "blue")
            
            
              
            ## fazer a regressÃ£o colocando os feriados. 
            ## Colocar 2 feriados (natal, pascoa)
            
            
            
            
            