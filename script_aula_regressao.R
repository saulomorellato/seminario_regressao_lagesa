### INTRODUCAO ###

#Neste conjunto de dados sao apresentadas as siglas dos 40 estados norte-americanos
#contiguos juntamente com as seguintes variaveis:
#- taxa (taxa do combustível no estado em USD),
#- licenca (proporcao de motoristas licenciados),
#- renda (renda per capita em USD),
#- estradas (ajuda federal para as estradas em mil USD),
#- consumo (consumo de combust´ıvel por habitante).
#
#O interesse nesse estudo e tentar explicar o consumo medio de combustivel
#pelas variaveis taxa, licenca, renda e estradas.



## LIMPANDO A MEMORIA ##

rm(list=ls(all=TRUE))




## CARREGANDO PACOTES ##

library(tidyverse)  # para organizar os dados
library(gtsummary)  # para organizar resultados em tabela
library(mixlm)      # para selecao de variaveis (stepwise)



## CARREGANDO OS DADOS ##

dados<- read.csv("dados_combustivel.csv", header=TRUE)
novos<- read.csv("novos_combustivel.csv", header=TRUE)



## VISUALIZANDO/MANIPULANDO OS DADOS ##

glimpse(dados)
glimpse(novos)

summary(dados)



## AJUSTANDO O MODELO ##

modelo_inicial<- lm(consumo ~ ( . - UF)^2 , data=dados)
summary(modelo_inicial)
tbl_regression(modelo_inicial)
tbl_regression(modelo_inicial,
               estimate_fun = function(x) style_ratio(x, digits = 4))


## SELECAO DE VARIAVEIS ##

melhor_modelo<- backward(modelo_inicial, alpha=0.1)
summary(melhor_modelo)
tbl_regression(melhor_modelo)
tbl_regression(melhor_modelo,
               estimate_fun = function(x) style_ratio(x, digits = 4))



## PREDICAO ##

preditos<- predict(melhor_modelo, newdata=novos)
preditos



## VISUALIZANDO MELHOR A PREDICAO ##

cbind.data.frame(novos,preditos)


