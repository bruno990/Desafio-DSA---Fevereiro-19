# Desafio 2 DSA

setwd("E:/Kaggle/DSA/Desafio2-Fevereiro")
getwd()

library(stringr)
library(tidyr)
require(dplyr)
library(plyr)
library(gbm)
library(ModelMetrics)
library(caret)
library(randomForest)
library(corrplot)
library(nlstools)

dados_teste <- read.csv("dataset_teste.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
dados_teste2 <- read.csv("dataset_teste.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")
dados_treino <- read.csv("dataset_treino.csv", header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

View(dados_treino)
str(dados_treino)


# Verificando se o data frame possui valores missing (NA)
anyNA(dados_treino)
dadosTreino1 <- na.omit(dados_treino)


# Criando caracter para armazenar nome das colunas
nomeColunas <- colnames(dados_treino)
write.csv(nomeColunas,"nomeColunas.csv")

# Criando váriavéis
dadosTreino1$codBairro <- (str_sub(dadosTreino1$BBL...10.digits, start = 1, end = 1))
dadosTreino1$taxBlock <- str_sub(dadosTreino1$BBL...10.digits, start = 2, end = 6)
dadosTreino1$taxLotNumber <- str_sub(dadosTreino1$BBL...10.digits, start = 7, end = 10)

dados_teste$codBairro <- (str_sub(dados_teste$BBL...10.digits, start = 1, end = 1))
dados_teste$taxBlock <- (str_sub(dados_teste$BBL...10.digits, start = 2, end = 6))
dados_teste$taxLotNumber <- (str_sub(dados_teste$BBL...10.digits, start = 7, end = 10))

View(dadosTreino1)

# Eliminando colunas desnecessárias
dadosTreino1$Borough <- NULL
dadosTreino1$Order <- NULL
dadosTreino1$Property.Name <- NULL
dadosTreino1$Parent.Property.Id <- NULL
dadosTreino1$Parent.Property.Name <- NULL
dadosTreino1$Address.2 <- NULL
dadosTreino1$Postal.Code <- NULL
dadosTreino1$NYC.Building.Identification.Number..BIN. <- NULL
dadosTreino1$Street.Number <- NULL
dadosTreino1[,c(1,2,3)] <- NULL
dadosTreino1[,c(40,41,42)] <- NULL
dadosTreino1$NTA <- NULL

nomeColunas <- colnames(dadosTreino1)
write.csv(nomeColunas,"nomeColunas2.csv")

View(dadosTreino1)
colsDouble <- c(7,9,11,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38)
dadosTreino1[,colsDouble] <- lapply(dadosTreino1[,colsDouble], as.double)

str(dadosTreino1)

dadosTreino1[is.na(dadosTreino1)] <- 0
any(is.na(dadosTreino1))

grafico <- function(x){
  boxplot(dadosTreino1[,x], col="bisque", xlab = colnames(dadosTreino1[x]))
}  

lapply(colsDouble, grafico)

dadosTreino1=dadosTreino1 %>% mutate_if(is.character, as.ordered)
str(dadosTreino1)

# Treino e Validação
trainIndex <- createDataPartition(dadosTreino1$ENERGY.STAR.Score, p = .8, 
                                  list = FALSE, 
                                  times = 1)

dadosTreino3_treino <- dadosTreino1[trainIndex, ]
dadosTreino3_teste <- dadosTreino1[-trainIndex, ]

View(dadosTreino3_treino)
str(dadosTreino3_treino)
any(is.na(dadosTreino3_treino))
modeloRandom <- randomForest(ENERGY.STAR.Score ~ ., data=dadosTreino3_treino, ntree = 500, importance = TRUE)
previsaoRandom <- predict(modeloRandom, dadosTreino3_teste)
modeloRandom
teste <- varImp(modeloRandom, numTrees = 500)
varImpPlot(modeloRandom, sort = TRUE)
mae(dadosTreino3_teste$ENERGY.STAR.Score, previsaoRandom)


#################################
colnames(dadosTreino3_treino)
colunasTreinadas <- colnames(dadosTreino3_treino[,-17])
colunasTreinadas
dados_teste <- dados_teste[,colunasTreinadas]
View(dados_teste)

colsDouble_teste <- c(3,7,9,11,13,14,17:38)

dados_teste[,colsDouble_teste] <- lapply(dados_teste[,colsDouble_teste], as.double)
dados_teste[is.na(dados_teste)] <- 0
dados_teste=dados_teste %>% mutate_if(is.character, as.ordered)
str(dados_teste)

previsao4 <- predict(modeloRandom, dados_teste)
View(previsao4)
hist(previsao4)
dados_teste$Score <- round(previsao4,0)
hist(dados_teste$Score)
View(dados_teste)
saida <- cbind(dados_teste2$Property.Id, dados_teste$Score)
View(saida)
colnames(saida) <- c("Property Id", "Score")
write.csv(saida, "saida8.csv", row.names = FALSE)

##############################