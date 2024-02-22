#########################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             
#########################################################################################
# 1 - Pacotes utilizados
#########################################################################################
pacotes <- c("readr", "dplyr","tidyverse")

#########################################################################################
# 2 - Instalando os pacotes
#########################################################################################
if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

#########################################################################################
# 3 - Importando os Dados
#########################################################################################
lognaut_fretes = read.csv('D:\\USP\\TCC\\TCC-USP\\lognaut_fretes.csv',sep=';',dec = ",")

#########################################################################################
# 4 - Realizando uma copia dos dados para preservar a matriz original
#########################################################################################
lognaut_dados <- lognaut_fretes 

#########################################################################################
# 5 - Etapa 1 - Data Wrangling, tratamento dos dados e eliminação de colunas irrelevan-
#               tes ao processamento
#########################################################################################
# 5.1 - Mostrando o nomes das colunas
#########################################################################################
names(lognaut_dados)

#########################################################################################
# 5.2 - Eliminando campos irrelevantes ao algoritimo                 
#########################################################################################
lognaut_dados$ptcli_operacao_id                               <- NULL                                    
lognaut_dados$ptcli_status_id                                 <- NULL                                      

#########################################################################################
# 5.3 - Variavel já é explicada pelas seguintes variavais do modelo(frete, origem e destino)
#########################################################################################
lognaut_dados$ttl_convertido                                  <- NULL

#########################################################################################
# 5.4 - Renomeando os campos, para facil manipulação e compreensão dos dados
#########################################################################################
novos_nomes <- c("agente",                                   
                 "qtdeEstadiaOrigem", 
                 "qtdeEstadiaDestino",
                 "qtdeRotas",                                           
                 "qtdeDias",                                             
                 "vlrFrete",                                                
                 "vlrOrigem",                                               
                 "vlrDestino")    

#########################################################################################
# 5.5 Atribuimos o vetor com nomes ao dataset
#########################################################################################
names(lognaut_dados) <- novos_nomes

#########################################################################################
# 6 - Tirando apresentação cientifica dos dados                 
#########################################################################################
format(lognaut_dados, scientific = FALSE)

#########################################################################################
# 7 - Determinação da Matriz de Decisão
#########################################################################################
# 7.1 - Gerando o Monotônico de Custo (Menor Melhor)
#########################################################################################
for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_qtdeEstadiaOrigem[i]  <- 1 / lognaut_dados[i, 2]
  lognaut_dados$fator_mc_qtdeEstadiaDestino[i] <- 1 / lognaut_dados[i, 3]
  lognaut_dados$fator_mc_qtdeRotas[i]          <- 1 / lognaut_dados[i, 4]
  lognaut_dados$fator_mc_qtdeDias[i]           <- 1 / lognaut_dados[i, 5]            
  lognaut_dados$fator_mc_vlrFrete[i]           <- 1 / lognaut_dados[i, 6]
  lognaut_dados$fator_mc_vlrOrigem[i]          <- 1 / lognaut_dados[i, 7]
  lognaut_dados$fator_mc_vlrDestino[i]         <- 1 / lognaut_dados[i, 8]
}

#########################################################################################
# 7.1 - Somando os fatores
#########################################################################################
somaQtdeEstadiaOrigem   = sum(lognaut_dados$fator_mc_qtdeEstadiaOrigem)
somaQtdeEstadiaDestino  = sum(lognaut_dados$fator_mc_qtdeEstadiaDestino)
somaQtdeRotas           = sum(lognaut_dados$fator_mc_qtdeRotas) 
somaQtdeDias            = sum(lognaut_dados$fator_mc_qtdeDias)
somaVlrFrete            = sum(lognaut_dados$fator_mc_vlrFrete)
somaVlrOrigem           = sum(lognaut_dados$fator_mc_vlrOrigem)
somaVlrDestino          = sum(lognaut_dados$fator_mc_vlrDestino) 

#########################################################################################
# 7.2 - Finalizando a geração dos fatores
#########################################################################################
for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_qtdeEstadiaOrigem[i]  <- lognaut_dados[i, 9]  / somaQtdeEstadiaOrigem
  lognaut_dados$fator_mc_qtdeEstadiaDestino[i] <- lognaut_dados[i, 10] / somaQtdeEstadiaDestino
  lognaut_dados$fator_mc_qtdeRotas[i]          <- lognaut_dados[i, 11] / somaQtdeRotas
  lognaut_dados$fator_mc_qtdeDias[i]           <- lognaut_dados[i, 12] / somaQtdeDias
  lognaut_dados$fator_mc_vlrFrete[i]           <- lognaut_dados[i, 13] / somaVlrFrete
  lognaut_dados$fator_mc_vlrOrigem[i]          <- lognaut_dados[i, 14] / somaVlrOrigem
  lognaut_dados$fator_mc_vlrDestino[i]         <- lognaut_dados[i, 15] / somaVlrDestino
}

#########################################################################################
# 8 - Calculo da média das alternativas em cada critério
#########################################################################################
# 8.1 - Transposição da Matriz
#########################################################################################
lognaut_dados_t <- t(lognaut_dados)

#########################################################################################
# 8.2 - Calculando as Medias
#########################################################################################
mediaQtdeEstadiaOrigem  <- mean(as.numeric(lognaut_dados_t[09, 1:ncol(lognaut_dados_t)]))
mediaQtdeEstadiaDestino <- mean(as.numeric(lognaut_dados_t[10, 1:ncol(lognaut_dados_t)]))
mediaQtdeRotas          <- mean(as.numeric(lognaut_dados_t[11, 1:ncol(lognaut_dados_t)]))
mediaQtdeDias           <- mean(as.numeric(lognaut_dados_t[12, 1:ncol(lognaut_dados_t)]))
mediaVlrFrete           <- mean(as.numeric(lognaut_dados_t[13, 1:ncol(lognaut_dados_t)]))
mediaVlrOrigem          <- mean(as.numeric(lognaut_dados_t[14, 1:ncol(lognaut_dados_t)]))
mediaVlrDestino         <- mean(as.numeric(lognaut_dados_t[15, 1:ncol(lognaut_dados_t)]))

#########################################################################################
# 8.3 - Criando uma lista com os critérios
#########################################################################################
criterios <- c("qtdeEstadiaOrigem", 
               "qtdeEstadiaDestino",
               "qtdeRotas",                                           
               "qtdeDias",                                             
               "vlrFrete",                                                
               "vlrOrigem",                                               
               "vlrDestino")

#########################################################################################
# 8.4 - Criando uma lista com as médias
#########################################################################################
media <- c(mediaQtdeEstadiaOrigem, 
           mediaQtdeEstadiaDestino, 
           mediaQtdeRotas,
           mediaQtdeDias,
           mediaVlrFrete,
           mediaVlrOrigem,
           mediaVlrDestino)

df         <- data.frame(criterios, media)
DT::datatable(df, rownames = TRUE)
#########################################################################################
# 9 - Cálculo do desvio padrão dos critérios com base na amostra das alternativas
#########################################################################################
devPadQtdeEstadiaOrigem  <- sd(as.numeric(lognaut_dados_t[09, 1:ncol(lognaut_dados_t)]))
devPadQtdeEstadiaDestino <- sd(as.numeric(lognaut_dados_t[10, 1:ncol(lognaut_dados_t)]))
devPadQtdeRotas          <- sd(as.numeric(lognaut_dados_t[11, 1:ncol(lognaut_dados_t)]))
devPadQtdeDias           <- sd(as.numeric(lognaut_dados_t[12, 1:ncol(lognaut_dados_t)]))
devPadVlrFrete           <- sd(as.numeric(lognaut_dados_t[13, 1:ncol(lognaut_dados_t)]))
devPadVlrOrigem          <- sd(as.numeric(lognaut_dados_t[14, 1:ncol(lognaut_dados_t)]))
devPadVlrDestino         <- sd(as.numeric(lognaut_dados_t[15, 1:ncol(lognaut_dados_t)]))

#########################################################################################
# 9.1 - Criando uma lista com os desvios padrão
#########################################################################################
desvioPadrao <- c(devPadQtdeEstadiaOrigem, 
                  devPadQtdeEstadiaDestino, 
                  devPadQtdeRotas,
                  devPadQtdeDias,
                  devPadVlrFrete,
                  devPadVlrOrigem,
                  devPadVlrDestino)

df         <- data.frame(criterios, desvioPadrao)
DT::datatable(df, rownames = TRUE)
#########################################################################################
# 10 - Cálculo do fator gaussiano para cada critério
#########################################################################################
fatGausQtdeEstadiaOrigem  <- devPadQtdeEstadiaOrigem  / mediaQtdeEstadiaOrigem 
fatGausQtdeEstadiaDestino <- devPadQtdeEstadiaDestino / mediaQtdeEstadiaDestino 
fatGausQtdeRotas          <- devPadQtdeRotas          / mediaQtdeRotas 
fatGausQtdeDias           <- devPadQtdeDias           / mediaQtdeDias 
fatGausVlrFrete           <- devPadVlrFrete           / mediaVlrFrete 
fatGausVlrOrigem          <- devPadVlrOrigem          / mediaVlrOrigem 
fatGausVlrDestino         <- devPadVlrDestino         / mediaVlrDestino 

#########################################################################################
# 10.1 - Criando uma lista com os fatores gaussiano
#########################################################################################
fatorGaussiano <- c(fatGausQtdeEstadiaOrigem, 
                    fatGausQtdeEstadiaDestino, 
                    fatGausQtdeRotas,
                    fatGausQtdeDias,
                    fatGausVlrFrete,
                    fatGausVlrOrigem,
                    fatGausVlrDestino)

df          <- data.frame(criterios, fatorGaussiano)
DT::datatable(df, rownames = TRUE)

#########################################################################################
# 11 - Cálculo do fator gaussiano normalizado para cada critério
# 11 - Fatores Gaussiano Normalizado
#########################################################################################
somaFatores <- fatGausQtdeEstadiaOrigem  + 
               fatGausQtdeEstadiaDestino + 
               fatGausQtdeRotas          + 
               fatGausQtdeDias           + 
               fatGausVlrFrete           + 
               fatGausVlrOrigem          + 
               fatGausVlrDestino

fatGausNormQtdeEstadiaOrigem  <- fatGausQtdeEstadiaOrigem  / somaFatores
fatGausNormQtdeEstadiaDestino <- fatGausQtdeEstadiaDestino / somaFatores
fatGausNormQtdeRotas          <- fatGausQtdeRotas          / somaFatores
fatGausNormQtdeDias           <- fatGausQtdeDias           / somaFatores
fatGausNormVlrFrete           <- fatGausVlrFrete           / somaFatores
fatGausNormVlrOrigem          <- fatGausVlrOrigem          / somaFatores
fatGausNormVlrDestino         <- fatGausVlrDestino         / somaFatores

#########################################################################################
# 11.1 - Criando uma lista com os fatores gaussiano normalizados
#########################################################################################
fatorGaussianoNormalizado <- c(fatGausNormQtdeEstadiaOrigem, 
                               fatGausNormQtdeEstadiaDestino, 
                               fatGausNormQtdeRotas,
                               fatGausNormQtdeDias,
                               fatGausNormVlrFrete,
                               fatGausNormVlrOrigem,
                               fatGausNormVlrDestino)

df   <- data.frame(criterios, fatorGaussianoNormalizado)
DT::datatable(df, rownames = TRUE)

#########################################################################################
# 12 - Ponderação da Matriz de Decisão
#########################################################################################
# 12.1 - Criando uma lista com as alternativas
#########################################################################################
alternativas <- c("Agente 01",
                  "Agente 02",
                  "Agente 03",
                  "Agente 04",
                  "Agente 05",
                  "Agente 06",
                  "Agente 07",
                  "Agente 08",
                  "Agente 09",
                  "Agente 10")

#########################################################################################
# 12.2 - Ponderação da Matriz para o critério: Qtde Estadias Origem
#########################################################################################
podMatrizQtdeEstadiaOrigemAg1  <- as.numeric(lognaut_dados_t[9, 1])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg2  <- as.numeric(lognaut_dados_t[9, 2])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg3  <- as.numeric(lognaut_dados_t[9, 3])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg4  <- as.numeric(lognaut_dados_t[9, 4])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg5  <- as.numeric(lognaut_dados_t[9, 5])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg6  <- as.numeric(lognaut_dados_t[9, 6])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg7  <- as.numeric(lognaut_dados_t[9, 7])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg8  <- as.numeric(lognaut_dados_t[9, 8])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg9  <- as.numeric(lognaut_dados_t[9, 9])  * fatGausNormQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg10 <- as.numeric(lognaut_dados_t[9, 10]) * fatGausNormQtdeEstadiaOrigem

podMatrizQtdeEstadiaOrigem <- c(podMatrizQtdeEstadiaOrigemAg1,
                                podMatrizQtdeEstadiaOrigemAg2,
                                podMatrizQtdeEstadiaOrigemAg3,
                                podMatrizQtdeEstadiaOrigemAg4,
                                podMatrizQtdeEstadiaOrigemAg5,
                                podMatrizQtdeEstadiaOrigemAg6,
                                podMatrizQtdeEstadiaOrigemAg7,
                                podMatrizQtdeEstadiaOrigemAg8,
                                podMatrizQtdeEstadiaOrigemAg9,
                                podMatrizQtdeEstadiaOrigemAg10)

#########################################################################################
#Etapa 12.3 - Ponderação da Matriz para o critério: Qtde Estadias Destino
#########################################################################################
podMatrizQtdeEstadiaDestinoAg1  <- as.numeric(lognaut_dados_t[10, 1])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg2  <- as.numeric(lognaut_dados_t[10, 2])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg3  <- as.numeric(lognaut_dados_t[10, 3])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg4  <- as.numeric(lognaut_dados_t[10, 4])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg5  <- as.numeric(lognaut_dados_t[10, 5])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg6  <- as.numeric(lognaut_dados_t[10, 6])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg7  <- as.numeric(lognaut_dados_t[10, 7])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg8  <- as.numeric(lognaut_dados_t[10, 8])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg9  <- as.numeric(lognaut_dados_t[10, 9])  * fatGausNormQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg10 <- as.numeric(lognaut_dados_t[10, 10]) * fatGausNormQtdeEstadiaDestino

podMatrizQtdeDestinoDestino <- c(podMatrizQtdeEstadiaDestinoAg1,
                                 podMatrizQtdeEstadiaDestinoAg2,
                                 podMatrizQtdeEstadiaDestinoAg3,
                                 podMatrizQtdeEstadiaDestinoAg4,
                                 podMatrizQtdeEstadiaDestinoAg5,
                                 podMatrizQtdeEstadiaDestinoAg6,
                                 podMatrizQtdeEstadiaDestinoAg7,
                                 podMatrizQtdeEstadiaDestinoAg8,
                                 podMatrizQtdeEstadiaDestinoAg9,
                                 podMatrizQtdeEstadiaDestinoAg10)

#########################################################################################
#Etapa 12.4 - Ponderação da Matriz para o critério: Qtde Rotas
#########################################################################################
podMatrizQtdeRotasAg1  <- as.numeric(lognaut_dados_t[11, 1])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg2  <- as.numeric(lognaut_dados_t[11, 2])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg3  <- as.numeric(lognaut_dados_t[11, 3])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg4  <- as.numeric(lognaut_dados_t[11, 4])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg5  <- as.numeric(lognaut_dados_t[11, 5])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg6  <- as.numeric(lognaut_dados_t[11, 6])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg7  <- as.numeric(lognaut_dados_t[11, 7])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg8  <- as.numeric(lognaut_dados_t[11, 8])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg9  <- as.numeric(lognaut_dados_t[11, 9])  * fatGausNormQtdeRotas
podMatrizQtdeRotasAg10 <- as.numeric(lognaut_dados_t[11, 10]) * fatGausNormQtdeRotas

podMatrizQtdeQtdeRotas <- c(podMatrizQtdeRotasAg1,
                            podMatrizQtdeRotasAg2,
                            podMatrizQtdeRotasAg3,
                            podMatrizQtdeRotasAg4,
                            podMatrizQtdeRotasAg5,
                            podMatrizQtdeRotasAg6,
                            podMatrizQtdeRotasAg7,
                            podMatrizQtdeRotasAg8,
                            podMatrizQtdeRotasAg9,
                            podMatrizQtdeRotasAg10)

#########################################################################################
# 12.5 - Ponderação da Matriz para o critério: Qtde Dias
#########################################################################################
podMatrizQtdeDiasAg1  <- as.numeric(lognaut_dados_t[12, 1])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg2  <- as.numeric(lognaut_dados_t[12, 2])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg3  <- as.numeric(lognaut_dados_t[12, 3])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg4  <- as.numeric(lognaut_dados_t[12, 4])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg5  <- as.numeric(lognaut_dados_t[12, 5])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg6  <- as.numeric(lognaut_dados_t[12, 6])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg7  <- as.numeric(lognaut_dados_t[12, 7])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg8  <- as.numeric(lognaut_dados_t[12, 8])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg9  <- as.numeric(lognaut_dados_t[12, 9])  * fatGausNormQtdeDias
podMatrizQtdeDiasAg10 <- as.numeric(lognaut_dados_t[12, 10]) * fatGausNormQtdeDias

podMatrizQtdeQtdeDias <- c(podMatrizQtdeDiasAg1,
                           podMatrizQtdeDiasAg2,
                           podMatrizQtdeDiasAg3,
                           podMatrizQtdeDiasAg4,
                           podMatrizQtdeDiasAg5,
                           podMatrizQtdeDiasAg6,
                           podMatrizQtdeDiasAg7,
                           podMatrizQtdeDiasAg8,
                           podMatrizQtdeDiasAg9,
                           podMatrizQtdeDiasAg10)

#########################################################################################
# 12.6 - Ponderação da Matriz para o critério: Valor Frete
#########################################################################################
podMatrizVlrFreteAg1  <- as.numeric(lognaut_dados_t[13, 1])  * fatGausNormVlrFrete
podMatrizVlrFreteAg2  <- as.numeric(lognaut_dados_t[13, 2])  * fatGausNormVlrFrete
podMatrizVlrFreteAg3  <- as.numeric(lognaut_dados_t[13, 3])  * fatGausNormVlrFrete
podMatrizVlrFreteAg4  <- as.numeric(lognaut_dados_t[13, 4])  * fatGausNormVlrFrete
podMatrizVlrFreteAg5  <- as.numeric(lognaut_dados_t[13, 5])  * fatGausNormVlrFrete
podMatrizVlrFreteAg6  <- as.numeric(lognaut_dados_t[13, 6])  * fatGausNormVlrFrete
podMatrizVlrFreteAg7  <- as.numeric(lognaut_dados_t[13, 7])  * fatGausNormVlrFrete
podMatrizVlrFreteAg8  <- as.numeric(lognaut_dados_t[13, 8])  * fatGausNormVlrFrete
podMatrizVlrFreteAg9  <- as.numeric(lognaut_dados_t[13, 9])  * fatGausNormVlrFrete
podMatrizVlrFreteAg10 <- as.numeric(lognaut_dados_t[13, 10]) * fatGausNormVlrFrete

MatrizPonderacaoVlrFrete <- c(podMatrizVlrFreteAg1,
                       podMatrizVlrFreteAg2,
                       podMatrizVlrFreteAg3,
                       podMatrizVlrFreteAg4,
                       podMatrizVlrFreteAg5,
                       podMatrizVlrFreteAg6,
                       podMatrizVlrFreteAg7,
                       podMatrizVlrFreteAg8,
                       podMatrizVlrFreteAg9,
                       podMatrizVlrFreteAg10)

df  <- data.frame(alternativas, MatrizPonderacaoVlrFrete)
DT::datatable(df, rownames = TRUE)

E#########################################################################################
# 12.7 - Ponderação da Matriz para o critério: Valor Origem
#########################################################################################
podMatrizVlrOrigemAg1  <- as.numeric(lognaut_dados_t[14, 1])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg2  <- as.numeric(lognaut_dados_t[14, 2])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg3  <- as.numeric(lognaut_dados_t[14, 3])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg4  <- as.numeric(lognaut_dados_t[14, 4])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg5  <- as.numeric(lognaut_dados_t[14, 5])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg6  <- as.numeric(lognaut_dados_t[14, 6])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg7  <- as.numeric(lognaut_dados_t[14, 7])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg8  <- as.numeric(lognaut_dados_t[14, 8])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg9  <- as.numeric(lognaut_dados_t[14, 9])  * fatGausNormVlrOrigem
podMatrizVlrOrigemAg10 <- as.numeric(lognaut_dados_t[14, 10]) * fatGausNormVlrOrigem

podMatrizVlrOrigem <- c(podMatrizVlrOrigemAg1,
                        podMatrizVlrOrigemAg2,
                        podMatrizVlrOrigemAg3,
                        podMatrizVlrOrigemAg4,
                        podMatrizVlrOrigemAg5,
                        podMatrizVlrOrigemAg6,
                        podMatrizVlrOrigemAg7,
                        podMatrizVlrOrigemAg8,
                        podMatrizVlrOrigemAg9,
                        podMatrizVlrOrigemAg10)

#########################################################################################
# 12.8 - Ponderação da Matriz para o critério: Valor Destino
#########################################################################################
podMatrizVlrDestinoAg1  <- as.numeric(lognaut_dados_t[15, 1])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg2  <- as.numeric(lognaut_dados_t[15, 2])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg3  <- as.numeric(lognaut_dados_t[15, 3])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg4  <- as.numeric(lognaut_dados_t[15, 4])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg5  <- as.numeric(lognaut_dados_t[15, 5])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg6  <- as.numeric(lognaut_dados_t[15, 6])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg7  <- as.numeric(lognaut_dados_t[15, 7])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg8  <- as.numeric(lognaut_dados_t[15, 8])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg9  <- as.numeric(lognaut_dados_t[15, 9])  * fatGausNormVlrDestino
podMatrizVlrDestinoAg10 <- as.numeric(lognaut_dados_t[15, 10]) * fatGausNormVlrDestino

podMatrizVlrDestino <- c(podMatrizVlrDestinoAg1,
                         podMatrizVlrDestinoAg2,
                         podMatrizVlrDestinoAg3,
                         podMatrizVlrDestinoAg4,
                         podMatrizVlrDestinoAg5,
                         podMatrizVlrDestinoAg6,
                         podMatrizVlrDestinoAg7,
                         podMatrizVlrDestinoAg8,
                         podMatrizVlrDestinoAg9,
                         podMatrizVlrDestinoAg10)

df         <- data.frame(alternativas, 
                         podMatrizQtdeEstadiaOrigem, 
                         podMatrizQtdeDestinoDestino,
                         podMatrizQtdeQtdeRotas,
                         podMatrizQtdeQtdeDias,
                         podMatrizVlrFrete,
                         podMatrizVlrOrigem,
                         podMatrizVlrDestino)


#########################################################################################
# 13 - Obtenção do Ranking
#########################################################################################
# 13.1 - Somando as ponderações de todos os critérios para a alternativa: Agente 1
#########################################################################################
RankAg1 <- podMatrizQtdeEstadiaOrigemAg1 + 
           podMatrizQtdeEstadiaDestinoAg1 +
           podMatrizQtdeRotasAg1 + 
           podMatrizQtdeDiasAg1 + 
           podMatrizVlrFreteAg1 + 
           podMatrizVlrOrigemAg1 +
           podMatrizVlrDestinoAg1 

#########################################################################################
# 13.2 - Somando as ponderações de todos os critérios para a alternativa: Agente 2
#########################################################################################
RankAg2 <- podMatrizQtdeEstadiaOrigemAg2 + 
           podMatrizQtdeEstadiaDestinoAg2 +
           podMatrizQtdeRotasAg2 + 
           podMatrizQtdeDiasAg2 + 
           podMatrizVlrFreteAg2 + 
           podMatrizVlrOrigemAg2 +
           podMatrizVlrDestinoAg2 

#########################################################################################
# 13.3 - Somando as ponderações de todos os critérios para a alternativa: Agente 3
#########################################################################################
RankAg3 <- podMatrizQtdeEstadiaOrigemAg3 + 
           podMatrizQtdeEstadiaDestinoAg3 +
           podMatrizQtdeRotasAg3 + 
           podMatrizQtdeDiasAg3 + 
           podMatrizVlrFreteAg3 + 
           podMatrizVlrOrigemAg3 +
           podMatrizVlrDestinoAg3 

#########################################################################################
# 13.4 - Somando as ponderações de todos os critérios para a alternativa: Agente 4
#########################################################################################
RankAg4 <- podMatrizQtdeEstadiaOrigemAg4 + 
           podMatrizQtdeEstadiaDestinoAg4 +
           podMatrizQtdeRotasAg4 + 
           podMatrizQtdeDiasAg4 + 
           podMatrizVlrFreteAg4 + 
           podMatrizVlrOrigemAg4 +
           podMatrizVlrDestinoAg4 

#########################################################################################
# 13.5 - Somando as ponderações de todos os critérios para a alternativa: Agente 5
#########################################################################################
RankAg5 <- podMatrizQtdeEstadiaOrigemAg5 + 
           podMatrizQtdeEstadiaDestinoAg5 +
           podMatrizQtdeRotasAg5 + 
           podMatrizQtdeDiasAg5 + 
           podMatrizVlrFreteAg5 + 
           podMatrizVlrOrigemAg5 +
           podMatrizVlrDestinoAg5 

#########################################################################################
# 13.6 - Somando as ponderações de todos os critérios para a alternativa: Agente 6
#########################################################################################
RankAg6 <- podMatrizQtdeEstadiaOrigemAg6 + 
           podMatrizQtdeEstadiaDestinoAg6 +
           podMatrizQtdeRotasAg6 + 
           podMatrizQtdeDiasAg6 + 
           podMatrizVlrFreteAg6 + 
           podMatrizVlrOrigemAg6 +
           podMatrizVlrDestinoAg6 

#########################################################################################
# 13.7 - Somando as ponderações de todos os critérios para a alternativa: Agente 7
#########################################################################################
RankAg7 <- podMatrizQtdeEstadiaOrigemAg7 + 
           podMatrizQtdeEstadiaDestinoAg7 +
           podMatrizQtdeRotasAg7 + 
           podMatrizQtdeDiasAg7 + 
           podMatrizVlrFreteAg7 + 
           podMatrizVlrOrigemAg7 +
           podMatrizVlrDestinoAg7 

#########################################################################################
# 13.8 - Somando as ponderações de todos os critérios para a alternativa: Agente 8
#########################################################################################
RankAg8 <- podMatrizQtdeEstadiaOrigemAg8 + 
           podMatrizQtdeEstadiaDestinoAg8 +
           podMatrizQtdeRotasAg8 + 
           podMatrizQtdeDiasAg8 + 
           podMatrizVlrFreteAg8 + 
           podMatrizVlrOrigemAg8 +
           podMatrizVlrDestinoAg8 

#########################################################################################
# 13.9 - Somando as ponderações de todos os critérios para a alternativa: Agente 9
#########################################################################################
RankAg9 <- podMatrizQtdeEstadiaOrigemAg9 + 
           podMatrizQtdeEstadiaDestinoAg9 +
           podMatrizQtdeRotasAg9 + 
           podMatrizQtdeDiasAg9 + 
           podMatrizVlrFreteAg9 + 
           podMatrizVlrOrigemAg9 +
           podMatrizVlrDestinoAg9 

#########################################################################################
# 13.10 - Somando as ponderações de todos os critérios para a alternativa: Agente 10
#########################################################################################
RankAg10 <- podMatrizQtdeEstadiaOrigemAg10 + 
            podMatrizQtdeEstadiaDestinoAg10 +
            podMatrizQtdeRotasAg10 + 
            podMatrizQtdeDiasAg10 + 
            podMatrizVlrFreteAg10 + 
            podMatrizVlrOrigemAg10 +
            podMatrizVlrDestinoAg10

rankings <- c(RankAg1,
             RankAg2,
             RankAg3,
             RankAg4,
             RankAg5,
             RankAg6,
             RankAg7,
             RankAg8,
             RankAg9,
             RankAg10)


df  <- data.frame(alternativas, rankings)
DT::datatable(df, rownames = TRUE)
#########################################################################################
# 14 - Adicionando Ranking na Ultima linha
#########################################################################################
temp = list(V1=as.character(RankAg1), 
            V2=as.character(RankAg2), 
            V3=as.character(RankAg3), 
            V4=as.character(RankAg4), 
            V5=as.character(RankAg5), 
            V6=as.character(RankAg6), 
            V7=as.character(RankAg7), 
            V8=as.character(RankAg8), 
            V9=as.character(RankAg9), 
            V10=as.character(RankAg10))

temp = rbind(lognaut_dados_t, temp)

#########################################################################################
#Etapa 15 - Criando estrutura de apresentação
#########################################################################################
# 15.1 - Criando lista de alternativas a partir da matriz transposta
#########################################################################################
alternativas <- c(as.character(temp[1,1]), 
                  as.character(temp[1,2]), 
                  as.character(temp[1,3]),
                  as.character(temp[1,4]),
                  as.character(temp[1,5]),
                  as.character(temp[1,6]),
                  as.character(temp[1,7]),
                  as.character(temp[1,8]),
                  as.character(temp[1,9]),
                  as.character(temp[1,10]))

#########################################################################################
# 15.2 - Criando a lista de critérios a partir da matriz transposta
#########################################################################################
criterios  <- c(as.numeric(temp[16,1]),  
                as.numeric(temp[16,2]),  
                as.numeric(temp[16,3]),
                as.numeric(temp[16,4]),
                as.numeric(temp[16,5]),
                as.numeric(temp[16,6]),
                as.numeric(temp[16,7]),
                as.numeric(temp[16,8]),
                as.numeric(temp[16,9]),
                as.numeric(temp[16,10]))

df         <- data.frame(alternativas, criterios)

#########################################################################################
# 16 - Ordenando o Ranking
#########################################################################################
ranking <- print(df[order(df$criterios, decreasing = TRUE), ])
DT::datatable(ranking, rownames = TRUE)
