##################################################################################
#                  INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS             #
##################################################################################
#Pacotes utilizados
pacotes <- c("readr", "dplyr","tidyverse")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

##################################################################################
# importar os dados
##################################################################################
lognaut_fretes = read.csv('D:\\USP\\TCC\\TCC-USP\\lognaut_fretes.csv',sep=';',dec = ",")
lognaut_dados <- lognaut_fretes 

##################################################################################
#Etapa 1 - Data Wrangling                  
##################################################################################

#1.1 - Eliminando campos irrelevantes ao algoritimo                 
##################################################################################
names(lognaut_dados)

lognaut_dados$ptcli_operacao_id                               <- NULL                                    
lognaut_dados$ptcli_status_id                                 <- NULL                                      
lognaut_dados$ptcli_operacao_cotacao_id                       <- NULL                            
lognaut_dados$ptcli_agente_id                                 <- NULL                                      
lognaut_dados$ptcli_operacao_liberada                         <- NULL                              
lognaut_dados$ptcli_operacao_cotacao_frete_inf_rota_descricao <- NULL      
lognaut_dados$ptcli_operacao_embarque_prev_embarque           <- NULL                
lognaut_dados$ptcli_operacao_embarque_prev_entrega            <- NULL                 
lognaut_dados$moeda                                           <- NULL 

#1.2 - Renomeando os campos
##################################################################################
names(lognaut_dados)

novos_nomes <- c("numero",                                
                 "agente",                                   
                 "qtdeEstadiaOrigem", 
                 "qtdeEstadiaDestino",
                 "qtdeRotas",                                           
                 "qtdeDias",                                             
                 "vlrFrete",                                                
                 "vlrOrigem",                                               
                 "vlrDestino",                                              
                 "vlrTotal")    

# 2º: Em seguida, atribuimos o vetor com nomes ao dataset
##################################################################################
names(lognaut_dados) <- novos_nomes

#Tirando apresentação cientifica dos dados                 
##################################################################################
format(lognaut_dados, scientific = FALSE)

##################################################################################
#Etapa 1 - Determinação da Matriz de Decisão
##################################################################################

#Etapa 1.1 - Monotônico de Custo (Menor Melhor)
##################################################################################
for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_qtdeEstadiaOrigem[i]  <- 1 / lognaut_dados[i:nrow(lognaut_dados), 3]
  lognaut_dados$fator_mc_qtdeEstadiaDestino[i] <- 1 / lognaut_dados[i:nrow(lognaut_dados), 4]
  lognaut_dados$fator_mc_qtdeRotas[i]          <- 1 / lognaut_dados[i:nrow(lognaut_dados), 5]
  lognaut_dados$fator_mc_qtdeDias[i]           <- 1 / lognaut_dados[i:nrow(lognaut_dados), 6]            

  lognaut_dados$fator_mc_vlrFrete[i]           <- 1 / lognaut_dados[i:nrow(lognaut_dados), 7]
  lognaut_dados$fator_mc_vlrOrigem[i]          <- 1 / lognaut_dados[i:nrow(lognaut_dados), 8]
  lognaut_dados$fator_mc_vlrDestino[i]         <- 1 / lognaut_dados[i:nrow(lognaut_dados), 9]
  lognaut_dados$fator_mc_vlrTotal[i]           <- 1 / lognaut_dados[i:nrow(lognaut_dados), 10]
}

somaQtdeEstadiaOrigem   = sum(lognaut_dados$fator_mc_qtdeEstadiaOrigem)
somaQtdeEstadiaDestino  = sum(lognaut_dados$fator_mc_qtdeEstadiaDestino)
somaQtdeRotas           = sum(lognaut_dados$fator_mc_qtdeRotas) 
somaQtdeDias            = sum(lognaut_dados$fator_mc_qtdeDias)

somaVlrFrete   = sum(lognaut_dados$fator_mc_vlrFrete)
somaVlrOrigem  = sum(lognaut_dados$fator_mc_vlrOrigem)
somaVlrDestino = sum(lognaut_dados$fator_mc_vlrDestino) 
somaVlrTotal   = sum(lognaut_dados$fator_mc_vlrTotal)

for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_qtdeEstadiaOrigem[i]  <- lognaut_dados[i:nrow(lognaut_dados), 3]   / somaQtdeEstadiaOrigem
  lognaut_dados$fator_mc_qtdeEstadiaDestino[i] <- lognaut_dados[i:nrow(lognaut_dados), 4]   / somaQtdeEstadiaDestino
  lognaut_dados$fator_mc_qtdeRotas[i]          <- lognaut_dados[i:nrow(lognaut_dados), 5]   / somaQtdeRotas
  lognaut_dados$fator_mc_qtdeDias[i]           <- lognaut_dados[i:nrow(lognaut_dados), 6]   / somaQtdeDias

  lognaut_dados$fator_mc_vlrFrete[i]   <- lognaut_dados[i:nrow(lognaut_dados), 7]   / somaVlrFrete
  lognaut_dados$fator_mc_vlrOrigem[i]  <- lognaut_dados[i:nrow(lognaut_dados), 8]   / somaVlrOrigem
  lognaut_dados$fator_mc_vlrDestino[i] <- lognaut_dados[i:nrow(lognaut_dados), 9]   / somaVlrDestino
  lognaut_dados$fator_mc_vlrTotal[i]   <- lognaut_dados[i:nrow(lognaut_dados), 10] / somaVlrTotal
}

##################################################################################
#Etapa 1.2 - Monotônico de Lucro (Maior Melhor)
##################################################################################
#Não se aplica ao modelo

##################################################################################
#Etapa 2 - Calculo da média das alternativas em cada critério
##################################################################################
# Transposição da Matriz
lognaut_dados_t <- t(lognaut_dados)

##################################################################################
#Calculando as Medias
##################################################################################
mediaQtdeEstadiaOrigem  <- mean(as.numeric(lognaut_dados_t[3:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaQtdeEstadiaDestino <- mean(as.numeric(lognaut_dados_t[4:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaQtdeRotas          <- mean(as.numeric(lognaut_dados_t[5:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaQtdeDias           <- mean(as.numeric(lognaut_dados_t[6:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))

mediaVlrFrete           <- mean(as.numeric(lognaut_dados_t[7:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaVlrOrigem          <- mean(as.numeric(lognaut_dados_t[8:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaVlrDestino         <- mean(as.numeric(lognaut_dados_t[9:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
mediaVlrTotal           <- mean(as.numeric(lognaut_dados_t[10:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)])) 

##################################################################################
#Etapa 4 - Cálculo do desvio padrão dos critérios com base na amostra das alternativas
##################################################################################
devPadQtdeEstadiaOrigem  <- sd(as.numeric(lognaut_dados_t[3:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadQtdeEstadiaDestino <- sd(as.numeric(lognaut_dados_t[4:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadQtdeRotas          <- sd(as.numeric(lognaut_dados_t[5:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadQtdeDias           <- sd(as.numeric(lognaut_dados_t[6:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))

devPadVlrFrete           <- sd(as.numeric(lognaut_dados_t[7:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadVlrOrigem          <- sd(as.numeric(lognaut_dados_t[8:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadVlrDestino         <- sd(as.numeric(lognaut_dados_t[9:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)]))
devPadVlrTotal           <- sd(as.numeric(lognaut_dados_t[10:nrow(lognaut_dados_t), 1:ncol(lognaut_dados_t)])) 

##################################################################################
#Etapa 4 - Cálculo do fator gaussiano para cada critério
##################################################################################
fatGausQtdeEstadiaOrigem  <- devPadQtdeEstadiaOrigem  / mediaQtdeEstadiaOrigem 
fatGausQtdeEstadiaDestino <- devPadQtdeEstadiaDestino / mediaQtdeEstadiaDestino 
fatGausQtdeRotas          <- devPadQtdeRotas          / mediaQtdeRotas 
fatGausQtdeDias           <- devPadQtdeDias           / mediaQtdeDias 

fatGausVlrFrete           <- devPadVlrFrete           / mediaVlrFrete 
fatGausVlrOrigem          <- devPadVlrOrigem          / mediaVlrOrigem 
fatGausVlrDestino         <- devPadVlrDestino         / mediaVlrDestino 
fatGausVlrTotal           <- devPadVlrTotal           / mediaVlrTotal 

##################################################################################
#Etapa 4.1 - Cálculo do fator gaussiano normalizado para cada critério
##################################################################################
somaFatores <- fatGausQtdeEstadiaOrigem  + 
               fatGausQtdeEstadiaDestino + 
               fatGausQtdeRotas          + 
               fatGausQtdeDias           + 
               fatGausVlrFrete           + 
               fatGausVlrOrigem          + 
               fatGausVlrDestino         + 
               fatGausVlrTotal

fatGausNormQtdeEstadiaOrigem  <- fatGausQtdeEstadiaOrigem  / somaFatores
fatGausNormQtdeEstadiaDestino <- fatGausQtdeEstadiaDestino / somaFatores
fatGausNormQtdeRotas          <- fatGausQtdeRotas          / somaFatores
fatGausNormQtdeDias           <- fatGausQtdeDias           / somaFatores

fatGausNormVlrFrete           <- fatGausVlrFrete           / somaFatores
fatGausNormVlrOrigem          <- fatGausVlrOrigem          / somaFatores
fatGausNormVlrDestino         <- fatGausVlrDestino         / somaFatores
fatGausNormVlrTotal           <- fatGausVlrTotal           / somaFatores

##################################################################################
#Etapa 5 - Ponderação da Matriz de Decisão
##################################################################################
podMatrizQtdeEstadiaOrigemAg1  <- as.numeric(lognaut_dados_t[11, 1]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg2  <- as.numeric(lognaut_dados_t[11, 2]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg3  <- as.numeric(lognaut_dados_t[11, 3]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg4  <- as.numeric(lognaut_dados_t[11, 4]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg5  <- as.numeric(lognaut_dados_t[11, 5]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg6  <- as.numeric(lognaut_dados_t[11, 6]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg7  <- as.numeric(lognaut_dados_t[11, 7]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg8  <- as.numeric(lognaut_dados_t[11, 8]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg9  <- as.numeric(lognaut_dados_t[11, 9]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg10 <- as.numeric(lognaut_dados_t[11, 10]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg11 <- as.numeric(lognaut_dados_t[11, 11]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg12 <- as.numeric(lognaut_dados_t[11, 12]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg13 <- as.numeric(lognaut_dados_t[11, 13]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg14 <- as.numeric(lognaut_dados_t[11, 14]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg15 <- as.numeric(lognaut_dados_t[11, 15]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg16 <- as.numeric(lognaut_dados_t[11, 16]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg17 <- as.numeric(lognaut_dados_t[11, 17]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg18 <- as.numeric(lognaut_dados_t[11, 18]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg19 <- as.numeric(lognaut_dados_t[11, 19]) * fatGausQtdeEstadiaOrigem
podMatrizQtdeEstadiaOrigemAg20 <- as.numeric(lognaut_dados_t[11, 20]) * fatGausQtdeEstadiaOrigem

podMatrizQtdeEstadiaDestinoAg1  <- as.numeric(lognaut_dados_t[12, 1]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg2  <- as.numeric(lognaut_dados_t[12, 2]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg3  <- as.numeric(lognaut_dados_t[12, 3]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg4  <- as.numeric(lognaut_dados_t[12, 4]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg5  <- as.numeric(lognaut_dados_t[12, 5]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg6  <- as.numeric(lognaut_dados_t[12, 6]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg7  <- as.numeric(lognaut_dados_t[12, 7]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg8  <- as.numeric(lognaut_dados_t[12, 8]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg9  <- as.numeric(lognaut_dados_t[12, 9]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg10 <- as.numeric(lognaut_dados_t[12, 10]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg11 <- as.numeric(lognaut_dados_t[12, 11]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg12 <- as.numeric(lognaut_dados_t[12, 12]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg13 <- as.numeric(lognaut_dados_t[12, 13]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg14 <- as.numeric(lognaut_dados_t[12, 14]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg15 <- as.numeric(lognaut_dados_t[12, 15]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg16 <- as.numeric(lognaut_dados_t[12, 16]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg17 <- as.numeric(lognaut_dados_t[12, 17]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg18 <- as.numeric(lognaut_dados_t[12, 18]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg19 <- as.numeric(lognaut_dados_t[12, 19]) * fatGausQtdeEstadiaDestino
podMatrizQtdeEstadiaDestinoAg20 <- as.numeric(lognaut_dados_t[12, 20]) * fatGausQtdeEstadiaDestino

podMatrizQtdeRotasAg1  <- as.numeric(lognaut_dados_t[13, 1]) * fatGausQtdeRotas
podMatrizQtdeRotasAg2  <- as.numeric(lognaut_dados_t[13, 2]) * fatGausQtdeRotas
podMatrizQtdeRotasAg3  <- as.numeric(lognaut_dados_t[13, 3]) * fatGausQtdeRotas
podMatrizQtdeRotasAg4  <- as.numeric(lognaut_dados_t[13, 4]) * fatGausQtdeRotas
podMatrizQtdeRotasAg5  <- as.numeric(lognaut_dados_t[13, 5]) * fatGausQtdeRotas
podMatrizQtdeRotasAg6  <- as.numeric(lognaut_dados_t[13, 6]) * fatGausQtdeRotas
podMatrizQtdeRotasAg7  <- as.numeric(lognaut_dados_t[13, 7]) * fatGausQtdeRotas
podMatrizQtdeRotasAg8  <- as.numeric(lognaut_dados_t[13, 8]) * fatGausQtdeRotas
podMatrizQtdeRotasAg9  <- as.numeric(lognaut_dados_t[13, 9]) * fatGausQtdeRotas
podMatrizQtdeRotasAg10 <- as.numeric(lognaut_dados_t[13, 10]) * fatGausQtdeRotas
podMatrizQtdeRotasAg11 <- as.numeric(lognaut_dados_t[13, 11]) * fatGausQtdeRotas
podMatrizQtdeRotasAg12 <- as.numeric(lognaut_dados_t[13, 12]) * fatGausQtdeRotas
podMatrizQtdeRotasAg13 <- as.numeric(lognaut_dados_t[13, 13]) * fatGausQtdeRotas
podMatrizQtdeRotasAg14 <- as.numeric(lognaut_dados_t[13, 14]) * fatGausQtdeRotas
podMatrizQtdeRotasAg15 <- as.numeric(lognaut_dados_t[13, 15]) * fatGausQtdeRotas
podMatrizQtdeRotasAg16 <- as.numeric(lognaut_dados_t[13, 16]) * fatGausQtdeRotas
podMatrizQtdeRotasAg17 <- as.numeric(lognaut_dados_t[13, 17]) * fatGausQtdeRotas
podMatrizQtdeRotasAg18 <- as.numeric(lognaut_dados_t[13, 18]) * fatGausQtdeRotas
podMatrizQtdeRotasAg19 <- as.numeric(lognaut_dados_t[13, 19]) * fatGausQtdeRotas
podMatrizQtdeRotasAg20 <- as.numeric(lognaut_dados_t[13, 20]) * fatGausQtdeRotas

podMatrizQtdeDiasAg1  <- as.numeric(lognaut_dados_t[14, 1]) * fatGausQtdeDias
podMatrizQtdeDiasAg2  <- as.numeric(lognaut_dados_t[14, 2]) * fatGausQtdeDias
podMatrizQtdeDiasAg3  <- as.numeric(lognaut_dados_t[14, 3]) * fatGausQtdeDias
podMatrizQtdeDiasAg4  <- as.numeric(lognaut_dados_t[14, 4]) * fatGausQtdeDias
podMatrizQtdeDiasAg5  <- as.numeric(lognaut_dados_t[14, 5]) * fatGausQtdeDias
podMatrizQtdeDiasAg6  <- as.numeric(lognaut_dados_t[14, 6]) * fatGausQtdeDias
podMatrizQtdeDiasAg7  <- as.numeric(lognaut_dados_t[14, 7]) * fatGausQtdeDias
podMatrizQtdeDiasAg8  <- as.numeric(lognaut_dados_t[14, 8]) * fatGausQtdeDias
podMatrizQtdeDiasAg9  <- as.numeric(lognaut_dados_t[14, 9]) * fatGausQtdeDias
podMatrizQtdeDiasAg10 <- as.numeric(lognaut_dados_t[14, 10]) * fatGausQtdeDias
podMatrizQtdeDiasAg11 <- as.numeric(lognaut_dados_t[14, 11]) * fatGausQtdeDias
podMatrizQtdeDiasAg12 <- as.numeric(lognaut_dados_t[14, 12]) * fatGausQtdeDias
podMatrizQtdeDiasAg13 <- as.numeric(lognaut_dados_t[14, 13]) * fatGausQtdeDias
podMatrizQtdeDiasAg14 <- as.numeric(lognaut_dados_t[14, 14]) * fatGausQtdeDias
podMatrizQtdeDiasAg15 <- as.numeric(lognaut_dados_t[14, 15]) * fatGausQtdeDias
podMatrizQtdeDiasAg16 <- as.numeric(lognaut_dados_t[14, 16]) * fatGausQtdeDias
podMatrizQtdeDiasAg17 <- as.numeric(lognaut_dados_t[14, 17]) * fatGausQtdeDias
podMatrizQtdeDiasAg18 <- as.numeric(lognaut_dados_t[14, 18]) * fatGausQtdeDias
podMatrizQtdeDiasAg19 <- as.numeric(lognaut_dados_t[14, 19]) * fatGausQtdeDias
podMatrizQtdeDiasAg20 <- as.numeric(lognaut_dados_t[14, 20]) * fatGausQtdeDias

podMatrizVlrFreteAg1  <- as.numeric(lognaut_dados_t[15, 1]) * fatGausVlrFrete
podMatrizVlrFreteAg2  <- as.numeric(lognaut_dados_t[15, 2]) * fatGausVlrFrete
podMatrizVlrFreteAg3  <- as.numeric(lognaut_dados_t[15, 3]) * fatGausVlrFrete
podMatrizVlrFreteAg4  <- as.numeric(lognaut_dados_t[15, 4]) * fatGausVlrFrete
podMatrizVlrFreteAg5  <- as.numeric(lognaut_dados_t[15, 5]) * fatGausVlrFrete
podMatrizVlrFreteAg6  <- as.numeric(lognaut_dados_t[15, 6]) * fatGausVlrFrete
podMatrizVlrFreteAg7  <- as.numeric(lognaut_dados_t[15, 7]) * fatGausVlrFrete
podMatrizVlrFreteAg8  <- as.numeric(lognaut_dados_t[15, 8]) * fatGausVlrFrete
podMatrizVlrFreteAg9  <- as.numeric(lognaut_dados_t[15, 9]) * fatGausVlrFrete
podMatrizVlrFreteAg10 <- as.numeric(lognaut_dados_t[15, 10]) * fatGausVlrFrete
podMatrizVlrFreteAg11 <- as.numeric(lognaut_dados_t[15, 11]) * fatGausVlrFrete
podMatrizVlrFreteAg12 <- as.numeric(lognaut_dados_t[15, 12]) * fatGausVlrFrete
podMatrizVlrFreteAg13 <- as.numeric(lognaut_dados_t[15, 13]) * fatGausVlrFrete
podMatrizVlrFreteAg14 <- as.numeric(lognaut_dados_t[15, 14]) * fatGausVlrFrete
podMatrizVlrFreteAg15 <- as.numeric(lognaut_dados_t[15, 15]) * fatGausVlrFrete
podMatrizVlrFreteAg16 <- as.numeric(lognaut_dados_t[15, 16]) * fatGausVlrFrete
podMatrizVlrFreteAg17 <- as.numeric(lognaut_dados_t[15, 17]) * fatGausVlrFrete
podMatrizVlrFreteAg18 <- as.numeric(lognaut_dados_t[15, 18]) * fatGausVlrFrete
podMatrizVlrFreteAg19 <- as.numeric(lognaut_dados_t[15, 19]) * fatGausVlrFrete
podMatrizVlrFreteAg20 <- as.numeric(lognaut_dados_t[15, 20]) * fatGausVlrFrete

podMatrizVlrOrigemAg1  <- as.numeric(lognaut_dados_t[16, 1]) * fatGausVlrOrigem
podMatrizVlrOrigemAg2  <- as.numeric(lognaut_dados_t[16, 2]) * fatGausVlrOrigem
podMatrizVlrOrigemAg3  <- as.numeric(lognaut_dados_t[16, 3]) * fatGausVlrOrigem
podMatrizVlrOrigemAg4  <- as.numeric(lognaut_dados_t[16, 4]) * fatGausVlrOrigem
podMatrizVlrOrigemAg5  <- as.numeric(lognaut_dados_t[16, 5]) * fatGausVlrOrigem
podMatrizVlrOrigemAg6  <- as.numeric(lognaut_dados_t[16, 6]) * fatGausVlrOrigem
podMatrizVlrOrigemAg7  <- as.numeric(lognaut_dados_t[16, 7]) * fatGausVlrOrigem
podMatrizVlrOrigemAg8  <- as.numeric(lognaut_dados_t[16, 8]) * fatGausVlrOrigem
podMatrizVlrOrigemAg9  <- as.numeric(lognaut_dados_t[16, 9]) * fatGausVlrOrigem
podMatrizVlrOrigemAg10 <- as.numeric(lognaut_dados_t[16, 10]) * fatGausVlrOrigem
podMatrizVlrOrigemAg11 <- as.numeric(lognaut_dados_t[16, 11]) * fatGausVlrOrigem
podMatrizVlrOrigemAg12 <- as.numeric(lognaut_dados_t[16, 12]) * fatGausVlrOrigem
podMatrizVlrOrigemAg13 <- as.numeric(lognaut_dados_t[16, 13]) * fatGausVlrOrigem
podMatrizVlrOrigemAg14 <- as.numeric(lognaut_dados_t[16, 14]) * fatGausVlrOrigem
podMatrizVlrOrigemAg15 <- as.numeric(lognaut_dados_t[16, 15]) * fatGausVlrOrigem
podMatrizVlrOrigemAg16 <- as.numeric(lognaut_dados_t[16, 16]) * fatGausVlrOrigem
podMatrizVlrOrigemAg17 <- as.numeric(lognaut_dados_t[16, 17]) * fatGausVlrOrigem
podMatrizVlrOrigemAg18 <- as.numeric(lognaut_dados_t[16, 18]) * fatGausVlrOrigem
podMatrizVlrOrigemAg19 <- as.numeric(lognaut_dados_t[16, 19]) * fatGausVlrOrigem
podMatrizVlrOrigemAg20 <- as.numeric(lognaut_dados_t[16, 20]) * fatGausVlrOrigem

podMatrizVlrDestinoAg1  <- as.numeric(lognaut_dados_t[17, 1]) * fatGausVlrDestino
podMatrizVlrDestinoAg2  <- as.numeric(lognaut_dados_t[17, 2]) * fatGausVlrDestino
podMatrizVlrDestinoAg3  <- as.numeric(lognaut_dados_t[17, 3]) * fatGausVlrDestino
podMatrizVlrDestinoAg4  <- as.numeric(lognaut_dados_t[17, 4]) * fatGausVlrDestino
podMatrizVlrDestinoAg5  <- as.numeric(lognaut_dados_t[17, 5]) * fatGausVlrDestino
podMatrizVlrDestinoAg6  <- as.numeric(lognaut_dados_t[17, 6]) * fatGausVlrDestino
podMatrizVlrDestinoAg7  <- as.numeric(lognaut_dados_t[17, 7]) * fatGausVlrDestino
podMatrizVlrDestinoAg8  <- as.numeric(lognaut_dados_t[17, 8]) * fatGausVlrDestino
podMatrizVlrDestinoAg9  <- as.numeric(lognaut_dados_t[17, 9]) * fatGausVlrDestino
podMatrizVlrDestinoAg10 <- as.numeric(lognaut_dados_t[17, 10]) * fatGausVlrDestino
podMatrizVlrDestinoAg11 <- as.numeric(lognaut_dados_t[17, 11]) * fatGausVlrDestino
podMatrizVlrDestinoAg12 <- as.numeric(lognaut_dados_t[17, 12]) * fatGausVlrDestino
podMatrizVlrDestinoAg13 <- as.numeric(lognaut_dados_t[17, 13]) * fatGausVlrDestino
podMatrizVlrDestinoAg14 <- as.numeric(lognaut_dados_t[17, 14]) * fatGausVlrDestino
podMatrizVlrDestinoAg15 <- as.numeric(lognaut_dados_t[17, 15]) * fatGausVlrDestino
podMatrizVlrDestinoAg16 <- as.numeric(lognaut_dados_t[17, 16]) * fatGausVlrDestino
podMatrizVlrDestinoAg17 <- as.numeric(lognaut_dados_t[17, 17]) * fatGausVlrDestino
podMatrizVlrDestinoAg18 <- as.numeric(lognaut_dados_t[17, 18]) * fatGausVlrDestino
podMatrizVlrDestinoAg19 <- as.numeric(lognaut_dados_t[17, 19]) * fatGausVlrDestino
podMatrizVlrDestinoAg20 <- as.numeric(lognaut_dados_t[17, 20]) * fatGausVlrDestino

podMatrizVlrVlrTotalAg1  <- as.numeric(lognaut_dados_t[18, 1]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg2  <- as.numeric(lognaut_dados_t[18, 2]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg3  <- as.numeric(lognaut_dados_t[18, 3]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg4  <- as.numeric(lognaut_dados_t[18, 4]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg5  <- as.numeric(lognaut_dados_t[18, 5]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg6  <- as.numeric(lognaut_dados_t[18, 6]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg7  <- as.numeric(lognaut_dados_t[18, 7]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg8  <- as.numeric(lognaut_dados_t[18, 8]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg9  <- as.numeric(lognaut_dados_t[18, 9]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg10 <- as.numeric(lognaut_dados_t[18, 10]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg11 <- as.numeric(lognaut_dados_t[18, 11]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg12 <- as.numeric(lognaut_dados_t[18, 12]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg13 <- as.numeric(lognaut_dados_t[18, 13]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg14 <- as.numeric(lognaut_dados_t[18, 14]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg15 <- as.numeric(lognaut_dados_t[18, 15]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg16 <- as.numeric(lognaut_dados_t[18, 16]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg17 <- as.numeric(lognaut_dados_t[18, 17]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg18 <- as.numeric(lognaut_dados_t[18, 18]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg19 <- as.numeric(lognaut_dados_t[18, 19]) * fatGausVlrTotal
podMatrizVlrVlrTotalAg20 <- as.numeric(lognaut_dados_t[18, 20]) * fatGausVlrTotal

##################################################################################
#Etapa 6 - Obtenção do Ranking
##################################################################################
RankAg1 <- podMatrizQtdeEstadiaOrigemAg1 + 
           podMatrizQtdeEstadiaDestinoAg1 +
           podMatrizQtdeRotasAg1 + 
           podMatrizQtdeDiasAg1 + 
           podMatrizVlrFreteAg1 + 
           podMatrizVlrOrigemAg1 +
           podMatrizVlrDestinoAg1 +
           podMatrizVlrVlrTotalAg1
  
RankAg2 <- podMatrizQtdeEstadiaOrigemAg2 + 
           podMatrizQtdeEstadiaDestinoAg2 +
           podMatrizQtdeRotasAg2 + 
           podMatrizQtdeDiasAg2 + 
           podMatrizVlrFreteAg2 + 
           podMatrizVlrOrigemAg2 +
           podMatrizVlrDestinoAg2 +
           podMatrizVlrVlrTotalAg2

RankAg3 <- podMatrizQtdeEstadiaOrigemAg3 + 
           podMatrizQtdeEstadiaDestinoAg3 +
           podMatrizQtdeRotasAg3 + 
           podMatrizQtdeDiasAg3 + 
           podMatrizVlrFreteAg3 + 
           podMatrizVlrOrigemAg3 +
           podMatrizVlrDestinoAg3 +
           podMatrizVlrVlrTotalAg3

RankAg4 <- podMatrizQtdeEstadiaOrigemAg4 + 
           podMatrizQtdeEstadiaDestinoAg4 +
           podMatrizQtdeRotasAg4 + 
           podMatrizQtdeDiasAg4 + 
           podMatrizVlrFreteAg4 + 
           podMatrizVlrOrigemAg4 +
           podMatrizVlrDestinoAg4 +
           podMatrizVlrVlrTotalAg4

RankAg5 <- podMatrizQtdeEstadiaOrigemAg5 + 
           podMatrizQtdeEstadiaDestinoAg5 +
           podMatrizQtdeRotasAg5 + 
           podMatrizQtdeDiasAg5 + 
           podMatrizVlrFreteAg5 + 
           podMatrizVlrOrigemAg5 +
           podMatrizVlrDestinoAg5 +
           podMatrizVlrVlrTotalAg5

RankAg6 <- podMatrizQtdeEstadiaOrigemAg6 + 
           podMatrizQtdeEstadiaDestinoAg6 +
           podMatrizQtdeRotasAg6 + 
           podMatrizQtdeDiasAg6 + 
           podMatrizVlrFreteAg6 + 
           podMatrizVlrOrigemAg6 +
           podMatrizVlrDestinoAg6 +
           podMatrizVlrVlrTotalAg6

RankAg7 <- podMatrizQtdeEstadiaOrigemAg7 + 
           podMatrizQtdeEstadiaDestinoAg7 +
           podMatrizQtdeRotasAg7 + 
           podMatrizQtdeDiasAg7 + 
           podMatrizVlrFreteAg7 + 
           podMatrizVlrOrigemAg7 +
           podMatrizVlrDestinoAg7 +
           podMatrizVlrVlrTotalAg7

RankAg8 <- podMatrizQtdeEstadiaOrigemAg8 + 
           podMatrizQtdeEstadiaDestinoAg8 +
           podMatrizQtdeRotasAg8 + 
           podMatrizQtdeDiasAg8 + 
           podMatrizVlrFreteAg8 + 
           podMatrizVlrOrigemAg8 +
           podMatrizVlrDestinoAg8 +
           podMatrizVlrVlrTotalAg8

RankAg9 <- podMatrizQtdeEstadiaOrigemAg9 + 
           podMatrizQtdeEstadiaDestinoAg9 +
           podMatrizQtdeRotasAg9 + 
           podMatrizQtdeDiasAg9 + 
           podMatrizVlrFreteAg9 + 
           podMatrizVlrOrigemAg9 +
           podMatrizVlrDestinoAg9 +
           podMatrizVlrVlrTotalAg9

RankAg10 <- podMatrizQtdeEstadiaOrigemAg10 + 
            podMatrizQtdeEstadiaDestinoAg10 +
            podMatrizQtdeRotasAg10 + 
            podMatrizQtdeDiasAg10 + 
            podMatrizVlrFreteAg10 + 
            podMatrizVlrOrigemAg10 +
            podMatrizVlrDestinoAg10 +
            podMatrizVlrVlrTotalAg10

RankAg11 <- podMatrizQtdeEstadiaOrigemAg11 + 
            podMatrizQtdeEstadiaDestinoAg11 +
            podMatrizQtdeRotasAg11 + 
            podMatrizQtdeDiasAg11 + 
            podMatrizVlrFreteAg11 + 
            podMatrizVlrOrigemAg11 +
            podMatrizVlrDestinoAg11 +
            podMatrizVlrVlrTotalAg11

RankAg12 <- podMatrizQtdeEstadiaOrigemAg12 + 
            podMatrizQtdeEstadiaDestinoAg12 +
            podMatrizQtdeRotasAg12 + 
            podMatrizQtdeDiasAg12 + 
            podMatrizVlrFreteAg12 + 
            podMatrizVlrOrigemAg12 +
            podMatrizVlrDestinoAg12 +
            podMatrizVlrVlrTotalAg12

RankAg13 <- podMatrizQtdeEstadiaOrigemAg13 + 
            podMatrizQtdeEstadiaDestinoAg13 +
            podMatrizQtdeRotasAg13 + 
            podMatrizQtdeDiasAg13 + 
            podMatrizVlrFreteAg13 + 
            podMatrizVlrOrigemAg13 +
            podMatrizVlrDestinoAg13 +
            podMatrizVlrVlrTotalAg13

RankAg14 <- podMatrizQtdeEstadiaOrigemAg14 + 
            podMatrizQtdeEstadiaDestinoAg14 +
            podMatrizQtdeRotasAg14 + 
            podMatrizQtdeDiasAg14 + 
            podMatrizVlrFreteAg14 + 
            podMatrizVlrOrigemAg14 +
            podMatrizVlrDestinoAg14 +
            podMatrizVlrVlrTotalAg14

RankAg15 <- podMatrizQtdeEstadiaOrigemAg15 + 
            podMatrizQtdeEstadiaDestinoAg15 +
            podMatrizQtdeRotasAg15 + 
            podMatrizQtdeDiasAg15 + 
            podMatrizVlrFreteAg15 + 
            podMatrizVlrOrigemAg15 +
            podMatrizVlrDestinoAg15 +
            podMatrizVlrVlrTotalAg15

RankAg16 <- podMatrizQtdeEstadiaOrigemAg16 + 
            podMatrizQtdeEstadiaDestinoAg16 +
            podMatrizQtdeRotasAg16 + 
            podMatrizQtdeDiasAg16 + 
            podMatrizVlrFreteAg16 + 
            podMatrizVlrOrigemAg16 +
            podMatrizVlrDestinoAg16 +
            podMatrizVlrVlrTotalAg16

RankAg17 <- podMatrizQtdeEstadiaOrigemAg17 + 
            podMatrizQtdeEstadiaDestinoAg17 +
            podMatrizQtdeRotasAg17 + 
            podMatrizQtdeDiasAg17 + 
            podMatrizVlrFreteAg17 + 
            podMatrizVlrOrigemAg17 +
            podMatrizVlrDestinoAg17 +
            podMatrizVlrVlrTotalAg17

RankAg18 <- podMatrizQtdeEstadiaOrigemAg18 + 
            podMatrizQtdeEstadiaDestinoAg18 +
            podMatrizQtdeRotasAg18 + 
            podMatrizQtdeDiasAg18 + 
            podMatrizVlrFreteAg18 + 
            podMatrizVlrOrigemAg18 +
            podMatrizVlrDestinoAg18 +
            podMatrizVlrVlrTotalAg18

RankAg19 <- podMatrizQtdeEstadiaOrigemAg19 + 
            podMatrizQtdeEstadiaDestinoAg19 +
            podMatrizQtdeRotasAg19 + 
            podMatrizQtdeDiasAg19 + 
            podMatrizVlrFreteAg19 + 
            podMatrizVlrOrigemAg19 +
            podMatrizVlrDestinoAg19 +
            podMatrizVlrVlrTotalAg19

RankAg20 <- podMatrizQtdeEstadiaOrigemAg20 + 
            podMatrizQtdeEstadiaDestinoAg20 +
            podMatrizQtdeRotasAg20 + 
            podMatrizQtdeDiasAg20 + 
            podMatrizVlrFreteAg20 + 
            podMatrizVlrOrigemAg20 +
            podMatrizVlrDestinoAg20 +
            podMatrizVlrVlrTotalAg20

#Adicionando Raking na Ultima linha
temp = list(V1=as.character(RankAg1), 
            V2=as.character(RankAg2), 
            V3=as.character(RankAg3), 
            V4=as.character(RankAg4), 
            V5=as.character(RankAg5), 
            V6=as.character(RankAg6), 
            V7=as.character(RankAg7), 
            V8=as.character(RankAg8), 
            V9=as.character(RankAg9), 
            V10=as.character(RankAg10), 
            V11=as.character(RankAg11), 
            V12=as.character(RankAg12), 
            V13=as.character(RankAg13), 
            V14=as.character(RankAg14), 
            V15=as.character(RankAg15), 
            V16=as.character(RankAg16), 
            V17=as.character(RankAg17), 
            V18=as.character(RankAg18), 
            V19=as.character(RankAg19), 
            V20=as.character(RankAg20))

temp = rbind(lognaut_dados_t, temp)

#Criando estrutura de apresentação
alternativas <- c(as.character(temp[2,1]), 
                  as.character(temp[2,2]), 
                  as.character(temp[2,3]),
                  as.character(temp[2,4]),
                  as.character(temp[2,5]),
                  as.character(temp[2,6]),
                  as.character(temp[2,7]),
                  as.character(temp[2,8]),
                  as.character(temp[2,9]),
                  as.character(temp[2,10]),
                  as.character(temp[2,11]),
                  as.character(temp[2,12]),
                  as.character(temp[2,13]),
                  as.character(temp[2,14]),
                  as.character(temp[2,15]),
                  as.character(temp[2,16]),
                  as.character(temp[2,17]),
                  as.character(temp[2,18]),
                  as.character(temp[2,19]),
                  as.character(temp[2,20]))

criterios  <- c(as.numeric(temp[19,1]),  
                as.numeric(temp[19,2]),  
                as.numeric(temp[19,3]),
                as.numeric(temp[19,4]),
                as.numeric(temp[19,5]),
                as.numeric(temp[19,6]),
                as.numeric(temp[19,7]),
                as.numeric(temp[19,8]),
                as.numeric(temp[19,9]),
                as.numeric(temp[19,10]),
                as.numeric(temp[19,11]),
                as.numeric(temp[19,12]),
                as.numeric(temp[19,13]),
                as.numeric(temp[19,14]),
                as.numeric(temp[19,15]),
                as.numeric(temp[19,16]),
                as.numeric(temp[19,17]),
                as.numeric(temp[19,18]),
                as.numeric(temp[19,19]),
                as.numeric(temp[19,20]))

df         <- data.frame(alternativas, criterios)

#Ordenando o Ranking
ranking <- print(df[order(df$crit, decreasing = TRUE), ])

