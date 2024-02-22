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
lognaut_fretes = read.csv('D:\\USP\\TCC\\TCC-USP\\lognaut_fretesDEMO.csv',sep=';',dec = ",")
lognaut_dados <- lognaut_fretes 

##################################################################################
#Tirando apresentação cientifica dos dados                 
##################################################################################
format(lognaut_dados, scientific = FALSE)

##################################################################################
#Etapa 1 - Determinação da Matriz de Decisão
##################################################################################
#Etapa 1.1 - Monotônico de Custo (Menor Melhor)
for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_custo <- 1 / lognaut_dados[i:nrow(lognaut_dados), 2]
}

#linhas, coluna
lognaut_dados[1:3, 2]

somaCusto = sum(lognaut_dados$fator_mc_custo)
for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_mc_custo <- lognaut_dados[i:nrow(lognaut_dados), 6:6] / somaCusto
}

##################################################################################
#Etapa 1.2 - Monotônico de Lucro (Maior Melhor)
##################################################################################
soma_camera        <- sum(lognaut_dados$Camera)
soma_armazenamento <- sum(lognaut_dados$Armazenamento)
soma_bateria       <- sum(lognaut_dados$DuracaoBateria)

for(i in 1:nrow(lognaut_dados)){
  lognaut_dados$fator_ml_camera        <- lognaut_dados[i:nrow(lognaut_dados), 3:3] / soma_camera
  lognaut_dados$fator_ml_armazenamento <- lognaut_dados[i:nrow(lognaut_dados), 4:4] / soma_armazenamento
  lognaut_dados$fator_ml_bateria       <- lognaut_dados[i:nrow(lognaut_dados), 5:5] / soma_bateria
}

##################################################################################
#Etapa 2 - Calculo da média das alternativas em cada critério
##################################################################################
# Transposição da Matriz
lognaut_dados_t <- t(lognaut_dados)

##################################################################################
#Calculando as Medias
##################################################################################
mediaCust <- mean(as.numeric(lognaut_dados_t[6:nrow(lognaut_dados_t), 1:3]))
mediaCam  <- mean(as.numeric(lognaut_dados_t[7:nrow(lognaut_dados_t), 1:3]))
mediaArm  <- mean(as.numeric(lognaut_dados_t[8:nrow(lognaut_dados_t), 1:3]))
mediaBat  <- mean(as.numeric(lognaut_dados_t[9:nrow(lognaut_dados_t), 1:3]))

##################################################################################
#Etapa 3 - Cálculo do desvio padrão dos critérios com base na amostra das alternativas
##################################################################################
devPadCust <- sd(as.numeric(lognaut_dados_t[6, 1:3]))
devPadCam  <- sd(as.numeric(lognaut_dados_t[7, 1:3]))
devPadArm  <- sd(as.numeric(lognaut_dados_t[8, 1:3]))
devPadBat  <- sd(as.numeric(lognaut_dados_t[9, 1:3]))

##################################################################################
#Etapa 4 - Cálculo do fator gaussiano para cada critério
##################################################################################
fatGausCusto <- devPadCust / mediaCust
fatGausCam   <- devPadCam  / mediaCam
fatGausArm   <- devPadArm  / mediaArm
fatGausBat   <- devPadBat  / mediaBat

##################################################################################
#Etapa 4.1 - Cálculo do fator gaussiano normalizado para cada critério
##################################################################################
somaFatores      <- fatGausCusto + fatGausCam + fatGausArm + fatGausBat

fatGausNormCusto <- fatGausCusto / somaFatores
fatGausNormCam   <- fatGausCam   / somaFatores
fatGausNormArm   <- fatGausArm   / somaFatores
fatGausNormBat   <- fatGausBat   / somaFatores

##################################################################################
#Etapa 5 - Ponderação da Matriz de Decisão
##################################################################################
podMatrizCustoX <- as.numeric(lognaut_dados_t[6, 1]) * fatGausNormCusto
podMatrizCustoS <- as.numeric(lognaut_dados_t[6, 2]) * fatGausNormCusto
podMatrizCustoI <- as.numeric(lognaut_dados_t[6, 3]) * fatGausNormCusto

podMatrizCamX   <- as.numeric(lognaut_dados_t[7, 1]) * fatGausNormCam
podMatrizCamS   <- as.numeric(lognaut_dados_t[7, 2]) * fatGausNormCam
podMatrizCamI   <- as.numeric(lognaut_dados_t[7, 3]) * fatGausNormCam

podMatrizArmX   <- as.numeric(lognaut_dados_t[8, 1]) * fatGausNormArm
podMatrizArmS   <- as.numeric(lognaut_dados_t[8, 2]) * fatGausNormArm
podMatrizArmI   <- as.numeric(lognaut_dados_t[8, 3]) * fatGausNormArm

podMatrizBatX   <- as.numeric(lognaut_dados_t[9, 1]) * fatGausNormBat
podMatrizBatS   <- as.numeric(lognaut_dados_t[9, 2]) * fatGausNormBat
podMatrizBatI   <- as.numeric(lognaut_dados_t[9, 3]) * fatGausNormBat

##################################################################################
#Etapa 6 - Obtenção do Ranking
##################################################################################
RanX <- podMatrizCustoX + podMatrizCamX + podMatrizArmX + podMatrizBatX
RanS <- podMatrizCustoS + podMatrizCamS + podMatrizArmS + podMatrizBatS
RanI <- podMatrizCustoI + podMatrizCamI + podMatrizArmI + podMatrizBatI

#Adicionando Raking na Ultima linha
temp = list(V1= as.character(RanX), V2=as.character(RanS), V3=as.character(RanI))
temp = rbind(lognaut_dados_t, temp)

#Criando estrutura de apresentação
alter <- c(as.character(temp[1,1]), as.character(temp[1,2]), as.character(temp[1,3]))
crit  <- c(as.numeric(temp[10,1]),  as.numeric(temp[10,2]),  as.numeric(temp[10,3]))
df    <- data.frame(alter, crit)

#Ordenando o Ranlinh
ranking <- print(df[order(df$crit, decreasing = TRUE), ])

