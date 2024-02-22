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

vlrFrete <- c(lognaut_dados$vlrFrete)
df         <- data.frame(alternativas, vlrFrete)

criterio        <- c("Frete")
devPadVlrFrete  <- c(devPadVlrFrete)          
mediaVlrFrete   <- c(mediaVlrFrete)

df         <- data.frame(criterio, devPadVlrFrete, mediaVlrFrete)


vlrMonFrete <- c(lognaut_dados$fator_mc_vlrFrete)
df         <- data.frame(alternativas, vlrMonFrete)

A <- (0.11871873 + 
     0.12138158 + 
     0.05503318 +
     0.12229594 + 
     0.08293633 + 
     0.11871873 + 
     0.12138158 +
     0.05407090 +
     0.12252669 + 
     0.08293633) / 10

df         <- data.frame(c("Media do Frete"), A)

agentes <- c("VlrFrete(Agente 1)", 
              "VlrFrete(Agente 2)",
              "VlrFrete(Agente 3)",                                           
              "VlrFrete(Agente 4)",                                             
              "VlrFrete(Agente 5)",                                                
              "VlrFrete(Agente 6)",                                               
              "VlrFrete(Agente 7)",                                               
              "VlrFrete(Agente 8)",                                               
              "VlrFrete(Agente 9)",                                               
              "VlrFrete(Agente 10)")

fatoresFrete <- c(lognaut_dados_t[13, 1],
             lognaut_dados_t[13, 2],
             lognaut_dados_t[13, 3],
             lognaut_dados_t[13, 4],
             lognaut_dados_t[13, 5],
             lognaut_dados_t[13, 6],
             lognaut_dados_t[13, 7],
             lognaut_dados_t[13, 8],
             lognaut_dados_t[13, 9],
             lognaut_dados_t[13, 10])

df         <- data.frame(agentes, fatoresFrete)


criterioFrete <- c("Frete")
fatorGaussiano <- c(fatGausVlrFrete)
df         <- data.frame(criterioFrete, fatorGaussiano)


devPadVlrFrete


0.11871873 * 0.2854533


devPadQtdeEstadiaOrigem  <- sd(as.numeric(lognaut_dados_t[09, 1:ncol(lognaut_dados_t)]))
devPadQtdeEstadiaDestino <- sd(as.numeric(lognaut_dados_t[10, 1:ncol(lognaut_dados_t)]))
devPadQtdeRotas          <- sd(as.numeric(lognaut_dados_t[11, 1:ncol(lognaut_dados_t)]))
devPadQtdeDias           <- sd(as.numeric(lognaut_dados_t[12, 1:ncol(lognaut_dados_t)]))
devPadVlrFrete           <- sd(as.numeric(lognaut_dados_t[13, 1:ncol(lognaut_dados_t)]))
devPadVlrOrigem          <- sd(as.numeric(lognaut_dados_t[14, 1:ncol(lognaut_dados_t)]))
devPadVlrDestino 

fatGausVlrFrete           <- devPadVlrFrete           / mediaVlrFrete 

criterios <- c("mediaQtdeEstadiaOrigem",  
               "mediaQtdeEstadiaDestino",
               "mediaQtdeRotas",
               "mediaQtdeDias",
               "mediaVlrFrete",
               "mediaVlrOrigem",
               "mediaVlrDestino")

df         <- data.frame(criterios, desvioPadrao)


desvioPadrao <- c(devPadQtdeEstadiaOrigem,  
                  devPadQtdeEstadiaDestino,
                  devPadQtdeRotas,
                  devPadQtdeDias,
                  devPadVlrFrete,
                  devPadVlrOrigem,
                  devPadVlrDestino)

df         <- data.frame(criterios, desvioPadrao)




fatorGaussiaNorm <- c(fatGausNormQtdeEstadiaOrigem,
                    fatGausNormQtdeEstadiaDestino,
                    fatGausNormQtdeRotas,
                    fatGausNormQtdeDias,
                    fatGausNormVlrFrete,
                    fatGausNormVlrOrigem,
                    fatGausNormVlrDestino)      
df         <- data.frame(criterios, fatorGaussiaNorm)
DT::datatable(fatorGaussiaNorm, rownames = TRUE)

criterio <- "Vlr Frete"
df         <- data.frame(criterio, fatGausNormVlrFrete)


criterios <- c("mediaQtdeEstadiaOrigem",  
               "mediaQtdeEstadiaDestino",
               "mediaQtdeRotas",
               "mediaQtdeDias",
               "mediaVlrFrete",
               "mediaVlrOrigem",
               "mediaVlrDestino")


medias <- c(mediaQtdeEstadiaOrigem,
           mediaQtdeEstadiaDestino,
           mediaQtdeRotas,
           mediaQtdeDias,
           mediaVlrFrete,
           mediaVlrOrigem,
           mediaVlrDestino)

df         <- data.frame(criterios, medias)
DT::datatable(df, rownames = TRUE)

novos_nomes <- c("agente",                                   
                 "qtdeEstadiaOrigem", 
                 "qtdeEstadiaDestino",
                 "qtdeRotas",                                           
                 "qtdeDias",                                             
                 "vlrFrete",                                                
                 "vlrOrigem",                                               
                 "vlrDestino") 

names(lognaut_fretes) <- novos_nomes

install.packages("DT")  #caso ainda nao tenha o pacote instalado
DT::datatable(lognaut_dados[13:13], rownames = TRUE)


fi = fatGausQtdeEstadiaOrigem +  
fatGausQtdeEstadiaDestino + 
fatGausQtdeRotas +
fatGausQtdeDias +
fatGausVlrFrete +
fatGausVlrOrigem +
fatGausVlrDestino

2.603004 / fatGausVlrFrete

df$podMatrizQtdeEstadiaOrigem <- NULL
df$podMatrizQtdeDestinoDestino <- NULL
df$podMatrizQtdeQtdeRotas <- NULL
df$podMatrizQtdeQtdeDias <- NULL
df$podMatrizVlrOrigem <- NULL
df$podMatrizVlrDestino <- NULL


DT::datatable(df, rownames = TRUE)
