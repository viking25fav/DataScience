# Definição do Problema de Negócio
# Ler instruções anteriores

# Instalação dos pacotes para o projeto
# Instalar somente uma vez
# Amelia possui funções para tratar valores ausentes 
install.packages("Amelia")
# Caret permite construir modelos de ML para pré processar dados
install.packages("caret")
# Ggplot2 pacote para construção de dados
install.packages("ggplot2")
# Pacote para tratar e manipular dados
install.packages("dplyr")
# Reshape pacote para modificar o formato dos dados
install.packages("reshape")
# RandomForrest e E1071 pacotes para trabalhar com ML
install.packages("randomForest")
install.packages("e1071")

# Carregar os pacotes
# Carregar todas as vezes que abrir o script
library(Amelia)
library(caret)
library(ggplot2)
library(dplyr)
library(reshape)
library(randomForest)
library(e1071)

# Carregar os dados
dados_clientes <- read.csv("dataset.csv")

# Visualizar os dados e sua composição
View(dados_clientes)

# Visualizar as dimensões dos dados (30000 linhas e 25 colunas)
# Sendo 1 coluna de ID, 23 colunas com variáveis preditoras e 1 com variável target
dim(dados_clientes)

# Verificar como as variáveis foram carregadas
str(dados_clientes)

# Verificar informações estatísticas
summary(dados_clientes)


# ANÁLISE EXPLORATÓRIA / LIMPEZA / TRANSFORMAÇÃO

# Removendo a primeira coluna "ID" (pois não é um atributo)
dados_clientes$ID <- NULL
dim(dados_clientes)
View(dados_clientes)

# Verificar os nomes das colunas
colnames(dados_clientes)
# Renomear a última coluna (variável target)
colnames(dados_clientes)[24] <- "inadimplente"
colnames(dados_clientes)
View(dados_clientes)

# Verificar valores ausentes e remover do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Mapa de Valores Missing VS Observed")

# Caso existam valores ausentes
dados_clientes <- na.omit(dados_clientes)

# Renomear colunas categóricas
colnames(dados_clientes)[2] <- "Genero"
colnames(dados_clientes)[3] <- "Escolaridade"
colnames(dados_clientes)[4] <- "Estado_Civil"
colnames(dados_clientes)[5] <- "Idade"
colnames(dados_clientes)

# Alterar o conteúdo da coluna "Genero"
View(dados_clientes$Genero)
str(dados_clientes$Genero)
# Consultar a documentação do comando "cut"
# Usamos a função "cut" pois além do tipo da variável, modificamos o seu conteúdo também
?cut
# Alterar o conteúdo
dados_clientes$Genero <- cut(dados_clientes$Genero, c(0,1,2), labels = c("Masculino", "Feminino"))
View(dados_clientes$Genero)
str(dados_clientes$Genero)
summary(dados_clientes$Genero)

# Alterar o conteúdo da coluna "Escolaridade"
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
# Alterar o conteúdo
dados_clientes$Escolaridade <- cut(dados_clientes$Escolaridade, c(0,1,2,3,4), labels = c("Pos Graduado", "Graduado", "Ensino Medio", "Outros"))
View(dados_clientes$Escolaridade)
str(dados_clientes$Escolaridade)
summary(dados_clientes$Escolaridade)

# Alterar o conteúdo da coluna "Estado Civil"
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
# Alterar o conteúdo (colocamos o "-1" para tratar os valores desconhecidos)
dados_clientes$Estado_Civil <- cut(dados_clientes$Estado_Civil, c(-1,0,1,2,3), labels = c("Desconhecido", "Casado", "Solteiro", "Outros"))
View(dados_clientes$Estado_Civil)
str(dados_clientes$Estado_Civil)
summary(dados_clientes$Estado_Civil)

# Alterar o conteúdo da coluna "Idade" e dividindo em categorias
View(dados_clientes$Idade)
str(dados_clientes$Idade)
# Analisar estatisticamente para verificar se os dados fazem sentido
sumary(dados_clientes$Idade)
hist(dados_clientes$Idade)
# Alterar o conteúdo (colocamos o "-1" para tratar os valores desconhecidos)
dados_clientes$Idade <- cut(dados_clientes$Idade, c(0,30,50,100), labels = c("Jovem", "Adulto", "Idoso"))
View(dados_clientes$Idade)
str(dados_clientes$Idade)
summary(dados_clientes$Idade)

# Visualizar o dataset para analisar as alterações realizadas
View(dados_clientes)

# Converter as variáveis categóricas sobre histórico de pagamento
# As variáveis atuais estão com valores numéricos e precisamos converter para valores categóricos
# Converter para o tipo fator
# Usamos a função "as.factor" pois não vamos realizar mudanças no valor da variável 
dados_clientes$PAY_0 <- as.factor(dados_clientes$PAY_0)
dados_clientes$PAY_2 <- as.factor(dados_clientes$PAY_2)
dados_clientes$PAY_3 <- as.factor(dados_clientes$PAY_3)
dados_clientes$PAY_4 <- as.factor(dados_clientes$PAY_4)
dados_clientes$PAY_5 <- as.factor(dados_clientes$PAY_5)
dados_clientes$PAY_6 <- as.factor(dados_clientes$PAY_6)
# Verificar os tipos das variáveis do dataset
str(dados_clientes)

# Após a transformação dos dados na coluna "Escolaridade", surgiram valor NA (ausentes)
# Verificar valores ausentes e remover do dataset
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Mapa de Valores Missing VS Observed")
# Remover os registros ausentes
dados_clientes <- na.omit(dados_clientes)
# Nova verificação
sapply(dados_clientes, function(x) sum(is.na(x)))
missmap(dados_clientes, main = "Mapa de Valores Missing VS Observed")
dim(dados_clientes)

# Alterar a variável target para o tipo fator
# Como é uma variável alvo, requer um pouco mais de atenção
str(dados_clientes$inadimplente)
colnames(dados_clientes)
dados_clientes$inadimplente <- as.factor(dados_clientes$inadimplente)
str(dados_clientes$inadimplente)
View(dados_clientes)

# Total de inadimplentes versus pagadores (existem mais registros em uma classe do que outra)
table(dados_clientes$inadimplente)

# Verificar as porcentagens entre as classes
prop.table(table(dados_clientes$inadimplente))

# Plot da distribuição usando o ggplot2 (verificar a porcentagem através de gráfico)
qplot(inadimplente, data = dados_clientes, geom = "bar") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Set seed (para começar a divisão dos dados entre treino e teste)
set.seed(12345)

# AMOSTRAGEM ESTRATIFICADA para separação do dataset
# Separação do dataset em dados de treino e teste
# Usaremos a função createDataPartition
# Para verificar a documentação
?createDataPartition
# Iniciando a separação, neste caso, usando a variável alvo e separando em 75%
indice <- createDataPartition(dados_clientes$inadimplente, p = 0.75, list = FALSE)
dim(indice)

# Definir os dados de treinamento como subconjunto do conjunto de dados original
# com números de índice de linha (como identificado acima) e todas as colunas
dados_treino <- dados_clientes[indice,]
dim(dados_treino)
table(dados_treino$inadimplente)

# Verificar porcentagem entre as classes
prop.table(table(dados_treino$inadimplente))

# Verificar número de registros no dataset de treinamentos
dim(data_treino)

# Comparar as porcentagens entre as classes de treinamento e dados originais (função cbind)
compara_dados <- cbind(prop.table(table(dados_treino$inadimplente)), prop.table(table(dados_clientes$inadimplente)))
colnames(compara_dados) <- c("Treinamento", "Original")
compara_dados

# Melt Data - Converter colunas em linhas
?reshape2::melt
melt_compara_dados <- melt(compara_dados)
melt_compara_dados

# Plot para ver a distribuição do treinamento versus original
ggplot(melt_compara_dados, aes(x = X1, y = value)) + geom_bar( aes(fill = X2), stat = "identity", position = "dodge") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Gerar dataset de teste e de treino
# Tudo o que não está no dataset de treinamento, está no dataset de teste
dados_teste <- dados_clientes[-indice,]

# Verificar os datasets 
dim(dados_teste)
dim(dados_treino)


# CONSTRUINDO MODELOS DE MACHINE LEARNING

# Construção da primeira versão do modelo (RANDONFOREST)
# OBS.: OS DADOS NÃO ESTÃO BALANCEADOS
?randomForest
# "inadimplente é o que eu quero prever, o "~" representa a separação entre as variáveis e 
# o "." representa todos os dados que serão utilizados. Ponto porque vamos usar todos
modelo_v1 <- randomForest(inadimplente ~ ., data = dados_treino)
modelo_v1
# Resultado
# Call: randomForest(formula = inadimplente ~ ., data = dados_treino) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# OOB estimate of  error rate: 18.14%
# Confusion matrix:
#  0    1 class.error
# 16334  950  0.05496413
#  3084 1874  0.62202501

# Avaliar a primeira versão do modelo
plot(modelo_v1)
# linha verde = performance do modelo
# linha preta = erro do modelo
# linha vermelha = erro do modelo

# Previsões com dados de testes (FUNÇÃO PREDICT)
previsoes_v1 <- predict(modelo_v1, dados_teste)

# Matriz de Confusão para avaliação do modelo (Verificar Acurácia)
?caret::confusionMatrix
# Sendo "positive - 1" a informação de que o cliente vai realizar o pagamento
cm_v1 <- caret::confusionMatrix(previsoes_v1, dados_teste$inadimplente, positive = "1")
cm_v1
#Prediction    0    1
#              0 5420 1045
#              1  341  607
# Accuracy : 0.813          
# 95% CI : (0.804, 0.8218)
# No Information Rate : 0.7771         
# P-Value [Acc > NIR] : 1.999e-14      
# Kappa : 0.3635         
# Mcnemar's Test P-Value : < 2.2e-16      
#            Sensitivity : 0.36743        
#            Specificity : 0.94081        
#         Pos Pred Value : 0.64030        
#         Neg Pred Value : 0.83836        
#             Prevalence : 0.22285        
#         Detection Rate : 0.08188        
#   Detection Prevalence : 0.12788        
#      Balanced Accuracy : 0.65412        
#       'Positive' Class : 1 

# Calcular Precision, Recall e F1-Score (métricas de avaliação do modelo preditivo)
y <- dados_teste$inadimplente
y_pred_v1 <- previsoes_v1

precision <- posPredValue(y_pred_v1, y)
precision
# [1] 0.8383604

recall <- sensitivity(y_pred_v1, y)
recall
# [1] 0.9408089

F1 <- (2 * precision * recall) / (precision + recall)
F1
# [1] 0.886635

# Buscar melhora da acurácia e métricas
# Primeiro: Realizar balanceamento de classe com o método OVER-SAMPLING pois temos menos registros da classe "1"
# Dessa forma criamos mais registros de inadimplentes para treinar o nosso modelo
install.packages(c("zoo", "xts", "quantmod", "abind", "ROCR"))
install.packages("DMwR_0.4.1.tar.gz", repo=NULL, type = "source")
library(DMwR)
?SMOTE

# Segundo: Aplicar o SMOTE (Synthetic Minority Over-sampling Technique)
table(dados_treino$inadimplente)
prop.table(table(dados_treino$inadimplente))
set.seed(9560)
dados_treino_bal <- SMOTE(inadimplente ~ ., data = dados_treino)
table(dados_treino_bal$inadimplente)
prop.table(table(dados_treino_bal$inadimplente))


# Construção da SEGUNDA versão do modelo (RANDONFOREST + DADOS BALANCEADOS)
# A única diferença para o modelo anterior é que agora estamos usando dados balanceados
modelo_v2 <- randomForest(inadimplente ~ ., data = dados_treino_bal)
modelo_v2
# Call: randomForest(formula = inadimplente ~ ., data = dados_treino_bal) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 4
# OOB estimate of  error rate: 10.05%
# Confusion matrix:
#       0     1   class.error
#  0 19010   822  0.04144816
#  1  2665 12209  0.17917171

# Avaliando o modelo
plot(modelo_v2)

# Previsões com dados de testes (FUNÇÃO PREDICT)
previsoes_v2 <- predict(modelo_v2, dados_teste)

# Matriz de Confusão para avaliação do modelo (Verificar Acurácia)
?caret::confusionMatrix
# Sendo "positive - 1" a informação de que o cliente vai realizar o pagamento
cm_v2 <- caret::confusionMatrix(previsoes_v2, dados_teste$inadimplente, positive = "1")
cm_v2
# Reference
# Prediction    0    1
#            0 5050  810
#            1  711  842
# Accuracy : 0.7948         
# 95% CI : (0.7854, 0.804)
# No Information Rate : 0.7771         
# P-Value [Acc > NIR] : 0.0001189      
# Kappa : 0.3947         
# Mcnemar's Test P-Value : 0.0119770      
#            Sensitivity : 0.5097         
#            Specificity : 0.8766         
#         Pos Pred Value : 0.5422         
#         Neg Pred Value : 0.8618         
#             Prevalence : 0.2229         
#         Detection Rate : 0.1136         
#   Detection Prevalence : 0.2095         
#      Balanced Accuracy : 0.6931         
#       'Positive' Class : 1 

# Calcular Precision, Recall e F1-Score (métricas de avaliação do modelo preditivo)
y <- dados_teste$inadimplente
y_pred_v2 <- previsoes_v2

precision <- posPredValue(y_pred_v2, y)
precision
# [1] 0.8617747

recall <- sensitivity(y_pred_v2, y)
recall
# [1] 0.8765839

F1 <- (2 * precision * recall) / (precision + recall)
F1
# [1] 0.8691163

# Analise e escolha das variáveis preditoras para as previsões
View(dados_treino_bal)
varImpPlot(modelo_v2)

# Obter as variáveis mais importantes
imp_var <- importance(modelo_v2)
varImportance <- data.frame(Variables = row.names(imp_var), Importance = round(imp_var[ ,'MeanDecreaseGini'],2))

# Criando um rank de variáveis baseado na importância
rankImportance <- varImportance %>%
  mutate(Rank = paste0('#', dense_rank(desc(Importance))))

# Utilizando o ggplot2 para visualizar a importância relativa das variáveis
ggplot(rankImportance,
       aes(x = reorder(Variables, Importance),
           y = Importance,
           fill = Importance)) +
  geom_bar(stat='identity') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust = 0,
            vjust = 0.55,
            size = 4,
            colour = 'red') +
  labs(x = 'Variables') +
  coord_flip()

# Construção da TERCEIRA versão do modelo (RANDONFOREST + DADOS BALANCEADOS)
# Exclusão de variáveis, trabalhando apenas com as mais importantes
# Para verificar os nomes das variáveis:
colnames(dados_treino_bal)
# Construindo o modelo:
modelo_v3 <- randomForest(inadimplente ~ PAY_0 + PAY_2 + PAY_3 + PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, data = dados_treino_bal)
modelo_v3
# Call: randomForest(formula = inadimplente ~ PAY_0 + PAY_2 + PAY_3 +      PAY_AMT1 + PAY_AMT2 + PAY_5 + BILL_AMT1, data = dados_treino_bal) 
# Type of random forest: classification
# Number of trees: 500
# No. of variables tried at each split: 2
# OOB estimate of  error rate: 13.55%
# Confusion matrix:
#       0     1    class.error
#  0 18847   985   0.0496672
#  1  3718 11156   0.2499664

# Avaliando o modelo
plot(modelo_v3)

# Previsões com dados de testes (FUNÇÃO PREDICT)
previsoes_v3 <- predict(modelo_v3, dados_teste)

# Matriz de Confusão para avaliação do modelo (Verificar Acurácia)
?caret::confusionMatrix
# Sendo "positive - 1" a informação de que o cliente vai realizar o pagamento
cm_v3 <- caret::confusionMatrix(previsoes_v3, dados_teste$inadimplente, positive = "1")
cm_v3
#             Reference
# Prediction    0    1
#           0 5203  950
#           1  558  702
# Accuracy : 0.7966          
# 95% CI : (0.7872, 0.8057)
# No Information Rate : 0.7771          
# P-Value [Acc > NIR] : 2.61e-05        
# Kappa : 0.3584          
# Mcnemar's Test P-Value : < 2.2e-16       
#            Sensitivity : 0.4249          
#            Specificity : 0.9031          
#         Pos Pred Value : 0.5571          
#         Neg Pred Value : 0.8456          
#             Prevalence : 0.2229          
#         Detection Rate : 0.0947          
#   Detection Prevalence : 0.1700          
#      Balanced Accuracy : 0.6640          
#       'Positive' Class : 1  

# Calcular Precision, Recall e F1-Score (métricas de avaliação do modelo preditivo)
y <- dados_teste$inadimplente
y_pred_v3 <- previsoes_v3

precision <- posPredValue(y_pred_v3, y)
precision
# [1] 0.8456038

recall <- sensitivity(y_pred_v3, y)
recall
# [1] 0.9031418

F1 <- (2 * precision * recall) / (precision + recall)
F1
# [1] 0.8734262

# SALVANDO O MODELO EM DISCO
saveRDS(modelo_v3, file = "modelos/modelo_v3.rds")

# CARREGANDO O MODELO
modelo_final <- readRDS("modelos/modelo_v3.rds")

# Testando o modelo de ML com novos dados
# Modelo foi construido co 7 variáveis preditoras
# Dessa forma, para testar o modelo, devemos realizar o input de 7 variáveis por registro
# Abaixo os dados de novos clientes:
PAY_0 <- c(0, 0, 0)
PAY_2 <- c(0, 0, 0)
PAY_3 <- c(1, 0, 0)
PAY_AMT1 <- c(1100, 1000, 1200)
PAY_AMT2 <- c(1500, 1300, 1150)
PAY_5 <- c(0, 0, 0)
BILL_AMT1 <- c(350, 420, 280)

# Checando os tipos de dados (para verificar se estão no mesmo formato)
str(dados_treino_bal)
str(novos_clientes)

# Convertendo as variáveis para o tipo fator (igualando ao modelo)
novos_clientes$PAY_0 <- factor(novos_clientes$PAY_0, levels = levels(dados_treino_bal$PAY_0))
novos_clientes$PAY_2 <- factor(novos_clientes$PAY_2, levels = levels(dados_treino_bal$PAY_2))
novos_clientes$PAY_3 <- factor(novos_clientes$PAY_3, levels = levels(dados_treino_bal$PAY_3))
novos_clientes$PAY_5 <- factor(novos_clientes$PAY_5, levels = levels(dados_treino_bal$PAY_5))

# Checando os dados convertidos
str(novos_clientes)

# Agrupar em um dataframa
novos_clientes <- data.frame(PAY_0, PAY_2, PAY_3, PAY_AMT1, PAY_AMT2, PAY_5, BILL_AMT1)
View(novos_clientes)

# Previsões
previsoes_novos_clientes <- predict(modelo_final, novos_clientes)
View(previsoes_novos_clientes)







