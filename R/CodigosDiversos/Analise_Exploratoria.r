####### Análise exploratória de dados com R #######

# Remove objetos da memória do R
rm(list=ls(all=TRUE)) 

# Cria o data frame contendo o historico de vendas do cafe
dados <- data.frame(Vendas_Cafe = c(18, 20, 23, 23, 23, 23, 24, 25, 26, 26, 26, 26, 27, 28, 28, 
                                    29, 29, 30, 30, 31, 31, 33, 34, 35, 38, 39, 41, 44, 44, 46), 
                    Preco_Cafe = c(4.77, 4.67, 4.75, 4.74, 4.63, 4.56, 4.59, 4.75, 4.75, 4.49, 
                                    4.41, 4.32, 4.68, 4.66, 4.42, 4.71, 4.66, 4.46, 4.36, 4.47, 4.43, 
                                    4.4, 4.61, 4.09, 3.73, 3.89, 4.35, 3.84, 3.81, 3.79), 
                    Promocao = c("Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Sim", 
                                 "Nao", "Sim", "Nao", "Nao", "Sim", "Sim", "Nao", "Sim", "Sim",
                                 "Sim", "Nao", "Nao", "Sim", "Sim", "Sim", "Nao", "Sim", "Sim",
                                 "Sim", "Sim", "Sim"),
                    Preco_Leite = c(4.74, 4.81, 4.36, 4.29, 4.17, 4.66, 4.73, 4.11, 4.21, 4.25, 
                                    4.62, 4.53, 4.44, 4.19, 4.37, 4.29, 4.57, 4.21, 4.77, 4, 4.31,
                                    4.34, 4.05, 4.73, 4.07, 4.75, 4, 4.15, 4.34, 4.15))
                                    
# Visualizar a media (mean) e outras estatisticas descritivas das variáveis
summary(dados)

# Visualizar a media (mean) e outras estatisticas descritivas apenas de uma variável
summary(dados$Vendas_Cafe)

# Visualizar desvio padrão (standard deviation) das variáveis
sd(dados$Vendas_Cafe)
sd(dados$Preco_Cafe)
sd(dados$Preco_Leite)

# Visualizar através de um histograma a distribuição da variável Preco_Cafe 
hist(dados$Preco_Cafe)

# Customizando o histograma
hist(dados$Preco_Cafe, 
    col = 'blue',
    main = 'Distribuição dos Preços Praticados para o Café')

# Visualizar o histograma das três variáveis numéricas na mesma pagina
par(mfrow=c(2,2)) #Configura o layout para posicionar os gráficos em duas linhas e duas colunas
hist(dados$Vendas_Cafe, 
    col = 'blue',
    main = 'Distribuição das Vendas do Café') 
hist(dados$Preco_Cafe,
    col = 'blue',
    main = 'Distribuição dos Preços do Café') 
hist(dados$Preco_Leite,
    col = 'blue',
    main = 'Distribuição dos Preços do Leite')

# Limpa os gráficos e volta o layout para configuração padrão 
dev.off() 

# Visualizar relação entre as vendas do café o preço do café
plot(y = dados$Vendas_Cafe, x = dados$Preco_Cafe)

# Customizar o gráfico que foi gerado acima
plot(y = dados$Vendas_Cafe, x = dados$Preco_Cafe, 
     pch = 16,
     col = 'blue',
     xlab = 'Preço',
     ylab = 'Quantidade Vendidade',
     main = 'Relação entre o Preço e as Vendas do Café')
     grid() #este comando adiciona linhas de grade ao grafico 

# Colorir os pontos em que havia promoção naquele dia 
plot(y = dados$Vendas_Cafe, x = dados$Preco_Cafe, 
     col = factor(dados$Promocao), 
     pch = 16,
     xlab = 'Preço',
     ylab = 'Quantidade Vendida',
     main = 'Relação entre o Preço e as Vendas do Café')
    #esse comando adiciona legenda
    legend(x=4.4,y=45, 
           c("Promoção","Sem_Promoção"), 
           col=c("red","black"),
           pch=c(16,16)) 
    grid()

# Criar uma nova variável informando se naquele dia vendeu acima ou abaixo da média histórica
media <- mean(dados$Vendas_Cafe) #armazena a média em uma variável 
variavel <- ifelse(dados$Vendas_Cafe > media, 'Acima_da_media', 'Abaixo_da_media')
variavel <- factor(variavel) #converte nova variável para factor

# Gráfico com a quantidade abaixo e acima da média
plot(variavel) 

# Visualizar a quantidade abaixo e acima da média
table(variavel) 

# Gerar boxplot das vendas
boxplot(dados$Vendas_Cafe)

# Gerar boxplot do preco
boxplot(dados$Preco_Cafe)

# Gerar boxplot comparativo das vendas quando houve promoção e de quando não houve
boxplot(dados$Vendas_Cafe~dados$Promocao)

# Customizando o boxplot
boxplot(dados$Vendas_Cafe~dados$Promocao, 
    col = 'gray', 
    pch = 16,
    xlab = 'Promoção',
    ylab = 'Vendas',
    main = 'Vendas com promoção vs Vendas sem promoção')



