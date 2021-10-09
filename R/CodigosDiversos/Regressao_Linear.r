####### Regressão Linear #######


# Limpa memória do R
rm(list = ls()) 
library(ggplot2) # Biblioteca pra gerar visualizações mais sofisticadas
library(plotly)  # Biblioteca pra gerar visualizações mais sofisticadas

# Cria o data frame
dados <- data.frame(Vendas_Cafe = c(18, 20, 23, 23, 23, 23, 24, 25, 26, 26, 26, 26, 27, 28, 28,
									29, 29, 30, 30, 31, 31, 33, 34, 35, 38, 39, 41, 44, 44, 46), 
					Preco_Cafe = c(4.77, 4.67, 4.75, 4.74, 4.63, 4.56, 4.59, 4.75, 4.75, 4.49,
								   4.41, 4.32, 4.68, 4.66, 4.42, 4.71, 4.66, 4.46, 4.36, 4.47, 4.43,
								   4.4, 4.61, 4.09, 3.73, 3.89, 4.35, 3.84, 3.81, 3.79),
					Promocao = c("Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Nao", "Sim",
								 "Nao", "Sim", "Nao", "Nao", "Sim", "Sim", "Nao", "Sim", 
								 "Sim", "Sim", "Nao", "Nao", "Sim", "Sim", "Sim", "Nao", 
								 "Sim", "Sim", "Sim", "Sim", "Sim"),
					Preco_Leite = c(4.74, 4.81, 4.36, 4.29, 4.17, 4.66, 4.73, 4.11, 4.21, 4.25, 
									4.62, 4.53, 4.44, 4.19, 4.37, 4.29, 4.57, 4.21, 4.77, 4, 4.31, 
									4.34, 4.05, 4.73, 4.07, 4.75, 4, 4.15, 4.34, 4.15))
View(dados)

# Explorando os dados
# Relação entre preço do café e suas vendas 
plot(y = dados$Vendas_Cafe, x = dados$Preco_Cafe, main = 'Relação entre Vendas do Café VS Preço do Café', xlab = 'Preço do Café', ylab = 'Qtde Vendida do Café', pch = 16)
grid()

# É possível gerar um gráfico mais sofisticado utilizando a biblioteca ggplot
g1 <- ggplot(data = dados, aes(y = Vendas_Cafe, x = Preco_Cafe)) + geom_point()

# Podemos adicionar uma reta de regressão com o argumento geom_smooth (adicionar intervalo de confiança na reta de regressão)
g1 + geom_smooth(method = 'lm')
ggplotly(g1) # Este comando vem da biblioteca plotly. Podemos Passar o cursor do mouse nos pontos do gráfico

# Visualiza a correlação de Pearson entre as vendas e o preço do café
cor(dados$Vendas_Cafe, dados$Preco_Cafe) 

# Relação entre preço do leite e as vendas do café
plot(y = dados$Vendas_Cafe, x = dados$Preco_Leite, main = 'Relação entre Vendas do Café VS Preço do Leite', xlab = 'Preço do Leite', ylab = 'Qtde Vendida do Café', pch = 16)
grid()

# Coeficiente de correlação preço do leite e as vendas do café
cor(dados$Preco_Leite, dados$Vendas_Cafe)

# Gráfico 3D entre as vendas do café, preço do café e preço do leite 
# O gráfico é interativo, clique e arraste-o com o mouse para mudar o angulo de visualização
plot_ly(dados, z = ~Vendas_Cafe, x = ~Preco_Cafe, y = ~Preco_Leite) %>% add_markers()

# Relação entre vendas com promoção e sem promoção
boxplot(dados$Vendas_Cafe ~ dados$Promocao)

# Também podemos utilizar ggplot e o plotly
g2 <- ggplot(data = dados, aes(y = Vendas_Cafe, x = Promocao, col = Promocao)) + geom_boxplot()
ggplotly(g2)

# Podemos configurar a tela para exibir multiplos graficos
par(mfrow = c(2,2))

plot(y = dados$Vendas_Cafe, x = dados$Preco_Cafe, pch = 16, main = 'Vendas Cafe vs Preco Cafe')

plot(y = dados$Vendas_Cafe, x = dados$Preco_Leite, pch = 16, main = 'Vendas Cafe vs Preco Leite')

boxplot(dados$Vendas_Cafe ~ dados$Promocao, main = 'Vendas Cafe vs Promocao')

hist(dados$Vendas_Cafe, main = 'Distribuicao das vendas do cafe') 

dev.off() # Comando para voltar a visualização padrão da área de gráfico

# Ajusta um modelo de regressão linear múltipla
modelo <- lm(Vendas_Cafe ~ Preco_Cafe + Preco_Leite + Promocao, data = dados)

# Visualizar resumo do ajuste do modelo
summary(modelo)

# Diagnóstico de resíduos 
par(mfrow = c(2,2))
plot(modelo,pch = 16) dev.off()

# Caso input novos dados para realizarmos predições, iremos criar um data frame sem a variável resposta vendas do café, 
# pois ela será estimada pela equação de regressão que ajustamos
dados_para_predicao <- data.frame(Preco_Cafe = c(4.77, 4.67, 4.75), Promocao = c("Nao", "Nao", "Sim"), Preco_Leite = c(4.74, 4.81, 4.36))

# Observe que não há variável resposta 'Vendas do Café'
View(dados_para_predicao)

# Estima a variável resposta pra cada observação do novo data frame
predicoes <- predict(modelo, newdata = dados_para_predicao) 
View (data.frame(dados_para_predicao, predicoes))

# Metodo Stepwise para seleção automática de variáveis 
nova_variavel = rpois(n = 30, lambda = 2)
fit2 <- lm(Vendas_Cafe ~ Preco_Cafe + Promocao + Preco_Leite + nova_variavel, data = dados)
summary(fit2) # Observe o p valor da nova variável não é significativo 
fit2 <- step(fit2, direction = 'both')
summary(fit2) # Observe que o stepwise removeu a nova variável

