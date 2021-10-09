# Uma empresa que trabalha com vendas em um e-commerce deseja estudar quais características (variáveis) dos seus clientes que 
# impactam no consumo, ou seja, as variáveis que têm relação ao quanto o cliente consome (compra). 
# Para este objetivo, foram selecionados aleatoriamente 29 clientes do banco de dados.

# Foram selecionadas seis variáveis para esta análise:
# 1. Consumo (variável contínua medida em reais) – É a variável resposta, nos diz qual foi o consumo de cada cliente.
# 2. Estado Civil (variável categórica com três níveis) – Nos informa se o cliente é Casado, Divorciado ou Solteiro.
# 3. Gênero (variável categórica com dois níveis) – Nos informa se o cliente faz parte do público masculino ou do público feminino.
# 4. Idade (variável discreta medida em anos) – Nos informa a idade do cliente.
# 5. Renda Mensal (variável contínua medida em reais) – Nos informa a renda mensal do cliente.
# 6. Possui Imóvel Próprio (variável categórica com dois níveis) – Nos informa se cliente possui imóvel próprio ou não.

# Limpar o ambiente
rm(list = ls())

# Forma o conjunto de dados histórico contendo vinte e nove clientes selecionados aleatoriamente do banco de dados e o armazena
# em um dataframe denominado "dados"

dados <- data.frame(
  Consumo = c(2595, 8470, 4007, 6734, 33628, 3903, 
              13444, 12560, 31176, 5435, 26736, 3728, 8684, 8356, 36936, 3744, 
              30420, 5958, 1019, 11688, 4442, 2640, 23888, 25844, 7430, 10276, 
              3381, 3512, 4957),
  Estado_Civil = c("Divorciado", "Casado", "Divorciado", 
                   "Casado", "Solteiro", "Divorciado", "Solteiro", "Solteiro", "Solteiro", 
                   "Casado", "Solteiro", "Divorciado", "Solteiro", "Solteiro", "Solteiro", 
                   "Divorciado", "Solteiro", "Divorciado", "Divorciado", "Solteiro", 
                   "Casado", "Solteiro", "Solteiro", "Solteiro", "Casado", "Solteiro", 
                   "Casado", "Casado", "Casado"), 
  Genero = c("Feminino", "Feminino", 
             "Masculino", "Feminino", "Feminino", "Feminino", "Feminino", 
             "Feminino", "Feminino", "Masculino", "Masculino", "Masculino", 
             "Feminino", "Feminino", "Masculino", "Masculino", "Feminino", 
             "Masculino", "Feminino", "Feminino", "Masculino", "Masculino", 
             "Masculino", "Masculino", "Masculino", "Feminino", "Feminino", 
             "Masculino", "Feminino"), 
  Idade = c(20.2585750236176, 32.1118436595569, 
            31.7467633607352, 37.8198500301827, 71.2083039792698, 29.7176362875943, 
            44.8093915220787, 35.8376118862763, 58.8385574663313, 39.9177951271474, 
            58.0550560477448, 21.778210059274, 31.7467633607352, 37.8198500301827, 
            71.2083039792698, 18.2385329827666, 61.5387187691615, 35.8376118862763, 
            20.2585750236176, 39.9177951271474, 37.9148802474609, 23.3568492662162, 
            54.9958059606044, 61.2973920327649, 34.58738731516, 29.7176362875943, 
            19.2584996796213, 20.2585750236176, 37.8080561311484), 
  Renda_Mensal = c(1985.88181437971,3769.2479179441, 2291.88059431382, 2390.40280655742, 8534.7173298783, 
                   2495.88975635737, 5714.55454591117, 5832.6343267674, 6882.89867212748, 
                   2099.31900110052, 6342.50934129081, 1941.14793399086, 3575.98064224822, 
                   3589.23716442208, 8534.7173298783, 2740.46957610393, 5714.55454591117, 
                   2390.40280655742, 1635.64603350165, 4364.56455252666, 1905.53820715893, 
                   2338.3086884437, 5832.6343267674, 6882.89867212748, 3842.53578942318, 
                   4919.14670791015, 2063.01440787432, 2380.55793401061, 1985.88181437971),
  Possui_Imovel_Proprio = c("Sim", "Sim", "Nao", "Sim", "Nao", 
                            "Nao", "Nao", "Sim", "Sim", "Nao", "Nao", "Nao", "Nao", "Nao", 
                            "Nao", "Sim", "Nao", "Sim", "Nao", "Nao", "Nao", "Sim", "Sim", 
                            "Nao", "Nao", "Nao", "Nao", "Nao", "Nao")
)

View(dados)

#Explorar a variável resposta "Consumo":

# Histograma do Consumo
hist(dados$Consumo)

# Boxplot do Consumo
boxplot(dados$Consumo)

# Estatísticas descritivas do Consumo
summary(dados$Consumo)


# Suspeita-se que a variável Estado Civil está associada com o fato do cliente possuir Imóvel Próprio. 
# Investigar a relação entre essas duas variáveis: 

# Gerae tabela de contigência entre Estado Civil e Possui Imovel Proprio
tabela_contigencia <- table(dados$Estado_Civil, dados$Possui_Imovel_Proprio)
tabela_contigencia
plot(tabela_contigencia)

# Realização do Teste Qui-Quadrado
chisq.test(tabela_contigencia)

# Exploração da relação entre as variáveis Consumo e Possui Imóvel Próprio:
boxplot(dados$Consumo ~ dados$Possui_Imovel_Proprio)

# Test t de Student
t.test(dados$Consumo ~ dados$Possui_Imovel_Proprio, 
       paired = FALSE,                                  # amostras não pareadas
       alternative = 'two.sided',                       # bilateral
       conf.level = 0.95                                # 95% de confianca
)


# Exploração da relação entre as variáveis Consumo e Idade:
plot(y = dados$Consumo, x = dados$Idade, pch = 16)

# Coeficiente de correlação
cor(dados$Consumo, dados$Idade)

# Ajuste regressão linear do Consumo em funcao da Idade
regressao_linear <- lm(Consumo ~ Idade, data = dados)
summary(regressao_linear)

# Teste de normalidade para os residuos da regressao
shapiro.test(regressao_linear$residuals)

# Exploração da variável Renda Mensal:
summary(dados$Renda_Mensal)

#Exploração da relação entre as variáveis Consumo e Idade:
  
#Regressao linear do Consumo em funcao da Idade e Renda Mensal
regressao_linear <- lm(Consumo ~ Idade + Renda_Mensal, data = dados)
summary(regressao_linear)