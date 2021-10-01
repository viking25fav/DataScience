####### Distribuições de Probabilidades #######


# DISTRIBUIÇÃO BINOMIAL 

# Exemplo: Definindo como sucesso o cliente comprar, e supondo que a probabilidade de sucesso é 50%.
# Ao passar 10 clientes em nossa loja, qual a probabilidade de realizarmos 2 vendas? 
# Ou seja, queremos encontrar a probabilidade de dois sucessos, em dez tentativas.
# Cuja probabilidade de sucesso em cada tentativa é 50%

dbinom (x = 2, size = 10, prob = 0.5)*100
# Onde:
# x é o número de sucessos,
# size é o número de tentativas,
# prob é a probabilidade de sucesso em cada tentativa


# A função a seguir gera quantidades aleatórias de sucesso oriundos de uma quantidade (size) de tentativas dada a probabilidade
# (prob) de sucesso.
# É útil para realizar experimentos. Podemos simular qual a frequencia esperada de vendas a cada dez clientes?
# Ainda mantendo a probabilidade de sucesso (cliente comprar) de 50%

va_binomial <- rbinom(n = 30, size=10, prob=0.5)
# Onde:
# n é a quantidade de vezes que o experimento deve ser repetido
# size é o número de tentativas a cada experimento
# prob é o número de sucesso em cada uma das tentativas
hist(va_binomial) # A maior barra no histograma representa a quantidade esperada de vendas
# Ajuste o parametro n para 1000 e plote o histograma, observe como a distribuição binomial se aproxima da normal


# Podemos também querer a probabilidade de que até dois clientes comprem.
# Ao invés de saber a probabilidade de exatos dois comprarem. #A probabilidade de até dois clientes comprarem é:
# (probabilidade de nenhum cliente comprar) + (probabilidade de um cliente comprar) + probabilidade de dois cliente comprarem)
# Formalizando: P(X<=2) = P(X=0) + P(X=1) + P(X=2)
pbinom(q = 2,size = 10, prob = 0.5)
# A probabilidade de que até dois clientes comprem ao entrarem dez clientes, é de 5,48%


# DISTRIBUIÇÃO GEOMÉTRICA 

# Exemplo: Definindo como sucesso o cliente comprar, e supondo que a probabilidade de sucesso é 50%.
# Qual a probabilidade da primeira venda ocorrer quando o quinto cliente entrar na loja?

dgeom(x = 5, prob = 0.5)
# Onde:
# x é o número de tentativas
# prob é a probabilidade de sucessos
# Podemos utilizar a mesma função para nos dar a probabilidade do sucesso ocorrer na primeira tentativa,

# Segunda tentativa, terceira tentativa ... até a décima tentativa.
va_geometrica <- dgeom(x = 1:10, prob = 0.5) va_geometrica
plot(va_geometrica) #Veja como as probabilidades vão diminuindo. A probabilidade de sucesso de 50% é relativamente alta,
# então é muito provavel que o sucesso ocorra logo nas primeiras tentativas

# Podemos utilizar a distribuição geométrica acumulada para saber qual a probabilidade do primeiro sucesso
# ocorrer na primeira tentativa OU na segunda tentativa OU na terceira tentativa #Formalizando, queremos: P(X<=3)
va_geometrica_acumulada <- pgeom(0:3, prob = 0.5) plot(va_geometrica_acumulada) 


# DISTRIBUIÇÃO BINOMIAL NEGATIVA

# Exemplo: Definindo como sucesso o cliente comprar, e supondo que a probabilidade de sucesso é 50%.
# Qual a probabilidade de ter que entrar 8 clientes até que a segunda venda ocorra?

dnbinom(x=2, size = 8, prob = 0.50)
# Onde:
# x é o número de sucessos
# size é a quantidade de tentativos
# prob é a probabilidade de sucesso


# DISTRIBUIÇÃO POISSON 

# Exemplo: Uma loja recebe em média, 6 (𝝺) clientes por minuto. Qual a probabilidade de que 5(x) clientes entrem em um minuto?

dpois(x= 5,lambda = 6)
#Onde:
# x é a quantidade a ser testada
# lambda é a taxa média de ocorrêcia do evento em um determinado período de intervalo de tempo ou espaço

# Podemos utilizar a mesma funcao para obter a probabilidade de entrar um cliente, dois clientes... quinze clientes
va_poison <- dpois(x = 1:15, lambda = 6)
plot(va_poison)
# Observe que os valores se distribuiem simetricamente en tormo de seis, use acontece porque o paramentro
# lambda é a média (e também o desvio padrão) da distribuição de Poisson

# Também podemos obter a probabilidade acumulada de até 5 clientes entrarem na loja em um minuto
#Formalizando, queremos: P(X<=5)
va_poison <- ppois(1:5, lambda = 6)
plot(va_poison)


# DISTRIBUIÇÃO NORMAL 

# Exemplo: Suponha que a distribuição dos salários dos funcionários de uma empresa sigam uma distribuição normal com média 𝜇=2.500 e desvio padrão σ= 170.
# Ao selecionar aleatoriamente um indivíduo dessa população, qual a probabilidade de ter salário entre 2.400 e 2.600 ?
# Precisamos achar a probabilidade do indivíduo ter um salário de até 2.600 e subtrair pela probabilidade do indivíduo ter o salário até 2.400 
# P(X<=2600)
probabilidade_ate_2600 <- pnorm(q = 2600, mean = 2500, sd =170 ) 
#P(X<=2400)
probabilidade_ate_2400 <- pnorm(q = 2400, mean = 2500, sd =170 )
# P(X<=2600) - P(X<=2400) 
probabilidade_ate_2600 - probabilidade_ate_2400

# Podemos gerar 100 números aleatórios para uma distribuição normal com média 2500 e desvio padrão 170
va_normal <- rnorm(n = 100, mean = 2500,sd = 170)
hist(va_normal)


# DISTRIBUIÇÃO NORMAL PADRÃO 

# O comando scale() padroniza uma variável aleatória.
# Ao aplicar o comando na variável va_normal que acabmos de criar, ela ficará com média zero e desvio padrão unitário.
va_normal_padrao <- scale(va_normal)
hist(va_normal_padrao)

# Exemplo: Suponha que a distribuição dos salários dos funcionários de uma empresa sigam uma distribuição normal com média 𝜇=2.500 e desvio padrão σ= 170.
# Ao selecionar aleatoriamente um indivíduo dessa população, qual a probabilidade de ter salário acima de 2.600? 
# Padronização
z <- (2600-2500)/170
pnorm(z, mean = 0, sd = 1)
# ou simplesmente
pnorm(z)

# Podemos também visualizar onde está o nosso valor Z em relação a média
plot(density(scale(va_normal))) # Plota curva de densidade
abline(v = 0,col = 'blue') # Gera uma linha sobre média, que é zero pois padronizamos a distribuição
abline(v = 0.58,col = 'red') # Gera uma linha sobre o valor z obtido


# DISTRIBUIÇÃO F 

#Gerando uma amostra aleatória de 1000 número seguindo uma distribuição F
va_f <- rf( n= 1000, df1 = 5 , df2 = 33 )
# Onde:
# n é a quantidade de números a ser gerado # df1 é o primeiro grau de liberidade
# df2 é o segundo grau de liberdade 
hist(va_f)
# Vá aumentando os graus de liberdade e observe como a distribuição se aproxima da normal
# Informação Extra: Uma distribuição F é a razão entre duas qui-quadrado 


# DISTRIBUIÇÃO T 

# Gera uma amostra aleatória de 1000 números seguindo uma distribuição T
va_t <- rt(1000, df = 2)
hist(va_t)
# Observe que a distribuição t, assim como a normal padrão, é centrada no zero
# Vá aumentando o grau de liberdade e observando o comportamento do histograma


# DISTRIBUIÇÃO QUI-QUADRADO
# Gera uma amostra aleatória de 1000 números seguindo uma distribuição qui-quadrado
va_QuiQuadrado <- rchisq(1000,df = 3) 
hist(va_QuiQuadrado)