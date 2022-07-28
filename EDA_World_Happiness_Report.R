#### PROJECT: World Happiness Report ####

#### Exploratory Data Analysis Socioeconomic ####

#### 1. Working Directory ####
# Configurando o diretório de trabalho
setwd("C:/Users/Utilizador/repos/Formacao_cientista_de_dados/big_data_analytics_R_microsoft_azure_machine_learning/Modulo_17")
getwd()

#### 2. Imports ####
# install.packages("dplyr") 
# install.packages("ggplot2") 
library(dplyr) # manipulação de dados
library(ggplot2) # construção de gráficos

#### 3. Data Loading ####
dados <- read.csv("dataset.csv")

#### 4. Data Description ####
# Descreve, compreende, organiza e resumi os dados

##### 4.1 Data Viewer #####
# Visualiza os dados
View(dados)

##### 4.2 Data Dimension #####
# Dimensões
dim(dados)

##### 4.3 Data Type #####
# Variáveis e tipos de dados
str(dados)

##### 4.3 Data Summary #####
# Sumários das variáveis numéricas
summary(dados)

#### 5. Data Cleaning ####
# Limpeza dos Dados

##### 5.1 Check NA #####
# Quantas linhas (observação/ registro/ caso) tem casos completos?
# Pra cada linha, do dataset, todas as colunas estão preenchidas?
complete_cases <- sum(complete.cases(dados))
complete_cases

# Quantas linhas tem casos incompletos?
# Quantas linhas, do dataset, não possuem todas as colunas preenchidas. 
# Ou seja, quantas linhas possuem ao menos uma coluna não preenchida
not_complete_cases <- sum(!complete.cases(dados))
not_complete_cases

# Qual o percentual de dados incompletos?
# 14% do meu dataset possui valores ausentes (NA)
percentual <- (not_complete_cases / complete_cases) * 100
percentual

# Remove os objetos anteriores para liberar memória RAM
rm(complete_cases)
rm(not_complete_cases)

##### 5.2 Rename Columns #####
# Nomes das colunas
colnames(dados)

# Grava os nomes das colunas em um vetor
myColumns <- colnames(dados)
myColumns

# Vamos renomar as colunas para facilitar nosso trabalho mais tarde
myColumns[1] <- "NomePais"
myColumns[2] <- "Ano"
myColumns[3] <- "IndicadorNivelVida"
myColumns[4] <- "PIB_Per_Capita"
myColumns[5] <- "SuporteSocial"
myColumns[6] <- "ExpectativaVida"
myColumns[7] <- "IndicadorLiberdade"
myColumns[8] <- "IndicadorGenerosidade"
myColumns[9] <- "IndicadorCorrupcao"
myColumns[10] <- "IndicadorEmocoesPositivas"
myColumns[11] <- "IndicadorEmocoesNegativas"

# Verifica o resultado
myColumns

# Atribui os novos nomes de colunas ao dataframe
colnames(dados) <- myColumns
rm(myColumns)

# Visualiza os dados
View(dados)

##### 5.2 Fill Na #####
# Verificando quantos países foram incluídos na coleta de dados
length(unique(dados$NomePais))

# Lista os países únicos e grava o resultado (antes de remover registros com valores NA)
list_countries_with_na <- unique(dados$NomePais)
list_countries_with_na

# Vamos eliminar linhas com valores NA
dados <- na.omit(dados)

# Dimensões
dim(dados)

# Lista de países após remover valores NA
list_of_countries_without_na <- unique(dados$NomePais)
list_of_countries_without_na

# Verificando se perdemos países ao remover valores NA
# antes de remover NA
length(list_countries_with_na)
# depois de remover NA
length(list_of_countries_without_na)

# Verificando a diferença antes e depois de remover valores NA
# Verificado quais paises foram removidos por ter ao menos uma coluna vazia
setdiff(list_countries_with_na, list_of_countries_without_na)

# PORQUE REMOVES OS NA?
# Ou seja porque não tratei os NA, com imputação por exemplo?
# Substituindo com a média dos valores a coluna faltante;
# Para esse conjunto de dados especifíco entendeu-se ser melhor remover os NA
# Pois o pib é uma valor muito individual visto que o PIB pode variar de pais para pais
# Tendo em conta muitos outros critérios individuais

# Remove os objetos
rm(list_countries_with_na)
rm(list_of_countries_without_na)

#### 6. Data Filtering ####
# Verificando quais anos estão presentes nos dados
# valores unicos da coluna year(ano)
anos <- unique(dados$Ano)

# valores maximo e minimo da variavel ano
range(anos)

# comprimento(quantidade) da varavél ano
length(unique(dados$Ano))

# Depois e verificados os valore, se não for mais ser utilizada removemos a variavel da sessão
rm(anos)

# Número de registros por ano
# Frequência
table(dados$Ano)
hist(dados$Ano)

# Vamos remover os anos com menor contribuição (menor volume de dados)
# Aqui vamos remover os valores extremos (outliers)
# Como a média é aproximadamente 100 registros por ano
# Tudo que estver abaixo de 100 será removido
dados_por_anos <- dados[dados$Ano!=2005 & dados$Ano!=2006 & dados$Ano!=2007 & dados$Ano!=2020,]

# Número de registros por ano após remoção dos valores extremos
table(dados_por_anos$Ano)

#### 7. Descriptive Statistivas ####

###### 7.1 Numerical Attributes #####

# Extraindo as variáveis numéricas
numeric_variable_list <- sapply(dados, is.numeric)
numerical_data <- dados[numeric_variable_list]

# Matriz de Correlação das variavéis numéricas
cor(numerical_data)

# Correlation Plot
# Plot com todas as variaveis numéricas
pairs(numerical_data)

# Analisando as variaveis em grupos: 1 a 5
pairs(numerical_data[1:5],labels = colnames(numerical_data)[1:5])

# Analisando as variaveis em grupos: 6 a 10
pairs(numerical_data[6:10],labels = colnames(numerical_data)[6:10])

#### 8. Business Questions ####

##### 8.1 Data Organization #####
# Iremos realizar a análise considerando a média de indicadores por país;
# Calculamos as médias fazendo agrupamento por indicador e concatenamos os dataframes resultantes;

# Visualiza os dados
View(dados)

# Nomes das colunas
colnames(dados)

# Agrupando os dados e calculando média por país da variavél Pib_Per_Capita
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do pib_per_capita por país;
pib_per_capita_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(PIB_Per_Capita = mean(PIB_Per_Capita))

pib_per_capita_pais_media

# Agrupando os dados e calculando média por país da variavél SuporteSocial
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do SuporteSocial por país;
suporte_social_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(SuporteSocial = mean(SuporteSocial))

suporte_social_pais_media

# Merge dos dataframes
df_medias <- merge(pib_per_capita_pais_media, suporte_social_pais_media)
View(df_medias)

# Remova o que não estiver mais usando
rm(pib_per_capita_pais_media)
rm(suporte_social_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorNivelVida
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do IndicadorNivelVida por país;
ind_nivel_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorNivelVida = mean(IndicadorNivelVida))

ind_nivel_vida_pais_media

# Merge
df_medias <- merge(df_medias, ind_nivel_vida_pais_media)
View(df_medias)
rm(ind_nivel_vida_pais_media)

# Agrupando os dados e calculando média por país da variavél ExpectativaVida
# Agrupamos o dataset pelo nome do país;
# Calculamos a média da ExpextativaVida por país;
expectativa_vida_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(ExpectativaVida = mean(ExpectativaVida))

expectativa_vida_pais_media

# Merge
df_medias <- merge(df_medias, expectativa_vida_pais_media)
View(df_medias)
rm(expectativa_vida_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorLiberdade
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do IndicadorLiberdade por país;
ind_liberdade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorLiberdade = mean(IndicadorLiberdade))

ind_liberdade_pais_media

# Merge
df_medias <- merge(df_medias, ind_liberdade_pais_media)
View(df_medias)
rm(ind_liberdade_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorGenerosidade
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do IndicadorGenerosidade por país;
ind_generosidade_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorGenerosidade = mean(IndicadorGenerosidade))

ind_generosidade_pais_media 

# Merge
df_medias <- merge(df_medias, ind_generosidade_pais_media)
View(df_medias)
rm(ind_generosidade_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorCorrupção
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do IndicadorCorrupcao por país;
ind_corrupcao_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorCorrupcao = mean(IndicadorCorrupcao))

ind_corrupcao_pais_media

# Merge
df_medias <- merge(df_medias, ind_corrupcao_pais_media)
View(df_medias)
rm(ind_corrupcao_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorEmocoesPositivas
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do EmoçoesPositivas por país;
ind_pos_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesPositivas = mean(IndicadorEmocoesPositivas))

ind_pos_pais_media 

# Merge
df_medias <- merge(df_medias, ind_pos_pais_media)
View(df_medias)
rm(ind_pos_pais_media)

# Agrupando os dados e calculando média por país da variavél IndicadorEmocoesNegativas
# Agrupamos o dataset pelo nome do país;
# Calculamos a média do EmocoesNegativas por país;
ind_neg_pais_media <- dados %>%
  group_by(NomePais) %>%
  summarize(IndicadorEmocoesNegativas = mean(IndicadorEmocoesNegativas))

ind_neg_pais_media

# Merge
df_medias <- merge(df_medias, ind_neg_pais_media)

# Visualizar df_medias
View(df_medias)

# Remover as variaveis que não usaremos mais
rm(ind_neg_pais_media)

# Dimensões
dim(df_medias)

##### 6.2 Plots #####
#Plots e Estatísticas

# Dados

# Nome das colunas
colnames(df_medias)

# Visualizar os dados
View(df_medias)

# Tipo de dados
str(df_medias)

###### * Pergunta 1 ######
# O aumento do PIB per capita de um país afeta positivamente a expectativa de vida dos cidadãos ao nascer?
# Qual a correlação entre essas duas variáveis?
# Resposta: Verdadeiro. Conforme o PIB aumento a expectativa de vida dos cidadãos ao nascer aumenta
# Correlação: 0.853 --> Alta correlação positiva
plot(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida)
cor.test(df_medias$PIB_Per_Capita, df_medias$ExpectativaVida, method = "pearson")

###### * Pergunta 2 ######
# O aumento da escala de vida afeta positivamente o indice de corrupção e a percepção do público geral sobre a corrupção nos negócios e governo?
# Qual a correlação entre essas duas variaveis?
# Resposta: Verdadeiro. Conforme aumenta a escala de vida diminui a indice de corrupção, ou seja, melhora a percepção dos indivuduos a respeito da corrupção seja nos negócios ou no governo.
# Correlação: -0.464 --> Correlação negativa
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorCorrupcao, method = "pearson")

###### * Pergunta 3 ######
# O aumento na escala de vida tem algum efeito na média de felicidade entre o público em geral?
# Qual a correlação entre essas duas variáveis?
# Resposta: Verdadeiro. Conforme a escala de vida aumenta a média das emoções positivas (felicedade) do publico geral aumenta;
# Correlação: 0.577 --> Correlação positiva
plot(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorNivelVida, df_medias$IndicadorEmocoesPositivas, method = "pearson")

###### * Pergunta 4 ######
# O país com o menor índice de suporte social tem maior percepção de corrupção em relação às empresas e ao governo no país?
# Resposta: Verdadeiro. Conforme aumenta o suporte social diminui a percepção de corrupção
# Correlação: -0.49 --> Correlação negativa

# Indicadores
# Qual pais com menor idice de suporte social? 
# African Republic 
df_medias[df_medias$SuporteSocial == min(df_medias$SuporteSocial),]

# Criando novo dataframe filtrando apenas o país: African Republic 
df1 <- df_medias[df_medias$NomePais == "Central African Republic",]

# Visualizar
View(df1)

# Indicador Suporte Social do país African Republic: 0.4024
df1$SuporteSocial

# Valor máximo Suporte social: 0.97
max(df_medias$SuporteSocial)

# Indicador Corrupção do país African Republic: 0.842
df1$IndicadorCorrupcao

# Valor máximo Indicador de corrupção: 0.95
max(df_medias$IndicadorCorrupcao)

# Plot e Estatísticas

# Filtrando todos os dados de todos os anos para o pais African Republic
df2 <- dados[dados$NomePais == "Central African Republic",]

# Visualizar
View(df2)

plot(df2$SuporteSocial, df2$IndicadorEmocoesPositivas)
cor.test(df2$SuporteSocial, df2$IndicadorEmocoesPositivas, method = "pearson")

###### * Pergunta 5 ######
# Pessoas generosas são mais felizes?
# Resposta: Verdadeiro.
# Correlação: 0.39 --> Correlação positiva
plot(df_medias$IndicadorGenerosidade, df_medias$IndicadorEmocoesPositivas)
cor.test(df_medias$IndicadorGenerosidade, df_medias$IndicadorEmocoesPositivas, method = "pearson")


