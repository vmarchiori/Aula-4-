# Econometria Avançada A4

# Noções Básicas - RSTUDIO

install.packages("urca") #instalando o pacote urca
library("urca") #carregando o pacote urca
install.packages("readxl") #instalando o pacote readxl
library(readxl) #carregando o pacote readxl
interdaay <- read_excel("C:/Econometria/interdaay.xls", col_types = c("date", "numeric", "numeric", "numeric")) #Sobe para o R a tabela de excel interdaay que está no computador
View(interdaay) #Mostra a tabela interdaay
interdaay <- interdaay[,-1] #retira a primeira coluna da tabela interdaay
colnames(interdaay)[2] <- "Variação" #altera o nome da segunda coluna
write.csv(interdaay,file = "Ibovespa.csv")

# Contruindo SÉRIES Temporais RSTUDIO

dados_diarios <- ts(interdaay, start = 2017-01-10, frequency = 365) #Constrói uma série temporal com os dados da tabela que se inicia em 2017-01-10 utilizando a frequencia diaria
Variação <- ts(interdaay$Variação, start = 2017-01-10, frequency = 365) #Constrói uma Série temporal Sobre a Variação
Ibovespa <- ts(interdaay$Ibovespa, start = 2017-01-10, frequency = 365) #Constrói uma série temporal sobre o Indice Ibovespa
Quantidade <- ts(interdaay$Quantidade, start = 2017-01-10, frequency = 365) #Constrói uma Série temporal Sobre a Quantidade
plot(dados_diarios, col= "blue", main="Dados do Indice Bovespa", xlab="Dias") #Constrói um grafico dos dados diarios
plot(Variação, main="Percentual de Variação") #cria um grafico da Variação com titulo Percentual de Variação
plot(Ibovespa, main="Indice do Dia",col="red") #cria um grfico do Ibovespa com titulo Indice do Dia e com cor vermelha
plot(Quantidade, main="Indice do Dia", xlab="Dias", col="blue") #cria um gráfico da Quantidade com titulo Indice do dia com a descrição do eixo x como Dias e cor azul

# Realizando o Teste de Dick-Fuller Sobre os Dados

TesteDF_Variação_none <- ur.df(Variação, "none",lags = 0) # Realzando o teste 1 DF-DickFuller sem drift e sem tendencia
TesteDF_Variação_none #não contém o valor crítico do teste
summary(TesteDF_Variação_none) #Mostra o resumo estatistico do teste que contém o valor crítico com 1% 5% e 10% de significancia
TesteDF_Variação_drift <- ur.df(Variação, "drift", lags=0) #teste 2 com drift
TesteDF_Variação_drift #não contém o valor crítico do teste
summary(TesteDF_Variação_drift) #resumo estatistico do teste contém o valor crítico com 1% 5% e 10% de significancia

TesteDF_Variação_trend <- ur.df(Variação, "trend", lags = 0) #teste 3 com tendência e com drift
TesteDF_Variação_trend #não contém o valor crítico do teste
summary(TesteDF_Variação_trend) #resumo estatistico do teste contém o valor crítico com 1% 5% e 10% de significancia

TesteDF_Ibovespa_none <- ur.df(Ibovespa, "none",lags = 0) #teste 1 DF-DickFuller sem drift e sem tendência
TesteDF_Ibovespa_none #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_none) #resumo estatistico do teste contém o valor crítico com 1% 5% e 10% de significancia

TesteDF_Ibovespa_drift <- ur.df(Ibovespa, "drift", lags=0) #teste 2 com drift
TesteDF_Ibovespa_drift #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_drift) #resumo estatistico do teste contémm o valor crítico com 1% 5% e 10% de significÃ¢ncia

TesteDF_Ibovespa_trend <- ur.df(Ibovespa, "trend", lags = 0) #teste 3 com tendênncia e com drift
TesteDF_Ibovespa_trend #não contém o valor crítico do teste
summary(TesteDF_Ibovespa_trend) #resumo estatísstico do teste contémm o valor crítico com 1% 5% e 10% de significancia

#Salvar CNVAZQUEZ

