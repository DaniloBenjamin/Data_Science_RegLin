setwd("C:/Users/Danilo/OneDrive/FIA/P�s Data Science/Aulas/Analytics/03 - jan22/20210126")
getwd()
library(readxl)
options(scipen=30, digits=20)
startups <-read_excel("Analise de Residuos.xlsx", sheet = "Startups")
names(startups)
summary(startups)
library(GGally)
install.packages('psych')
ggpairs(startups, title = 'correlogram with ggpairs')
regressao <- lm(data = startups,
                 Lucro ~
                   Investimento_PeD + 
                   Investimento_em_Mkt + 
                   Gastos_Administrativos + 
                   Estado)
summary(regressao)

#Como o p valor de gastor administrativos � alto, retiramos a vari�vel

regressao1 <- lm(data = startups,
                Lucro ~
                  Investimento_PeD + 
                  Investimento_em_Mkt + 
                  Estado)
summary(regressao1)

#Como o p valor do estado de S�o Paulo � alto, retiramos a vari�vel

regressao2 <- lm(data = startups,
                 Lucro ~
                   Investimento_PeD + 
                   Investimento_em_Mkt)
summary(regressao2)

#Agora o pvalor e R^2 est�o ok verificaremos os res�duos

residuo <- residuals(regressao2) #fornece os residuos do modelo

# Graficos para verificar normalidade dos residuos
par(mfrow = c(1,2)) #comando que permite colocar gr�ficos um ao lado do outro
hist(residuo, col = "darkturquoise")
qqnorm(residuo, pch = 1, col = "darkturquoise", frame = FALSE)
qqline(residuo, col = "steelblue", lwd = 2)

predito <- fitted.values(regressao) # fornece os preditos do modelo

# Grafico para verificar igualdade de variancias
par(mfrow = c(1,1))
plot(predito, residuo, main = 'Residuos x Valores ajustados', ylab = 'Residuos', col = "darkturquoise")

#Chegamos a conclus�o que os preditos (residuos) n�o possuem um padr�o

# Quantificacao dos residuos
library(Metrics)
mape(actual = startups$Lucro, predicted = predito)
sse(actual = startups$Lucro, predicted = predito)

