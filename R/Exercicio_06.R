#--------------
##Exercicio 6
#--------------

#Crie um script no R contendo os seguintes passos:
  
#1- leia o arquivo trees.csv que já está na pasta data da aula 4, antes transferir para o seu diretório data

ex6 = read.csv("Data/trees.csv", header = TRUE, sep = ",", dec = ".")
ex6
head(ex6)
tail(ex6)

#2- faça o sumário dos dados

summary(ex6)

#3- calcule o desvio padrão e a variância de cada uma das variáveis

apply(ex6, 2, sd)
apply(ex6, 2, sd)

#4- construa um histograma para cada uma das variáveis

par(mfrow=c(1,3))
hist(ex6$Girth)#histograma padrão para os dados das 3 variaveis
hist(ex6$Height)
hist(ex6$Volume)

#5- construa um boxplot das variáveis

boxplot(ex6$Girth)
boxplot(ex6$Height)
boxplot(ex6$Volume)
par(mfrow=c(1,1))

#6- identifique o(s) outliers no conjunto de dados. Qual(is) são os valores extremos? De que variável(is)?

boxplot(ex6$Girth, plot = FALSE)#sem outliers, valores extremos 8,30 e 20,60
boxplot(ex6$Height, plot = FALSE)#sem outliers, valores extremos 63 e 87
boxplot(ex6$Volume, plot = FALSE)#com outlier (77), valores extremos 10.2 e 58.3

outlier <- (boxplot(ex6$Volume, plot=FALSE))$out#criando objeto com outlier
outlier#chamando outlier

which(ex6$Volume %in% outlier)#buscando a posicao do outlier
ex6[ex6$Volume %in% outlier, c("Girth", "Height", "Volume")]#indexando a linha do outlier

#7- calcule a correlação par a par entre cada uma das variáveis
require(GGally)
ggpairs(ex6)
