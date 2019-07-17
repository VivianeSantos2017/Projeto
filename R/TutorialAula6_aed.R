#----------------------------------------
##Tutorial Analise Exploratoria de Dados
#----------------------------------------

##Parte 1: O quarteto de Anscombe

data("anscombe")#carregando o dado Anscombe da base do R

#funcoes basicas para checar os dados
dim(anscombe) # conferindo dimensao dos dados, N de linhas e N de colunas (11 e 8)
head(anscombe) # olhando as seis primeiras linhas dos dados
class(anscombe) # checando a classe do objeto (data.frame)
str(anscombe) # checando a estrutura do objeto (11 obs de 8 variaveis numericas)

#tirando media de colunas especificas com a funcao media
mean(anscombe$x1)#media da coluna x1 do objeto anscombe (9)
mean(anscombe$x2)#9
mean(anscombe$x3)#9
mean(anscombe$x4)#9

# tirando a media dos 4 vetores x com apenas em 1 linha de comando
apply(anscombe[,1:4], 2, mean) #funcao apply aplica uma funcao a todas as linhas das colunas 1 a 4, do objeto anscombe, neste caso a media (mean)

#media de todos os vetores y com a funcao apply
apply(anscombe[,5:8], 2, mean) #7.500909 para todos

# variância dos dados
apply(anscombe, 2, var) # aplica a funcao var -variancia dos dados - a todas as linhas do objeto (11 para os vetores x e 4.1 para os vetores y)

# checando correlação
cor(anscombe$x1, anscombe$y1)
cor(anscombe$x2, anscombe$y2)
cor(anscombe$x3, anscombe$y3)
cor(anscombe$x4, anscombe$y4)#0.82 para todos os pares

# coeficiente de regressão
## primeiro criamos objetos com as regressoes dos quatro conjuntos
m1 <- lm(anscombe$y1 ~ anscombe$x1)
m2 <- lm(anscombe$y2 ~ anscombe$x2)
m3 <- lm(anscombe$y3 ~ anscombe$x3)
m4 <- lm(anscombe$y4 ~ anscombe$x4)
#outra forma de fazer o modelo linear retirando o cifrao para coluna e adicionando o argumento data
m1 <- lm(y1~x1, data = anscombe)
## vamos criar agora uma lista com todos os modelos para facilitar o trabalho
mlist <- list(m1, m2, m3, m4)
## agora sim podemos calcular de forma menos repetitiva os coeficientes de regressao
#mas ainda podemos explorar os dados separadamente, desde que seja indexado o modelo desejado
mlist[[1]]#indexando o elemento 1 da lista, qdo indexa lista tem que colocar 2 colchetes
summary(mlist[[1]])#traz infos do modelo m1
coef(mlist[[1]])#traz o intercepto e a inclinacao do m1
#calculando de forma menos repetitiva os coeficientes de regressao
lapply(mlist, coef)#para tirar o intercepto e a inclinacao de todos os modelos da lista de uma só vez, mesmos valores de novo (intercept = 3, inclinacao = 0,5)


#Os dados têm mesma média, mesma variância, mesma correlação e mesmo valores dos coeficientes (intercepto e inclinação do modelo linear). Em que os dados são diferentes?
anscombe#vendo o dado, os valores parecem diferentes, mas qto...

# funcao par para definir as configuracoes da janela grafica entre em ?par
par(mfrow=c(2,2), #abre uma janela gráfica com 2 linhas e 2 colunas
    las=1, # deixa as legendas dos eixos na vertical
    bty="l") # tipo do box do grafico em L 
plot(anscombe$y1 ~ anscombe$x1) #plot das variaveis
abline(mlist[[1]]) # adicionando a reta prevista pelo modelo de regressao
plot(anscombe$y2 ~ anscombe$x2) #repetindo para os demais conjuntos
abline(mlist[[2]])
plot(anscombe$y2 ~ anscombe$x3)
abline(mlist[[3]])
plot(anscombe$y3 ~ anscombe$x4)
abline(mlist[[4]])#definitivamente nao parecem iguais
par(mfrow=c(1,1)) # retorna a janela grafica para o padrao de 1 linha e 1 coluna

##Parte 2: Uma rotina (entre muitas possíveis) de análise exploratória de dados (aed)

data(iris)#carregando os dados iris da base do R, dados gerados por um estatístico Ronald Fisher e coletor Edgar Anderson
iris#chamando o objeto
head(iris)
summary(iris) #verificando valores min, max, media, mediana, 1o e 3o quartis de cada coluna

table(iris$Species)#qtas infos por especie (50)

##medias das variaveis por especie de Iris
# media do comprimento de sepala por especie, usando a funcao tapply
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = mean)
# a mesma tarefa, executada por outra funcao. Outros argumentos e outra saida.
aggregate(x = iris$Sepal.Length, by = list(iris$Species), FUN = mean)
aggregate(x = iris$Sepal.Length, by = list(SL=iris$Species), FUN = mean)#nomeando o título da coluna 1 no comando list (SL)
# ainda a mesma tarefa, com a mesma funcao mas em uma notacao diferente
aggregate(Sepal.Length ~ Species, data=iris, mean)

iris.stat=aggregate(Sepal.Length ~ Species, data=iris, mean)
iris.stat
iris.stat$Sepal.Width = aggregate(Sepal.Width ~ Species, data=iris, mean)$Sepal.Width
iris.stat

#calculando o mesmo para as outras variaveis
aggregate(Sepal.Length ~ Species, data=iris, mean)#saida como uma matriz, permite adicionar colunas com outros valores, por exemplo desvpad
aggregate(Sepal.Width ~ Species, data=iris, mean)
aggregate(Petal.Length ~ Species, data=iris, mean)

#calculando desvio padrao de cada especie para todas as variaveis
tapply(X = iris$Sepal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Sepal.Width, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Length, INDEX = list(iris$Species), FUN = sd)
tapply(X = iris$Petal.Width, INDEX = list(iris$Species), FUN = sd)

##Solucao de como calular a media por especie de todas as variÃ¡veis, sem precisar um comando para cada variavel. Para isso, vamos usar o comando for e executar todas as tarefas em um mesmo ciclo.
# criando matriz de 3 colunas - uma para cada sp - e 4 linhas - uma para cada metrica
medias <- matrix(NA, ncol=3, nrow=4)
medias
# definindo o nome das colunas e das linhas da matriz
colnames(medias) <- unique(iris$Species)#colunas com os nomes existentes no conteudo da coluna Species do objeto iris, citados uma unica vez (funcao unique, funciona para numero tb, nao somente caracter)
rownames(medias) <- names(iris)[-5]#nomes das linhas sao as variaveis de todas as colunas menos a 5 que e Species
for (i in 1:4){
  medias[i,] <- tapply(iris[,i], iris$Species, mean)  
}#looping com a funcao 'for' inserindo as medias de cada variavel do objeto iris na tabela medias criada vazia
medias#tabelas com as medias de cada variavel nas linhas e de cada especie nas colunas

##Estatistica Descritiva

#medidas de tendencia central

vars <- iris[,-5]#criando objeto vars indexando todas as colunas menos a 5 que e Species e nao variavel
apply(vars, 2, mean)#tirando media de todas as colunas de vars
apply(vars, 2, median)#tirando mediana de todas as colunas de vars
#tirando a moda, valor mais frequente na amostra
freq_sl <- sort(table(iris$Sepal.Length), decreasing = TRUE)#mostra para cada numero dessa coluna, qtas vezes ele apareceu; qdo coloca ordem decrescente, vc pede para o mais frequente aparecer na posicao 1
freq_sl#vendo o resultado
freq_sl[1]#pedindo para ver a posicao 1, vc ve qual o valor mais frequente (5) e com que frequencia ele aparece (10), que e a maior

#medidas de dispersao

#variancia: desvio da media
apply(vars, 2, var)#tirando variancia de todas as colunas de vars

#desvio padrão: raiz quadrada da variancia
sd01 <- apply(vars, 2, sd)#tirando desvio padrao
sd01
sd02 <- apply(vars, 2, function(x) sqrt(var(x))) #outra forma de tirar o desvpad é criando uma funcao que recebe um valor x e calcula o quadrado da variancia desse valor x
sd02
sd01==sd02#checando se as duas formas de calcular desvpad deram valores iguais - sim, tudo TRUE

#criando funcao para a media na mao
a = rnorm(100,5,1)
mean(a)

my_mean = function(x) {
  sum(x)/length(x)
}
my_mean(a)

#criando funcao para a media na mao
my_var = function(x){
  sum(((x-mean(x))^2)/(length(x)-1))
}
my_var(a)

var(a)#checando com a funcao do R

#Coeficiente de variacao: medida relativa de desvio padrao

cv <- function(x){
  sd(x)/mean(x)*100
}#Nao existe no R base uma funcao para calcular o coeficiente de variacao. Isto nao e um problema. Vamos formalmente criar nossa primeira funcao de R. Para isso, usamos a funcao function
apply(vars, 2, cv)#agora calculamos o coeficiente de variacao utilizando o objeto cv que e a funcao para calcular o coeficiente

#Quantis e percentis
apply(vars, 2, quantile)# sumario de 5 numeros
apply(vars, 2, quantile, probs=c(0.05, 0.5, 0.95))# 5%, 50% e 95%

#Intervalo: diferenca entre o maior e o menor valor de determinada variavel.
apply(vars, 2, range)# a funcao range nos retorna os valores minimo e maximo
# aplicando a funcao diff ao resultado do range, temos o valor desejado
# uma boa pratica e nunca sobrescrever um objeto ja existente no R, por isso
# nunca nomeie um objeto com um nome ja existente
my_range <- function(x){ 
  diff(range(x)) 
}#criando o objeto my_range que e uma funcao para calcular o intervalo
apply(vars, 2, my_range)

#Intervalo interquartil (IIQ)
apply(vars, 2, IQR)#O IIQ e a diferenca entre o quartil superior (75%) e o quartil inferior (25%).

#Correlacao de todas as variaveis do objeto vars
cor(vars)

##Metodos graficos

#Grafico de barras: mostra a frequência de observações em uma dada classe
barplot(table(iris$Species))#funcao do pacote base do R. Há funcao correspondente no ggplot2, ver tutorial aula 6 e R reference card 3.0 disponivel no CRAN (doc de 2004).Neste caso, todas as espécies tem o mesmo número de observações.

#Histograma
par(mfrow=c(2,2))
hist(iris$Sepal.Length)#histograma padrão para os dados das 4 variaveis medidas nas espécies de Iris
hist(iris$Sepal.Width)
hist(iris$Petal.Length)
hist(iris$Petal.Length)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, breaks = 4)#mesmo histograma, mas estabelecendo um numero de intervalos menor (breaks = 4)
par(mfrow=c(1,1))

#Curva de Densidade: mostra a probabilidade de observar determinado valor. Em comparação ao histograma, no eixo y, ao invés de termos a frequência, temos a densidade probabilística.

par(mfrow=c(1,2))
hist(iris$Sepal.Width)#histo padrao com frequencia no eixo y
hist(iris$Sepal.Width, freq = FALSE)#histo com dens probabilistica no eixo y
par(mfrow=c(1,1))

par(mfrow=c(1,2))
# plot da curva de densidade
plot(density(iris$Sepal.Width))
# plot da curva de densidade sobre o histograma de densidade
hist(iris$Sepal.Width, freq = FALSE)
lines(density(iris$Sepal.Width), col="blue") # note que agora estamos usando a funcao sem o comando add=TRUE. Nao e necessario para as funcoes secundarias line e abline se estas forem colocadas na sequencia do grafico criado.
par(mfrow=c(1,1))

#Box-plot ou box-whisker plot

#Fazendo os box-plots das variáveis gerais contidas no objeto iris
par(mfrow=c(2,2))
boxplot(iris$Sepal.Length)
boxplot(iris$Sepal.Width)
boxplot(iris$Petal.Length)
boxplot(iris$Petal.Width)

#olhando os valores por espécie

boxplot(Sepal.Length ~ Species, data=iris)
boxplot(Sepal.Width ~ Species, data=iris)
boxplot(Petal.Length ~ Species, data=iris)
boxplot(Petal.Width ~ Species, data=iris)
par(mfrow=c(1,1))#deu para identificar outliers no conjunto de dados? 

#Como podemos checar os outliers? Vamos usar a própria função boxplot para identificar os outliers.
boxplot(iris$Sepal.Width)

my_boxplot <- boxplot(iris$Sepal.Width, plot=FALSE)
my_boxplot#outliers estao na saida '$out'

outliers <- my_boxplot$out#criando um objeto que e uma lista e os valores outliers estao guardados no elemento $out da lista
outliers
#buscando a posicao dos outliers
which(iris$Sepal.Width %in% outliers)
# usando a posicao para indexar o objeto
iris[which(iris$Sepal.Width %in% outliers), c("Sepal.Width", "Species")]
#poderia fazer direto sem usar a funcao which
iris[iris$Sepal.Width %in% outliers, c("Sepal.Width", "Species")]

#Identificando outliers de maneira especie especifica.

boxplot(Sepal.Width ~ Species, data=iris)
my_boxplot2 <- boxplot(Sepal.Width ~ Species, data=iris, plot=FALSE)
my_boxplot2
# o objeto é uma lista e os valores outliers estão guardados no elemento $out da lista
outliers2 <- my_boxplot2$out
outliers2
# neste caso, queremos apenas os outliers da especie setosa
# vamos usar a posicao para indexar o objeto
iris[iris$Sepal.Width %in% outliers2 & 
       iris$Species=="setosa", 
     c("Sepal.Width", "Species")]#posicao 42, so 1 outlier e da setosa
iris[iris$Sepal.Width %in% outliers2 & 
       iris$Species=="versicolor", 
     c("Sepal.Width", "Species")]#os demais sao versicolor, posicoes 54, 88 e 94

#Entendendo a distribuicao dos dados

#Vamos olhar para os dados morfometricos das especies de Iris e comparar com uma distribuicao normal. No R, isto pode ser feito de forma visual com as funcoes qqnorm e qqline.

par(mfrow=c(1,3))
qqnorm(iris$Sepal.Length[iris$Species=="setosa"], 
       main="setosa")
qqline(iris$Sepal.Length[iris$Species=="setosa"])
qqnorm(iris$Sepal.Length[iris$Species=="versicolor"], 
       main="versicolor")
qqline(iris$Sepal.Length[iris$Species=="versicolor"])
qqnorm(iris$Sepal.Length[iris$Species=="virginica"], 
       main="virginica")
qqline(iris$Sepal.Length[iris$Species=="virginica"])
par(mfrow=c(1,1))

#Relacao entre variaveis
pairs(vars)#gera graficos espelho, correlacionando as variaveis do objeto vars

##saida mais informativa utilizando o pacote GGally
install.packages("GGally")#instalando o pacote, deu warning pq o pcte foi criado em versao anterior a 3.6.1
library(GGally)# carregando o pacote GGally
ggpairs(vars)#gerando  grafico de relacao de variaveis, mas contendo informacoes extras, de distribuicao na diagonal e do valor da correlacao no espelho
