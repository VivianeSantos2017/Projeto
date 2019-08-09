#----------------
##Graficos em R
#----------------

#Este script se trata de um tutorial disponibilizado na Aula 7 sobre gráficos. As figuras geradas a partir deste script (figura01 e figura02) estão na pasta figs.

##Grafico de dispersao: plot e abline

# lendo os dados direto do github: vai no endereco do arquivo, clica nele, clica em raw e copia o endereco na funcao read.csv.
sal <- read.csv("https://raw.githubusercontent.com/AndreaSanchezTapia/analise_de_dados_ENBT_2019/master/aula07/data/salarios.csv")
# explorando os dados com as funções head e summary
head(sal)
summary(sal)

## criando objetos para auxiliar a construção do gráfico
# criando modelos lineares
mh <- lm(salario ~ experiencia, data=sal[sal$sexo=="H",])
mm <- lm(salario ~ experiencia, data=sal[sal$sexo=="M",])
coefh <- coef(mh)#objeto so com os coeficientes
coefm <- coef(mm)
# definindo os limites dos eixos
limy <- c(min(sal$salario),max(sal$salario))
limx <- c(min(sal$experiencia),max(sal$experiencia))
# definindo os nomes dos eixos
labx <- "Experiência (anos)"
laby <- "Salário (R$)"

##construindo o grafico em si
# definindo os parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",], 
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x 
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)#nomeando como grafico A
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",], 
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)#nomeando como grafico B
# linha do previsto pelo modelo
## a + b*x 
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)

##Exportando o gráfico com as funções png() e dev.off()
# a funcao png cria o arquivo para depois gravar os comandos graficos dentro do arquivo, daqui pra frente você não vai mais ver o gráfico na tela plot
png("figs/figura01", res=300, width=2400, height=1200)#estabelecendo tamanho proporcional a resolucao
# define parametros graficos
par(mfrow=c(1,2), las=1, bty="l") # aqui estamos usando las e bty dentro do par para fixar para todas as janelas
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",], 
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x 
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
mtext("A", 3, adj=0, font=2)
## plot do salario das mulheres
plot(salario ~ experiencia, data=sal[sal$sexo=="M",], 
     col="navy",
     ylim=limy, xlim=limx,
     ylab="", xlab=labx)
mtext("B", 3, adj=0, font=2)
# linha do previsto pelo modelo
## a + b*x 
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# para finalizar o gráfico e gerar o arquivo, precisamos rodar o dev.off()
dev.off()#agora sim o grafico esta dentro do arquivo figura_01, se nao der dev.off() nao fecha a figura e tudo que vc rodar depois fica sobrescrito no png  figura01.

#mesmo gráfico anterior, mas com todos os pontos em uma mesma janela gráfica. Agora, vamos usar a função legend() para diferenciar os pontos.
# plot dos valores de salario dos homens
plot(salario ~ experiencia, data=sal[sal$sexo=="H",], 
     col="tomato",
     ylim=limy, xlim=limx,
     ylab=laby, xlab=labx)
# linha do previsto pelo modelo
## a + b*x 
abline(a=coefh[1], b=coefh[2],
       col='tomato', lwd=2)
## usando points para adicionar os pontos do salario das mulheres
points(salario ~ experiencia, data=sal[sal$sexo=="M",], 
       col="navy")
# linha do previsto pelo modelo das mulheres
## a + b*x 
abline(a=coefm[1], b=coefm[2],
       col='navy', lwd=2)
# incluindo a legenda
legend("topleft", legend=c("homens", "mulheres"),
       col=c("tomato", "navy"),
       lty=1, bty='n')#no lugar das opcoes do R (ex. topleft, vc pode dar as coordenadas e estabelecer exatamente outra posicao fora do padrao), lty e o tipo de linha, se fosse um ponto seria pch e nao lty, e bty = n e que vc nao quer contorno de caixa na legenda

##Boxplot

# criando vetor de cores
cores <- c("#3B9AB2", "#EBCC2A", "#F21A00")
# criando vetor com o nome das espécies
sp <- paste("I.", unique(iris$Species), sep=" ")

par(mfrow=c(2,2), bty='l', las=1)
boxplot(Sepal.Length ~ Species, data=iris, xlab="", col=cores,
        xaxt="n")#xlab sem nada pq só preciso nas janelas inferiores
axis(1, at=1:3, labels=sp, font=3)#no argumento font o numero 3 = italico
boxplot(Sepal.Width ~ Species, data=iris, xlab="", col=cores, 
        xaxt="n")#xaxt e para nao plotar os nomes das especies senao vai plotar nao italico, o default e xaxt = "s".
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Length ~ Species, data=iris,  col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
boxplot(Petal.Width ~ Species, data=iris, col=cores,
        xaxt="n")
axis(1, at=1:3, labels=sp, font=3)
par(mfrow=c(1,1))


##Gráfico de média com desvio padrão com arrows

# fixando uma semente de numeros aleatorios para manter o mesmo resultado no sample, com a funcao set.seed para que os valores sejam os mesmos em todos os computadores
set.seed(42)
# criando um data frame com valores medios e desvio padrão de uma variável
d2 <- data.frame(name=letters[1:5],
                 value=sample(seq(4,15),5),#amostrar 5 numeros na sequencia de de 4 a 15
                 sd=c(1,0.2,3,2,4))
d2

plot(x=1:5, d2$value, las=1, bty='l', ylim=c(0, 18), pch=19, xaxt='n',
     xlab="names", ylab="value")
axis(1, at=1:5, labels=d2$name)
arrows(x0=1:5,
       y0=d2$value+d2$sd,
       y1=d2$value-d2$sd, angle=90, length=0.05, code=3)

##Adicionando exemplos da aula com dados da base do R

##gráfico de barras com subcategorias

d.Titanic <- as.data.frame(Titanic)
head(d.Titanic)

barplot(Freq ~ Class + Survived, data = d.Titanic,
        subset = Age == "Adult" & Sex == "Male",
        ylab = "N of Passengers", legend = TRUE)

#gráfico de barras em “mosaico”
# Corresponding table :
xt <- xtabs(Freq ~ Survived + Class + Sex,
            d.Titanic,
            subset = Age=="Adult")#criando tabela de frequencia de sobreviventes, por classe e por sexo para individuos adultos
# Alternatively, a mosaic plot :
mosaicplot(xt[,,"Male"], main="", color=TRUE)#proporcao so para homens adultos

##grafico de dispersao com a base de dados cars
m1 <- lm(log(dist) ~ log(speed), data = cars)
plot(log(dist) ~ log(speed), data = cars,
     las=1, bty='l', pch=19)
abline(m1)

#adicionando uma imagem ao gráfico
install.packages("png")
install.packages("grid")
library(png)
library(grid)
pic <- readPNG("figs/car.png")#para dar certo tem que ter a figura car.png salva no diretorio figs
plot(log(dist) ~ log(speed), data = cars,
     las=1, bty='l', pch=19,
     col=mycol)#nao roda com col = mycol, pq nao tenho esse objeto que era para deixar as cores dos pontos num preto transparente ja que tem sobreposica. Mas roda sem, so mantem a cor preta cheia 
abline(m1)
grid.raster(pic, .25, .83, width=.2)

#gráficos com múltiplas janelas, tirando col = mycol
x <- log(cars$speed)
y <- log(cars$dist)
zones <- matrix(c(2,0,1,3), ncol=2, byrow=TRUE)
layout(zones, widths=c(4/5, 1/5), heights=c(1/5, 4/5))
xhist <- hist(x, plot=FALSE)
yhist <- hist(y, plot=FALSE)
top <- max(c(xhist$counts, yhist$counts))
plot(x,y, las=1, xlab="log(speed)", ylab="log(dist)",
     bty='l', pch=19)
abline(m1)
grid.raster(pic, .16, .63, width=.15)
par(mar=c(0,3,1,1))
barplot(xhist$counts, axes=FALSE, ylim=c(0, top),
        space=0)
par(mar=c(3,0,1,1))
barplot(yhist$counts, axes=FALSE, xlim=c(0, top),
        space=0, horiz=TRUE)

#para voltar na janela de plot original, clica na vassourinha da aba plot, pq se colocar par = (mfrow(1,1)) o graf fica estourado. A funcao dev.off() tb e util nesse caso, pq ela serve para fechar qq dispositivo grafico.
#uma janela grafica diferente da janela padrao da aba plot pode ser aberta com a funcao X11(), essa janela so fecha dando dev.off(). Vc pode rodar mais de um X11() para visualizar coisas diferentes, e o R te diz qual janela esta ativa, que e sempre a ultima que vc abriu.

##series temporais
data(longley)
head(longley)

barplot(GNP ~ Year, data = longley)
#de outra forma
plot(GNP ~ Year, data = longley, type='l',
     xaxt='n', las=1, bty='l', lty=2, lwd=2)
axis(1, at=longley$Year)

#de uma forma melhor
plot(GNP ~ Year, data = longley, type='l',
     xaxt='n', las=1, bty='l', lty=1, lwd=2)
axis(1, at=longley$Year)

##AVISO IMPORTANTE: No readme da aula 7 tem um exercicio e um desafio.
#O exercício foi elaborado em arquivo R Markdown e está na pasta dados.
#Para o desafio foi criado um script (arquivo DesafioAula7.R)