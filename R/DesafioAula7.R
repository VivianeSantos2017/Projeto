#----------------
##DESAFIO AULA 7
#----------------

##Este script apresenta o desafio da aula 7 sobre gráficos e o codigo utilizado para resolver o desafio. O código foi adaptado do Tutorial da Aula 7.

#----------------------------------------------------

##Gráfico de média com desvio padrão com arrows

# Construa um gráfico de pontos, contendo cada uma das variáveis (comprimento da sépala, largura da sépala, comprimento da pétala, largura da pétala) no eixo x e os valores médios no eixo y. Inclua as barras de erro (representando o desvio padrão em torno da média). Salve o gráfico em png no diretório /figs

#Importando e visualizando o dado
data(iris)
iris
head(iris)
tail(iris)
summary(iris)


# criando vetor com as medias (eixo y)
media = apply(iris[,-5], 2, mean)
media

# criando vetor com o desvio padrao
sd = apply(iris[,-5], 2, sd)
sd

#plotando o grafico
plot(x=1:4, media, las=2, bty='l', ylim=c(0, 8), pch=19, xaxt='n',
     xlab="Medidas", ylab="Média (cm)")
axis(1, at=1:4, labels=c("Comp.sépala", "Larg.sépala", "Comp.pétala", "Larg.pétala"))
arrows(x0=1:4,
       y0=media+sd,
       y1=media-sd, angle=90, length=0.05, code=3)

##Exportando o gráfico com as funções png() e dev.off()
# a funcao png cria o arquivo para depois gravar os comandos graficos dentro do arquivo, daqui pra frente você não vai mais ver o gráfico na tela plot
png("figs/figura03", res=300, width=2400, height=1200)#estabelecendo tamanho proporcional a resolucao

#plotando o grafico
plot(x=1:4, media, las=2, bty='l', ylim=c(0, 8), pch=19, xaxt='n',
     xlab="Medidas", ylab="Média (cm)")
axis(1, at=1:4, labels=c("Comp.sépala", "Larg.sépala", "Comp.pétala", "Larg.pétala"))
arrows(x0=1:4,
       y0=media+sd,
       y1=media-sd, angle=90, length=0.05, code=3)

#finalizando o gráfico e gerando o arquivo figura03
dev.off()

