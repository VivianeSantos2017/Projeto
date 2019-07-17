#-------------------
##Exercicios Aula 4
#-------------------


# ao final subir o script e as tabelas organizadas

##Exercicio2

#lendo a planilha organizada
ex2=read.csv("Data/exercicio_02.csv", header=TRUE, sep = ";")
ex2
str(ex2)

#tirando a media em 1 passo
mean(ex2[c(22:42),4])

#ou em 2 passos
Qchilled = ex2[c(22:42),4]
Qchilled
mean(Qchilled)


##Exercicio3

#lendo a planilha exercicio (salva somente a aba lista)
ex3=read.csv("Data/exercicio_03.csv", header=TRUE, sep = ";")
ex3
str(ex3)
#somando as linhas de todas as colunas, exceto a coluna de especie, temos a soma dos individuos para cada parcela
colSums(ex3[,-1])#em 1 passo
parcelas=ex3[,-1]#em 2 passos
parcelas
colSums(parcelas)


##Exercicio 4

#lendo o arquivo com a funcao read.delim: provide arguments to read.table appropriate for CSV and tab-delimited files exported from spreadsheets in English-speaking locales; read.delim2 are appropriate for use in those locales where the comma is used for the decimal point and (for read.csv2) for spreadsheets which use semicolons to separate fields. If you have a tab-delimited file containing empty fields be sure to use sep = "\t"; It is quite common for a file exported from a spreadsheet to have all trailing empty fields (and their separators) omitted. To read such files set fill = TRUE.

ex4=read.delim("Data/0012594-190621201848488.csv", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")#read.delim 
ex4
head(ex4)
str(ex4)
colnames(ex4)#vendo os nomes das colunas para identificar as de especies e coordenadas
gbif4 = ex4[,c(10, 17, 18)]#criando objeto com as colunas especies e coordenadas
gbif4
write.csv(gbif4, "Data/exercicio_04.csv", row.names = FALSE)#salvando arquivo com os dados
