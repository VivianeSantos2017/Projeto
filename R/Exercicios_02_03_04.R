#-------------------
##Exercicios Aula 4
#-------------------

# Este script apresenta os codigos com os resultados dos exercícios da aula 4 sobre limpeza e manipulacao de dados

##Exercicio2: Dados de um experimento sobre a tolerância ao frio das espécies de gramíneas _Echinochloa crus-galli_.

# Organize os dados em uma única tabela
# Exporte para um arquivo de texto
# Salve os metadados
# Importe para o R
# Calcule a média de _'uptake'_ para _'Quebec chilled'_

#lendo a planilha ja organizada e disponivel na pasta 'Data'
ex2=read.csv("Data/exercicio_02.csv", header=TRUE, sep = ";")
ex2
str(ex2)

#tirando a media em 1 passo
mean(ex2[c(22:42),4])

#ou em 2 passos
Qchilled = ex2[c(22:42),4]
Qchilled
mean(Qchilled)


##Exercicio3: Contagem de árvores em parcelas de 1 hectare na Ilha de Barro Colorado.

# Salve os dados em uma planilha
# Exporte para um arquivo de texto
# Calcule o número de indivíduos por parcela

#lendo a planilha exercicio disponivel na pasta 'Data' (salva somente a aba lista)
ex3=read.csv("Data/exercicio_03.csv", header=TRUE, sep = ";")
ex3
str(ex3)
#somando as linhas de todas as colunas, exceto a coluna de especie, temos a soma dos individuos para cada parcela
colSums(ex3[,-1])#em 1 passo
parcelas=ex3[,-1]#em 2 passos
parcelas
colSums(parcelas)

#teria tb a funcao 'apply', mas tem que colocar apply(nome do objeto, 1 ou 2 - sendo 1 para linha e 2 para coluna, operacao que quer fazer). Nesse caso seria apply (parcelas, 2, sum)

##Exercicio 4: Dados do GBIF com 320 linhas e 45 colunas

# Leia o arquivo "0012594-190621201848488.csv"
# selecione apenas as colunas com o nome da espécie, longitude e latitude
# salve um arquivo com esses dados
# Dica:  Use o `help` para descobrir outras funções para importar os dados.

#lendo o arquivo com a funcao read.delim: provide arguments to read.table appropriate for CSV and tab-delimited files exported from spreadsheets in English-speaking locales; read.delim2 are appropriate for use in those locales where the comma is used for the decimal point and (for read.csv2) for spreadsheets which use semicolons to separate fields. If you have a tab-delimited file containing empty fields be sure to use sep = "\t"; It is quite common for a file exported from a spreadsheet to have all trailing empty fields (and their separators) omitted. To read such files set fill = TRUE.

ex4=read.delim("Data/0012594-190621201848488.csv", header = TRUE, sep = "\t", quote = "\"", dec = ".", fill = TRUE, comment.char = "")#read.delim 
ex4
head(ex4)
str(ex4)
colnames(ex4)#vendo os nomes das colunas para identificar as de especies e coordenadas
gbif4 = ex4[,c(10, 17, 18)]#criando objeto com as colunas especies e coordenadas; o ideal seria concatenar ja com os nomes das colunas para tornar mais estavel o comando e evitar erro
gbif4
write.csv(gbif4, "Data/exercicio_04.csv", row.names = FALSE)#salvando arquivo com os dados
