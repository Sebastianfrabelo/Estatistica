#Trabalho 1: Análise Exploratória e Estatística Descritiva

# Entrando com o conjunto de dados do arquivo a ser escolhido na janela
dados = read.table(file=file.choose(),  dec = ",", header=T, sep="\t")

#habilita chamadas de variáveis individuais
attach(dados)

#funação para calcular moda
#unique retorna um vetor sem elementos repetido
#which.max calcula o elemento que mais se repete
mode <- function(x) {
   elem <- unique(x)
   elem[which.max(tabulate(match(x, elem)))]
}

mode(Tipo)
#Ainda que o numero de parafusos nao seja uma variavel qualitativa, é conveniente
#calcular sua moda pois os elementos se repetem frequentemente 
mode(Número_de_parafusos_utilizados)
