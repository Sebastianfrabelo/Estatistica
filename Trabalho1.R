#Trabalho 1: Análise Exploratória e Estatística Descritiva

# Entrando com o conjunto de dados do arquivo a ser escolhido na janela
dados = read.table(file=file.choose(),  dec = ",", header=T, sep="\t")

#habilita chamadas de variáveis individuais
attach(dados)

##################### Variável qualitativa Tipo

#função para calcular moda
#unique retorna um vetor sem elementos repetido
#which.max calcula o elemento que mais se repete
mode <- function(x) {
   elem <- unique(x)
   elem[which.max(tabulate(match(x, elem)))]
}

mode(Tipo)
table(Tipo)
prop.table(table(Tipo))

# Gráfico de Barras
#barplot(table(Tipo), ylim=c(0,25), space=.8, width=c(.2,.2),col=c("green","red"),legend=c("56%","44%"),main="Proporção de funcionario por estado civil",
#xlab="Estado Civil", ylab="Proporção de funcionários")

#Gréfico de Pareto (deixar bonito ainda)
library(qcc)
pareto.chart(table(Tipo))

######################## Variáveis quantitativas:

	##############	Discreta
#Ainda que o numero de parafusos nao seja uma variavel qualitativa, é conveniente
#calcular sua moda pois os elementos se repetem frequentemente 
mode(Número_de_parafusos_utilizados)

# Módulo que determinas as Medidas de Posição e dispersão#
medpd <- function(x) {
 s <- c(min(x), mean(x),max(x),
	quantile(x,0.25),median(x),quantile(x,0.75),
	var(x),sd(x),100*sd(x)/mean(x))
 #nomeia as variaveis
 names(s) <- c("mínimo","média","máximo",
	"primeiro quartil","mediana","terceiro quartil",
	"variância","desvio padrão","coeficiente de variação (%)")
 return(s)
 }

medpd(Número_de_parafusos_utilizados)

#range da variavel quantitativa é pequeno -> variancia pequena
table(Número_de_parafusos_utilizados)
prop.table(table(Número_de_parafusos_utilizados))

# Gráfico de Barras
barplot(table(Número_de_parafusos_utilizados), ylim=c(0,25), space=.8, width=c(.2,.2),col=c("green","red"),legend=c("56%","44%"),main="Proporção de ",
	xlab="Estado Civil", ylab="Proporção de funcionários")

boxplot(Número_de_parafusos_utilizados,col="yellow",ylab="Salario")
#boxplot(Número_de_parafusos_utilizados~Tipo, xlab="Salario",col=c("green","yellow"))
boxplot(Número_de_parafusos_utilizados~Tipo, xlab="Grau de Instrução", names=c("1 Grau", "2 Grau", "Superior"),
ylab="Salario",main="Diagrama de caixas dos salarios dos func. por grau de instrução")

	############# Contínua

medpd(Custo_de_fabricação_.em_reais.)

hist(Custo_de_fabricação_.em_reais.)
## Tabela de Frequencia
TDF=hist(Custo_de_fabricação_.em_reais., breaks =c(130,140,150,160,170,180), right = F,plot=F)
fabs=TDF$counts    #Frequencia absoluta
fr=fabs/length(Custo_de_fabricação_.em_reais.) # Frequencia relativa
saida=cbind(fabs,fr)
dimnames(saida)=list(c("130|-140","140|-150","150|-160","160|-170","170|-180"),c("f", "fr"))
saida

boxplot(Custo_de_fabricação_.em_reais.,col="yellow",ylab="Salario")
boxplot(Custo_de_fabricação_.em_reais.~Tipo, xlab="Salario",col=c("green","yellow"))
boxplot(Custo_de_fabricação_.em_reais.~Número_de_parafusos_utilizados, xlab="Salario",col=c("green","yellow"))

##  Análise bivariada (quantitativo) 	
plot(Custo_de_fabricação_.em_reais., Número_de_parafusos_utilizados, xlab = "Sa", ylab ="N parafusos", pch = 20)
cor(Custo_de_fabricação_.em_reais., Número_de_parafusos_utilizados)
lines(lowess(Custo_de_fabricação_.em_reais., Número_de_parafusos_utilizados), col = "blue")
