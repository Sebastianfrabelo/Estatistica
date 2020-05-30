#Análise Exploratória e Estatística Descritiva

#Entrando com o conjunto de dados do arquivo a ser escolhido na janela
dados = read.table(file=file.choose(),  dec = ",", header=T, sep="\t")

#habilita chamadas de variáveis individuais
attach(dados)

##################### Variável qualitativa - Tipo

#função para calcular moda
#unique retorna um vetor sem elementos repetido
#which.max calcula o elemento que mais se repete
mode <- function(x) {
   elem <- unique(x)
   elem[which.max(tabulate(match(x, elem)))]
}

mode(Tipo)

#Tabela de frequência
fi=table(Tipo)
#acumulada
Fi=cumsum(table(Tipo))
#relativa
fri=prop.table(table(Tipo))
Fri=cumsum(prop.table(table(Tipo)))

tfrequencia=cbind(fi,Fi,fri,Fri)
tfrequencia
sum(fi)
sum(fri)

#Gráfico de Pareto (deixar bonito ainda)
#install.packages("qcc")
library(qcc)
pareto.chart(table(Tipo),ylab = "Frequência", xlab = "Tipos",ylab2 = "Porcentagem Acumulada",main = "Distribuição de frequência")

######################## Variáveis quantitativas:

# Módulo que determinas as Medidas de Posição e dispersão
medpd <- function(x) {
 s <- c(min(x), mean(x),max(x),
	quantile(x,0.25),median(x),quantile(x,0.75),
#range retorna os valores min e max
	diff(range(x)),quantile(x,0.75)-quantile(x,0.25),
	var(x),sd(x),100*sd(x)/mean(x))
 #nomeia as variaveis
 names(s) <- c("mínimo","média","máximo",
	"primeiro quartil","mediana","terceiro quartil",
	"Amplitude","dq",
	"variância","desvio padrão","coeficiente de variação (%)")
 return(s)
 }

#Medidas de posição e dispersão
medpd(Número_de_parafusos_utilizados)
medpd(Custo_de_fabricação_.em_reais.)

	##############	Discreta


#Tabela de frequência
f2i=table(Número_de_parafusos_utilizados)
#acumulada
F2i=cumsum(table(Número_de_parafusos_utilizados))
#relativa
f2ri=prop.table(table(Número_de_parafusos_utilizados))
F2ri=cumsum(prop.table(table(Número_de_parafusos_utilizados)))
t2frequencia=cbind(f2i,F2i,f2ri,F2ri)
t2frequencia
sum(f2i)
sum(f2ri)

#Ainda que o numero de parafusos nao seja uma variavel qualitativa, é conveniente
#calcular sua moda pois os elementos se repetem frequentemente 
mode(Número_de_parafusos_utilizados)

# Gráfico de Barras
barplot(table(Número_de_parafusos_utilizados), ylim=c(0,15), space=.5, width=c(.1,.1),
	main="Distribuição dos parafusos",col = c("orange"),xlab="Número de parafusos utilizados", ylab="Quantidade de equipamentos")
abline(h = 0)

boxplot(Número_de_parafusos_utilizados,col="green",ylab="Número de parafusos utilizados", main="Diagrama de caixa para o número de parafusos utilizados")
#Em função do tipo
boxplot(Número_de_parafusos_utilizados~Tipo, xlab="Tipo", names=c("A", "B", "C"),ylab="Número de parafusos utilizados",
	col=c("green","yellow","orange"), main="Número de parafusos utilizados por tipo de equipamento")

	############# Contínua

#Histograma com intervalos aberto fechado a esquerda e aberto à diretira
## Tabela de Frequencia e plotar gráfico
TDF=hist(Custo_de_fabricação_.em_reais.,breaks =c(130,135,140,145,150,155,160,165,170,175,180), right = F,plot=T,col="lightgreen",main="Histograma para o custo de fabricação",xlab="Custo de fabricação em reais",ylab="Frequência")
curve(dnorm(x,mean=150,sd=10,add=TRUE) 
fabs=TDF$counts    #Frequencia absoluta
fr=fabs/length(Custo_de_fabricação_.em_reais.) # Frequencia relativa
Fabs=cumsum(fabs)
Fr=cumsum(fr)
saida=cbind(fabs,Fabs,fr,Fr)
dimnames(saida)=list(c("130|-135","135|-140","140|-145","145|-150","150|-155","155|-160","160|-165","165|-170","170|-175","175|-180"),c("fabs", "Fabs","fr","Fr"))
saida
sum(fabs)
sum(fr)

#pontos medios
pm = c(132.5,137.5,142.5,147.5,152.5,157.5,162.5,167.5,172.5,177.5)
med=fabs*c	

hist(Custo_de_fabricação_.em_reais.,breaks =c(130,135,140,145,150,155,160,165,170,175,180), right = F,plot=T,col="lightgreen",main="Histograma para o custo de fabricação",xlab="Custo de fabricação em reais",ylab="Frequência")
curve(dnorm(x,mean=mean(Custo_de_fabricação_.em_reais.),sd=sd(Custo_de_fabricação_.em_reais.),add=TRUE)

boxplot(Custo_de_fabricação_.em_reais.,col="gold",ylab="Custo de fabricação (R$)",main="Diagrama de caixa para o custo de fabricação")
boxplot(Custo_de_fabricação_.em_reais.~Tipo, ylab = "Custo de fabricação (R$)",xlab="Tipo",col=c("green","yellow","orange"),main="Diagrama de caixa para custo de fabricação por tipo")
boxplot(Custo_de_fabricação_.em_reais.~Número_de_parafusos_utilizados, ylab = "Custo de fabricação (R$)",
	xlab="Número de parafusos utilizados",col=c("green","yellow"),main="Diagrama de caixa para custo de fabricação por tipo")

########################  Análise bivariada
plot(Número_de_parafusos_utilizados,Custo_de_fabricação_.em_reais., xlab = "Número de parafusos utilizados", ylab ="Custo de fabricação (R$)",
	pch = 20, main="Número de parafusos utilizados por Custo de fabricação")
lines(lowess( Número_de_parafusos_utilizados,Custo_de_fabricação_.em_reais.), col = "blue")


##  Análise bivariada Custo_de_fabricação_.em_reais. por Número_de_parafusos_utilizados
plot( Custo_de_fabricação_.em_reais.,Número_de_parafusos_utilizados, xlab = "Custo de fabricação (R$)", ylab ="Número de parafusos utilizados",
	pch = 20, main="Custo de fabricação por Número de parafusos utilizados")
lines(lowess( Custo_de_fabricação_.em_reais.,Número_de_parafusos_utilizados), col = "red")

#Covariância e coeficiente de correlação entre as variáveis quantitativas
cov(Número_de_parafusos_utilizados,Custo_de_fabricação_.em_reais.)
cor(Número_de_parafusos_utilizados,Custo_de_fabricação_.em_reais.)
