#instalação do pacote ggplot2 
install.packages('ggplot2')

# carregamento do pacote 
library(ggplot2)

#carregamento do dataset iris 
names(iris)

#visualização das variáveis e categorias que compõem o dataset
# O comando summary já nos permite explorar dados como a mínima, primeiro quartio, mediana, terceiro quartio e máxima do dataset

summary(iris)

#inserção do dataset numa variável de mesmo nome
iris = iris

# análise exploratória :estrutura dos dados 
str(iris)

#separação das variáveis para facilitar as análises e construções futuras 

sepalas.comprimento <- iris$Sepal.Length
sepalas.largura <- iris$Sepal.Width

petalas.comprimento <- iris$Petal.Length
petalas.largura <- iris$Petal.Width

especies <- iris$Species



#definindo especies como um fator 
especies <- factor(especies)


#separando as variaveis para a correlacao
  #comprimentos

sd(iris$Petal.Length)
sd(iris$Petal.Width) 
sd(iris$Sepal.Length) 
sd(iris$Sepal.Width)

#setosa
min(iris$Petal.Length[iris$Species=="setosa"])
max(iris$Petal.Length[iris$Species=="setosa"])
mean(iris$Petal.Length[iris$Species=="setosa"])
sd(iris$Petal.Length[iris$Species=="setosa"]) 
median(iris$Petal.Length[iris$Species=="setosa"])

min(iris$Sepal.Length[iris$Species=="setosa"])
max(iris$Sepal.Length[iris$Species=="setosa"])
mean(iris$Sepal.Length[iris$Species=="setosa"])
sd(iris$Sepal.Length[iris$Species=="setosa"]) 
median(iris$Sepal.Length[iris$Species=="setosa"])


min(iris$Petal.Width[iris$Species=="setosa"])
max(iris$Petal.Width[iris$Species=="setosa"])
mean(iris$Petal.Width[iris$Species=="setosa"])
sd(iris$Petal.Width[iris$Species=="setosa"])  
median(iris$Petal.Width[iris$Species=="setosa"])

min(iris$Sepal.Width[iris$Species=="setosa"])
max(iris$Sepal.Width[iris$Species=="setosa"])
mean(iris$Sepal.Width[iris$Species=="setosa"])
sd(iris$Sepal.Width[iris$Species=="setosa"]) 
median(iris$Sepal.Width[iris$Species=="setosa"])

#versicolor

min(iris$Petal.Length[iris$Species=="versicolor"])
max(iris$Petal.Length[iris$Species=="versicolor"])
mean(iris$Petal.Length[iris$Species=="versicolor"])
sd(iris$Petal.Length[iris$Species=="versicolor"])
median(iris$Petal.Length[iris$Species=="versicolor"])

min(iris$Sepal.Length[iris$Species=="versicolor"])
max(iris$Sepal.Length[iris$Species=="versicolor"])
mean(iris$Sepal.Length[iris$Species=="versicolor"])
sd(iris$Sepal.Length[iris$Species=="versicolor"]) 
median(iris$Sepal.Length[iris$Species=="versicolor"])

min(iris$Petal.Width[iris$Species=="versicolor"])
max(iris$Petal.Width[iris$Species=="versicolor"])
mean(iris$Petal.Width[iris$Species=="versicolor"])
sd(iris$Petal.Width[iris$Species=="versicolor"]) 
median(iris$Petal.Width[iris$Species=="versicolor"])

min(iris$Sepal.Width[iris$Species=="versicolor"])
max(iris$Sepal.Width[iris$Species=="versicolor"])
mean(iris$Sepal.Width[iris$Species=="versicolor"])
sd(iris$Sepal.Width[iris$Species=="versicolor"])
median(iris$Sepal.Width[iris$Species=="versicolor"])


#virginica

min(iris$Petal.Length[iris$Species=="virginica"])
max(iris$Petal.Length[iris$Species=="virginica"])
mean(iris$Petal.Length[iris$Species=="virginica"])
sd(iris$Petal.Length[iris$Species=="virginica"])
median(iris$Petal.Length[iris$Species=="virginica"])

min(iris$Sepal.Length[iris$Species=="virginica"])
max(iris$Sepal.Length[iris$Species=="virginica"])
mean(iris$Sepal.Length[iris$Species=="virginica"])
sd(iris$Sepal.Length[iris$Species=="virginica"]) 
median(iris$Sepal.Length[iris$Species=="virginica"])

min(iris$Petal.Width[iris$Species=="virginica"])
max(iris$Petal.Width[iris$Species=="virginica"])
mean(iris$Petal.Width[iris$Species=="virginica"])
sd(iris$Petal.Width[iris$Species=="virginica"]) 
median(iris$Petal.Width[iris$Species=="virginica"])

min(iris$Sepal.Width[iris$Species=="virginica"])
max(iris$Sepal.Width[iris$Species=="virginica"])
mean(iris$Sepal.Width[iris$Species=="virginica"])
sd(iris$Sepal.Width[iris$Species=="virginica"])
median(iris$Sepal.Width[iris$Species=="virginica"])






#--------------------------------------------------------------------#




comp.petalas.setosa = iris$Petal.Length[iris$Species=="setosa"]
comp.petalas.versicolor =iris$Petal.Length[iris$Species=="versicolor"]
comp.petalas.virginica = iris$Petal.Length[iris$Species=="virginica"] 

comp.sepalas.setosa=iris$Sepal.Length[iris$Species=="setosa"]
comp.sepalas.versicolor = iris$Sepal.Length[iris$Species=="versicolor"]
comp.sepalas.virginica = iris$Sepal.Length [iris$Species=="virginica"] 

  #larguras

largura.petalas.setosa = iris$Petal.Width[iris$Species=="setosa"]
largura.petalas.versicolor = iris$Petal.Width[iris$Species=="versicolor"]
largura.petalas.virginica = iris$Petal.Width[iris$Species=="virginica"]

largura.sepalas.setosa = iris$Sepal.Width[iris$Species=="setosa"]
largura.sepalas.versicolor = iris$Sepal.Width[iris$Species=="versicolor"]
largura.sepalas.virginica = iris$Sepal.Width[iris$Species=="virginica"]


#regressao linear do comprimento para cada especie 


regressao.linear.setosa <- lm(comp.petalas.setosa ~ comp.sepalas.setosa)
summary(regressao.linear.setosa)

z =plot(comp.sepalas.setosa, comp.petalas.setosa, main="Regressão linear, pétalas e sépalas em Iris setosa", ylab= "pétalas", xlab="sépalas",  pch=19, col='#009874')
abline(regressao.linear.setosa)


regressao.linear.versicolor <- lm(comp.petalas.versicolor ~ comp.sepalas.versicolor)
summary(regressao.linear.versicolor) 

y=plot(comp.sepalas.versicolor, comp.petalas.versicolor, main="Regressão linear, pétalas e sépalas em Iris versicolor", ylab="pétalas", xlab="sépalas", pch=18, col='#004356')
abline(regressao.linear.versicolor)


regressao.linear.virginica <- lm(comp.petalas.virginica ~ comp.sepalas.virginica)
x=plot(comp.sepalas.virginica, comp.petalas.virginica, main=" Regressão linear, comprimento das pétalas e sépalas de Iris versicolor", ylab="pétalas", xlab="sépalas", pch=15, col="00783")
abline(regressao.linear.virginica)





# regressao comprimento e largura das petalas 

     #petalas
reg.comp.larg.setosa<-lm(comp.petalas.setosa ~ largura.petalas.setosa)
summary(reg.comp.larg.setosa)
plot(largura.petalas.setosa, comp.petalas.setosa,  xlab="largura das pétalas", ylab= "comprimento das pétalas", main= "Correlação :comprimento e largura de pétalas em Iris setosa", pch=20, col=2)
abline(reg.comp.larg.setosa)

reg.comp.larg.versicolor <- lm(comp.petalas.versicolor ~ largura.petalas.versicolor)
summary(reg.comp.larg.versicolor)
plot(largura.petalas.versicolor, comp.petalas.versicolor, xlab="largura das pétalas", ylab="comprimento das pétalas", main="correlação: comprimento e largura de pétalas em Iris versicolor", pch=20, col=3)
abline(reg.comp.larg.versicolor)


reg.comp.larg.virginica <-lm(comp.petalas.virginica ~largura.petalas.virginica)
summary(reg.comp.larg.virginica)
plot(largura.petalas.virginica, comp.petalas.virginica, xlab="largura das pétalas", ylab= "comprimento das pétalas", main= "Correlação :comprimento e largura de pétalas em Iris virginica", pch=20, col=4)
abline(reg.comp.larg.virginica)

     #sepalas

novaregressao <- lm (comp.sepalas.setosa ~ largura.sepalas.setosa)
summary(novaregressao)
plot(largura.sepalas.setosa, comp.sepalas.setosa, xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris setosa",pch=20,col=2)
abline(novaregressao) 

novaregressao2 <- lm (comp.sepalas.versicolor ~ largura.sepalas.versicolor)
summary(novaregressao2)
plot(largura.sepalas.versicolor, comp.sepalas.versicolor,xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris versicolor",pch=20,col=3)
abline(novaregressao2) 

novaregressao3 <- lm (comp.sepalas.virginica ~ largura.sepalas.virginica)
summary(novaregressao3)
plot(largura.sepalas.virginica, comp.sepalas.virginica, xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris virginica",pch=20,col=4)
abline(novaregressao3)





#regressao linear da largura para cada especie 

regressao.linear.larg.setosa <- lm (largura.petalas.setosa ~ largura.sepalas.setosa)
summary(regressao.linear.larg.setosa)
plot(largura.sepalas.setosa , largura.petalas.setosa, main=" Regressão: largura em pétalas e sépalas de Iris setosa", ylab="pétalas", xlab="sépalas")
abline(regressao.linear.larg.setosa)




#criando boxplot de comprimento e largura para as pétalas
boxplot(iris$Petal.Length ~ especies, ylab= "Comprimento", xlab="Espécies", main="Comprimento das pétalas entre as espécies", col=2:4)
boxplot(iris$Petal.Width ~ especies, ylab= "Largura", xlab="Espécies", main="Largura das pétalas entre as espécies",col=2:4)

#criando boxplot de comprimento e largura para as sépalas

boxplot(iris$Sepal.Length ~ especies, ylab="comprimento", xlab="Espécies", main="Comprimento das sépalas", col=2:4)
boxplot(iris$Sepal.Width ~ especies, ylab="Largura", xlab="Espécies", main="Largura das sépalas ", col= 2:4) 











