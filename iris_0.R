#instala��o do pacote ggplot2 
install.packages('ggplot2')

# carregamento do pacote 
library(ggplot2)

#carregamento do dataset iris 
names(iris)

#visualiza��o das vari�veis e categorias que comp�em o dataset
# O comando summary j� nos permite explorar dados como a m�nima, primeiro quartio, mediana, terceiro quartio e m�xima do dataset

summary(iris)

#inser��o do dataset numa vari�vel de mesmo nome
iris = iris

# an�lise explorat�ria :estrutura dos dados 
str(iris)

#separa��o das vari�veis para facilitar as an�lises e constru��es futuras 

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

z =plot(comp.sepalas.setosa, comp.petalas.setosa, main="Regress�o linear, p�talas e s�palas em Iris setosa", ylab= "p�talas", xlab="s�palas",  pch=19, col='#009874')
abline(regressao.linear.setosa)


regressao.linear.versicolor <- lm(comp.petalas.versicolor ~ comp.sepalas.versicolor)
summary(regressao.linear.versicolor) 

y=plot(comp.sepalas.versicolor, comp.petalas.versicolor, main="Regress�o linear, p�talas e s�palas em Iris versicolor", ylab="p�talas", xlab="s�palas", pch=18, col='#004356')
abline(regressao.linear.versicolor)


regressao.linear.virginica <- lm(comp.petalas.virginica ~ comp.sepalas.virginica)
x=plot(comp.sepalas.virginica, comp.petalas.virginica, main=" Regress�o linear, comprimento das p�talas e s�palas de Iris versicolor", ylab="p�talas", xlab="s�palas", pch=15, col="00783")
abline(regressao.linear.virginica)





# regressao comprimento e largura das petalas 

     #petalas
reg.comp.larg.setosa<-lm(comp.petalas.setosa ~ largura.petalas.setosa)
summary(reg.comp.larg.setosa)
plot(largura.petalas.setosa, comp.petalas.setosa,  xlab="largura das p�talas", ylab= "comprimento das p�talas", main= "Correla��o :comprimento e largura de p�talas em Iris setosa", pch=20, col=2)
abline(reg.comp.larg.setosa)

reg.comp.larg.versicolor <- lm(comp.petalas.versicolor ~ largura.petalas.versicolor)
summary(reg.comp.larg.versicolor)
plot(largura.petalas.versicolor, comp.petalas.versicolor, xlab="largura das p�talas", ylab="comprimento das p�talas", main="correla��o: comprimento e largura de p�talas em Iris versicolor", pch=20, col=3)
abline(reg.comp.larg.versicolor)


reg.comp.larg.virginica <-lm(comp.petalas.virginica ~largura.petalas.virginica)
summary(reg.comp.larg.virginica)
plot(largura.petalas.virginica, comp.petalas.virginica, xlab="largura das p�talas", ylab= "comprimento das p�talas", main= "Correla��o :comprimento e largura de p�talas em Iris virginica", pch=20, col=4)
abline(reg.comp.larg.virginica)

     #sepalas

novaregressao <- lm (comp.sepalas.setosa ~ largura.sepalas.setosa)
summary(novaregressao)
plot(largura.sepalas.setosa, comp.sepalas.setosa, xlab =" Largura das s�palas", ylab= "comprimento das s�palas", main="Correla��o: comrpimento e largura das s�palas em Iris setosa",pch=20,col=2)
abline(novaregressao) 

novaregressao2 <- lm (comp.sepalas.versicolor ~ largura.sepalas.versicolor)
summary(novaregressao2)
plot(largura.sepalas.versicolor, comp.sepalas.versicolor,xlab =" Largura das s�palas", ylab= "comprimento das s�palas", main="Correla��o: comrpimento e largura das s�palas em Iris versicolor",pch=20,col=3)
abline(novaregressao2) 

novaregressao3 <- lm (comp.sepalas.virginica ~ largura.sepalas.virginica)
summary(novaregressao3)
plot(largura.sepalas.virginica, comp.sepalas.virginica, xlab =" Largura das s�palas", ylab= "comprimento das s�palas", main="Correla��o: comrpimento e largura das s�palas em Iris virginica",pch=20,col=4)
abline(novaregressao3)





#regressao linear da largura para cada especie 

regressao.linear.larg.setosa <- lm (largura.petalas.setosa ~ largura.sepalas.setosa)
summary(regressao.linear.larg.setosa)
plot(largura.sepalas.setosa , largura.petalas.setosa, main=" Regress�o: largura em p�talas e s�palas de Iris setosa", ylab="p�talas", xlab="s�palas")
abline(regressao.linear.larg.setosa)




#criando boxplot de comprimento e largura para as p�talas
boxplot(iris$Petal.Length ~ especies, ylab= "Comprimento", xlab="Esp�cies", main="Comprimento das p�talas entre as esp�cies", col=2:4)
boxplot(iris$Petal.Width ~ especies, ylab= "Largura", xlab="Esp�cies", main="Largura das p�talas entre as esp�cies",col=2:4)

#criando boxplot de comprimento e largura para as s�palas

boxplot(iris$Sepal.Length ~ especies, ylab="comprimento", xlab="Esp�cies", main="Comprimento das s�palas", col=2:4)
boxplot(iris$Sepal.Width ~ especies, ylab="Largura", xlab="Esp�cies", main="Largura das s�palas ", col= 2:4) 











