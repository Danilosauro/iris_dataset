## obtencao dos dados
names(iris)

tinytex::install_tinytex()
#atribuicao dos dados a uma variavel de mesmo nome 
iris = iris

# resumo dos dados, para obtencao de valores de estatistica descritiva 
summary(iris)

# tratamento: separacao dos dados do dataset (referentes a comprimento) nas repctivas variaveis para consequente analise exploratoria 
comp.petalas.setosa = iris$Petal.Length[iris$Species=="setosa"]
comp.petalas.versicolor =iris$Petal.Length[iris$Species=="versicolor"]
comp.petalas.virginica = iris$Petal.Length[iris$Species=="virginica"] 

comp.sepalas.setosa=iris$Sepal.Length[iris$Species=="setosa"]
comp.sepalas.versicolor = iris$Sepal.Length[iris$Species=="versicolor"]
comp.sepalas.virginica = iris$Sepal.Length [iris$Species=="virginica"] 



#################################    ANÁLISES DE PETALAS E SEPALAS POR ESPECIE ########################################


#criacao dos graficos referentes a comprimento de petalas e sepalas por especies e analise dos valores de correlacao de pearson
regressao.linear.comp.pet.sep.setosa=(lm(comp.sepalas.setosa ~ comp.petalas.setosa))
plot(comp.petalas.setosa, comp.sepalas.setosa, xlab = "comprimento das pétalas", ylab= "comprimento das sépalas", main =" Correlação comprimento em Iris setosa", col=2, pch=20)
abline(regressao.linear.comp.pet.sep.setosa)
cor(comp.petalas.setosa, comp.sepalas.setosa)


regressao.linear.comp.pet.sep.versicolor=(lm(comp.sepalas.versicolor ~ comp.petalas.versicolor))
plot(comp.petalas.versicolor, comp.sepalas.versicolor, xlab="comprimento das pétalas", ylab = "Comprimento das sépalas", main= "Correlação comprimento em Iris versicolor", col=3, pch=20)
abline(regressao.linear.comp.pet.sep.versicolor)
cor(comp.petalas.versicolor, comp.sepalas.versicolor) 

regressao.linear.comp.pet.sep.virginica=(lm(comp.sepalas.virginica ~ comp.petalas.virginica))
plot(comp.petalas.virginica, comp.sepalas.virginica, xlab="comprimento das pétalas", ylab="comprimento das sépalas", main=" Correlação comprimento em Iris virginica", col=4, pch=20)
abline(regressao.linear.comp.pet.sep.virginica)
cor(comp.petalas.virginica, comp.sepalas.virginica) 


#tratamento: separacao dos dados do dataset (referentes a comprimento) nas repctivas variaveis para consequente analise exploratoria

largura.petalas.setosa = iris$Petal.Width[iris$Species=="setosa"]
largura.petalas.versicolor = iris$Petal.Width[iris$Species=="versicolor"]
largura.petalas.virginica = iris$Petal.Width[iris$Species=="virginica"]

largura.sepalas.setosa = iris$Sepal.Width[iris$Species=="setosa"]
largura.sepalas.versicolor = iris$Sepal.Width[iris$Species=="versicolor"]
largura.sepalas.virginica = iris$Sepal.Width[iris$Species=="virginica"]

#criacao dos graficos referentes a largura de petalas e sepalas por especie e analise dos valores de correlacao de pearson 

regressao.linear.larg.pet.sep.setosa = (lm( largura.sepalas.setosa ~ largura.petalas.setosa))
plot(largura.petalas.setosa, largura.sepalas.setosa, xlab="largura das pétalas", ylab = "largura das sépalas", main="Correlação largura em Iris setosa", col=2, pch=20)
abline(regressao.linear.larg.pet.sep.setosa)
cor(largura.petalas.setosa, largura.sepalas.setosa) 


regressao.linear.larg.pet.sep.versicolor=(lm(largura.sepalas.versicolor ~  largura.petalas.versicolor))
plot(largura.petalas.versicolor, largura.sepalas.versicolor, xlab="largura das pétalas", ylab ="largura das sépalas", main="Correlação largura em Iris versicolor", col=3, pch=20)
abline(regressao.linear.larg.pet.sep.versicolor)
cor(largura.petalas.versicolor, largura.sepalas.versicolor)


regressao.linear.larg.pet.sep.virginica=(lm(largura.sepalas.virginica ~ largura.petalas.virginica))
plot(largura.petalas.virginica, largura.sepalas.virginica, xlab="largura das pétalas", ylab="largura das sépalas", main ="Correlação largura em iris virginica", col=4, pch=20)
abline(regressao.linear.comp.pet.sep.virginica)
cor(largura.petalas.virginica, largura.sepalas.virginica)  



########################################### ANÁLISE ENTRE COMPRIMENTO E LARGURA DE PÉTALAS E SÉPALAS ENTRE SI, POR ESPÉCIE ###############################



# análise de regressao e construcao dos graficos para comprimento e largura de petalas em cada especie 
reg.comp.larg.setosa<-lm(comp.petalas.setosa ~ largura.petalas.setosa)
summary(reg.comp.larg.setosa)
plot(largura.petalas.setosa, comp.petalas.setosa,  xlab="largura das pétalas", ylab= "comprimento das pétalas", main= "Correlação :comprimento e largura de pétalas em Iris setosa", pch=20, col=2)
abline(reg.comp.larg.setosa) 
cor(comp.petalas.setosa, largura.petalas.setosa)

reg.comp.larg.versicolor <- lm(comp.petalas.versicolor ~ largura.petalas.versicolor)
summary(reg.comp.larg.versicolor)
plot(largura.petalas.versicolor, comp.petalas.versicolor, xlab="largura das pétalas", ylab="comprimento das pétalas", main="correlação: comprimento e largura de pétalas em Iris versicolor", pch=20, col=3)
abline(reg.comp.larg.versicolor)
cor(comp.petalas.versicolor, largura.petalas.versicolor)

reg.comp.larg.virginica <-lm(comp.petalas.virginica ~largura.petalas.virginica)
summary(reg.comp.larg.virginica)
plot(largura.petalas.virginica, comp.petalas.virginica, xlab="largura das pétalas", ylab= "comprimento das pétalas", main= "Correlação :comprimento e largura de pétalas em Iris virginica", pch=20, col=4)
abline(reg.comp.larg.virginica)
cor(comp.petalas.virginica, largura.petalas.virginica)


# analise de regressao e construcao dos graficos para comprimento e largura de sepalas em cada especie 

novaregressao <- lm (comp.sepalas.setosa ~ largura.sepalas.setosa)
summary(novaregressao)
plot(largura.sepalas.setosa, comp.sepalas.setosa, xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris setosa",pch=20,col=2)
abline(novaregressao) 
cor (comp.sepalas.setosa, largura.sepalas.setosa)

novaregressao2 <- lm (comp.sepalas.versicolor ~ largura.sepalas.versicolor)
summary(novaregressao2)
plot(largura.sepalas.versicolor, comp.sepalas.versicolor,xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris versicolor",pch=20,col=3)
abline(novaregressao2) 
cor(comp.sepalas.versicolor, largura.sepalas.versicolor)

novaregressao3 <- lm (comp.sepalas.virginica ~ largura.sepalas.virginica)
summary(novaregressao3)
plot(largura.sepalas.virginica, comp.sepalas.virginica, xlab =" Largura das sépalas", ylab= "comprimento das sépalas", main="Correlação: comrpimento e largura das sépalas em Iris virginica",pch=20,col=4)
abline(novaregressao3)
cor(comp.sepalas.virginica, largura.sepalas.virginica)



