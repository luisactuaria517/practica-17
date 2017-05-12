# practica-17
##Ejercicio bajar acciones de netflix 
##precio cierre 2016
##ajustar 4 modelos de suavizado exponencial simple (alpha = .2, .4, .6, .8)
##graficandolos en una sola grafica y
##con la grafica decidir cual de los 4 es el mejor ajuste que tiene
##una vez que hayan decidido graficamente el mejor ajuste
##aplicar la evaluacion de los ajustes y utilizar las medias MAE y RMSE extrayendolas
##de tal forma que creen un vector que compare estas medidas y este vector indique que alpha es
##la mejor
##identificar tendencia, estacionariedad y ciclos en la serie de tiempo e interpretar estos componentes

install.packages("fpp")
require(fpp)

net<-read.csv(file.choose())

tnet<- ts(net, frequency =252, start=2016)
tnet


mode1<-ses(tnet, alpha=.2, initial="simple", h=8)
mode2<-ses(tnet, alpha=.4, initial="simple", h=8)
mode3<-ses(tnet, alpha=.6, initial="simple", h=8)
mode4<-ses(tnet, alpha=.8, initial="simple", h=8)

plot(mode1, ylab = "Año", main = "Acciones netflix", type="o")
lines(fitted(mode1), col="brown", type="o")
lines(fitted(mode2), col="red", type="o")
lines(fitted(mode3), col="green", type="o")
lines(fitted(mode4), col="blue", type="o")

###mae y rmse para el modelo 1
aa<- tnet- mode1$fitted ##valores ajustados
aa   ##residuos

#analizamos los residules de manera manual
mean(abs(aa))##MAE
sqrt(mean(aa^2))#RMSE
#analizamos con accuracy
evalm1<-accuracy(mode1)
evalm1

###mae y rmse para el modelo 2
aa2<- tnet- mode2$fitted ##
aa2
mean(abs(aa2))##MAE
sqrt(mean(aa2^2))#RMSE

evalm2<-accuracy(mode2)
evalm2

###mae y rmse para el modelo 3
aa3<- tnet- mode3$fitted ##
aa3
mean(abs(aa3))##MAE
sqrt(mean(aa3^2))#RMSE

evalm3<-accuracy(mode3)
evalm3

###mae y rmse para el modelo 4
aa4<- tnet- mode4$fitted ##
aa4
mean(abs(aa4))##MAE
sqrt(mean(aa4^2))#RMSE

evalm4<-accuracy(mode4)
evalm4

##eligiriamos el modelo 4 por q graficamentes es el mas cercano 
##el mae es la medida de residuos, y es el mas cercano a 0

maed1<-evalm1[,3]
maed2<-evalm2[,3]
maed3<-evalm3[,3]
maed4<-evalm4[,3]
mae<-c(maed1, maed2,maed3, maed4)
mae<-min(mae)


if(mae = maed1){
  print("El MAE minimo es el del modelo 1")
}


else if (mae =maed2){
  print("El MAE minimo es el del modelo 2")
} 

if else (mae =maed3) {
    print("El MAE minimo es el del modelo 3")
} 

  else (mae =maed4) 
  print("El MAE minimo es el del modelo 4")


rmse1<-evalm1[,2]
rmse2<-evalm2[,2]
rmse3<-evalm3[,2]
rmse4<-evalm4[,2]
rmse<-c(rmse1, rmse2,rmse3, rmse4)
rmse<-min(rmse)

if(rmse = rmse1){
  print("El RMSE minimo es el del modelo 1")
}


else if (rmse = rmse2){
  print("El RMSE minimo es el del modelo 2")
} 

if else (rmse=rmse3) {
  print("El RMSE minimo es el del modelo 3")
} 
else (rmse = rmse4) 
print("El RMSE minimo es el del modelo 4")
 
plot(tnet)
##no tiene tendencia, posible ciclo en el ultimo trimestre y no presenta estacionalidad.
