sample(1:10,10)# el vector indica el número de valores máximo que puede aparecer (en este caso del 1 al 10) y el el número indica cuantos valores vamos a sacar en este caso sacaremos 10 valores de los 10 posibles.

sample(1:10,10,replace=T)# el replace indica que los números ya obtenidos en la muestra pueden volver a aperecer de forma que puede haber muestra con más de 8 o 9 etc.

set.seed(1)#sirve para fijar el valor obtenido mediante pseudoazar 

rbinom(10,20,0.5)# el 10 es el número de veces que repito el experimento, el 20 equivale al número de ensayo que tiene el experimento y el 0.5 al valor de la probabilidad de obtener el resultado que quiero. 


runif(10,1,10)#distribución uniforme 10 número de una uniforme entre el 1 y el 10. El primer valor es el n, segundo el mínimo y el tercero el máximo.

rnorm(10,0,1)#normal de 10 números con una media de 0 y una desviación típica de 1

rt(10, 1)#t de student: 10 numeros con un gl.

rf(10,1,1)# F de Fiseher-Snedecor: 10 números con grados de libertad de gl1= 1 y gl2=2

rchisq(10,1)# 10 números con grados de libertad de gl=1

##Función para representar la distribución normal

normal<- function(x, mu, sigma){
  n<-(1/sqrt((2*pi*sigma^2)))*exp(-(x-mu)^2/(2*sigma^2))
  
}



x<- seq(-10, 10, 0.1)#genero valores en x

y.ecuacionnrom<-normal(xnorm,0,4)#aplico la funcion

plot(y.ecuacion~x, type="l")#creo el gráfico

##Ahora con la función de rnorm

y.generador<- rnorm(10000, 0, 4)

y.generador<- y.generador[y.generador>-10 & y.generador<10]

d<-density(y.generador)#para que se vea mejor que un histograma

plot(d)
lines(y.ecuacion~x, col="green")#superpongo lo obtenido con la formula

## Comparación que a medida que aumentamos el número de valores el ajuste de la función rnorm se va adaptando al valor de función teórica de la distribución normal
y.generador.10<- rnorm(10,0,4)
y.generador.100<- rnorm(100,0,4)
y.generador.1000<- rnorm(1000,0,4)
y.generador.10000<- rnorm(10000,0,4)

recorta<- function(y,li,ls){
  res<- y[y>li & y<ls]
  return(res)
}

y.generador.10<- recorta(y.generador.10,-10,10)
y.generador.100<- recorta(y.generador.100,-10,10)
y.generador.1000<- recorta(y.generador.1000,-10,10)
y.generador.10000<- recorta(y.generador.10000,-10,10)

par(mfrow=c(2,2))

d <- density(y.generador.10)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.100)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.1000)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.10000)
plot(d, main="")
lines(y.ecuacion~x, col="green")



##Ejercicios con las ditribuciones uniforme, ji-cuadrado, F Snhedecor-Fisher y T-Student.

##Uniforme
uniforme<-function(x,a,b){
 return(ifelse(x>a & x<b, 1/(b-a), 0))
  
}

x<- seq(-2,12,0.1)
y.ecuacion<-uniforme(x,0,10)#aplico funcion


plot(y.ecuacion~x, type="l")#creo el gráfico

y.generador.10<- runif(10,0,10)
y.generador.100<- runif(100,0,10)
y.generador.1000<- runif(1000,0,10)
y.generador.10000<- runif(10000,0,10)



runif(10,0,10)

par(mfrow=c(2,2))

d <- density(y.generador.10)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.100)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.1000)
plot(d, main="")
lines(y.ecuacion~x, col="green")

d <- density(y.generador.10000)
plot(d, main="")
lines(y.ecuacion~x, col="green")

#T-student


t <- function(x, gl){
a <- gamma((gl+1)/2) / (sqrt(gl*pi)*gamma(gl/2))
x <- a*(1+(x**2)/gl)**(-(gl+1)/2)
return(x)
}

x<- seq(-4,4,0.1)

y.ecuacion<-t(x,10)#aplico funcion

plot(y.ecuacion~x, type="l")#creo el gráfico


y.generador<- rt(10000,10)


d <- density(y.generador)
plot(d, main="")
lines(y.ecuacion~x, col="green")




##ji-cuadrado

X <- function(x, gl){
a <- (1 / ( 2**(gl/2)*gamma(gl/2)))*x**((gl/2)-1)*exp(-x/2)
return(ifelse(x>0,a,0))
}

x<- seq(0, 4, 0.01)
y.ecuacion<-X(x,1)
plot(y.ecuacion~x,type="l")

y.generador<-rchisq(1000,1)

d<- density(y.generador)
plot(d,main= "")
lines(y.ecuacion~x, col="green")

##fsnedecor

f<- function(d1,d2,x){
  a<- 1/beta(d1/2,d2/2)
  b<- ((d1*x)/(d1*x+d2))^(d1/2)
  c<- (1- (d1*x)/(d1*x+d2) )^(d2/2)
  dist<-a*b*c*x^(-1)
  return(dist)
 
}

x<- seq(0,15,0.1)
y.ecuacion<- f(10,10,x)
plot(y.ecuacion~x, type="l")

y.generador<-rf(n = 10000,df1 = 10,df2 = 10)

y.generador<- recorta(y.generador, 0, 10)
d<- density(y.generador)
plot(d,main="")
lines(y.ecuacion~x,col="green")

##Ley débil de los grandes números


set.seed(26)
rbinom(1,10,0.5)

rbinom(2,10,0.5)

rbinom(4,10,0.5)

x<-vector()
for(n in 1:1000){
  x[n]<- mean(rbinom(n,10,0.5))
}

plot(x, type="l", xlab = "número de repeticiones (n)")


x<- vector()
for(n in 1:1000){
  x[n]<- mean(rbinom(n,5,1/6))
}



plot(x, type="l", xlab= "número de repeticiones (n)")
abline(h= (5*(1/6)), col= "green")


##Teorema central del límite
### con la t de student
x<- seq(-4,4,0.1)
y.normal<- normal(x, 0, 1)

y.tgl1<- rt(1000, 1)
y.tgl5<- rt(1000, 5)
y.tgl10<- rt(1000, 10)
y.tgl100<- rt(1000, 100)



y.tgl1 <- recorta(y.tgl1, -4, 4)
y.tgl5 <- recorta(y.tgl5, -4, 4)
y.tgl10 <- recorta(y.tgl10, -4, 4)
y.tgl100 <- recorta(y.tgl100, -4, 4)
par(mfrow=c(2,2))
plot(density(y.tgl1), main="1 gl")
lines(y.normal~x, col="green")
plot(density(y.tgl5), main="5 gl")
lines(y.normal~x, col="green")
plot(density(y.tgl10), main="10 gl")
lines(y.normal~x, col="green")
plot(density(y.tgl100), main="1000 gl")
lines(y.normal~x, col="green")

### con F Snedecor


y.fgl1<- rf(1000, 1, 1)
y.fgl5<- rf(1000, 5, 5)
y.fgl10<- rf(1000, 10, 10)
y.fgl100<- rf(1000, 100,100)

x.teo1<- seq(0,5,0.1)
x.teo2<- seq(0,5,0.1)
x.teo3<- seq(0,5,0.1)
x.teo4<- seq(0,5,0.1)

y.teo1<- f(x.teo1,1,1)
y.teo5<- f(x.teo2,5,5)
y.teo10<- f(x.teo3,10,10)
y.teo100<- f(x.teo4 ,100,100)

par(mfrow= c(2,2))
plot(density(y.fgl1), main="gl1 y gl2 = 1")
lines(y.teo1~x.teo1, col="green")

plot(density(y.fgl5), main="gl1 y gl2 = 5")
lines(y.teo5~x.teo2, col="green")

plot(density(y.fgl10), main="gl1 y gl2 = 10")
lines(y.teo10~x.teo3, col="green")

plot(density(y.fgl100), main="gl1 y gl2 = 100")
lines(y.teo100~x.teo4, col="green")

###con ji-cuadrado


y.chisqgl1<- rchisq(1000,1)
y.chisqgl5<- rchisq(1000,5)
y.chisqgl10<- rchisq(1000,10)
y.chisqgl100z<- rchisq(1000,100)


x.teo1<- seq(0,15,0.1)
x.teo2<- seq(0,30,0.1)
x.teo3<- seq(0,40,0.1)
x.teo4<- seq(0,180.1)

y.teo1<- X(x.teo1,1)
y.teo5<- X(x.teo2,5)
y.teo10<- X(x.teo3,10)
y.teo100<- X(x.teo4 ,100)


par(mfrow= c(2,2))
plot(density(y.chisqgl1), main="gl = 1")
lines(y.teo1~x.teo1, col="green")

plot(density(y.chisqgl5), main="gl = 5")
lines(y.teo5~x.teo2, col="green")

plot(density(y.chisqgl10), main="gl = 10")
lines(y.teo10~x.teo3, col="green")

plot(density(y.chisqgl100z), main="gl = 100")
lines(y.teo100~x.teo4, col="green")






