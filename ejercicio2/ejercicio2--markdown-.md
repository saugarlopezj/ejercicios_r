Distribuciones teóricas
================
Javier Saugar López







  - [1 Aleatoriedad](#aleatoriedad)
  - [2 Ajuste de los valores generados por R a la funciones de densidad
    de las distribuciones
    teóricas](#ajuste-de-los-valores-generados-por-r-a-la-funciones-de-densidad-de-las-distribuciones-teóricas)
      - [2.1 Ejercicios con las ditribuciones uniforme, ji-cuadrado, F
        Snhedecor-Fisher y
        T-Student.](#ejercicios-con-las-ditribuciones-uniforme-ji-cuadrado-f-snhedecor-fisher-y-t-student.)
  - [3 Ley débil de los grandes
    números](#ley-débil-de-los-grandes-números)
  - [4 Teorema central del límite](#teorema-central-del-límite)
      - [4.1 Con la T de Student](#con-la-t-de-student)
      - [4.2 Con F de Snedecor](#con-f-de-snedecor)
      - [4.3 Con ji-cuadrado](#con-ji-cuadrado)

# 1 Aleatoriedad

En este apartado se específica como funciona la elección de valores
mediante el “pseudoazar” de R. Además se incluye la creación de valores
provenientes de distintas distribuciones entre ellas:

  - La distibución normal

  - La distribución ji-cuadrado

  - La distribución T-student

  - La distribución F de Snedecor

<!-- end list -->

``` r
sample(1:10,10)# el vector indica el número de valores máximo que puede aparecer (en este caso del 1 al 10) y el el número indica cuantos valores vamos a sacar en este caso sacaremos 10 valores de los 10 posibles.
```

    ##  [1]  1  2  4  5  7 10  3  8  9  6

``` r
sample(1:10,10,replace=T)# el replace indica que los números ya obtenidos en la muestra pueden volver a aperecer de forma que puede haber muestra con más de 8 o 9 etc.
```

    ##  [1]  5  3  9 10  4  1  3  6  8  4

``` r
set.seed(1)#sirve para fijar el valor obtenido mediante pseudoazar 

rbinom(10,20,0.5)# el 10 es el número de veces que repito el experimento, el 20 equivale al número de ensayo que tiene el experimento y el 0.5 al valor de la probabilidad de obtener el resultado que quiero. 
```

    ##  [1]  9  9 10 13  8 13 14 11 11  7

``` r
runif(10,1,10)#distribución uniforme 10 número de una uniforme entre el 1 y el 10. El primer valor es el n, segundo el mínimo y el tercero el máximo.
```

    ##  [1] 2.853771 2.589011 7.183206 4.456933 7.928573 5.479293 7.458567 9.927155
    ##  [9] 4.420317 7.997007

``` r
rnorm(10,0,1)#normal de 10 números con una media de 0 y una desviación típica de 1
```

    ##  [1]  1.51178117  0.38984324 -0.62124058 -2.21469989  1.12493092 -0.04493361
    ##  [7] -0.01619026  0.94383621  0.82122120  0.59390132

``` r
rt(10, 1)#t de student: 10 numeros con un gl.
```

    ##  [1]   1.0361299 -16.8103321  -0.4313783   1.7676553   0.4838340  -2.4255767
    ##  [7]  -0.5887315   0.5853582  -0.9979839   1.2117985

``` r
rf(10,1,1)# F de Fiseher-Snedecor: 10 números con grados de libertad de gl1= 1 y gl2=2
```

    ##  [1] 2.744272e+00 4.268687e-01 1.088565e+03 4.334863e-02 4.997548e+00
    ##  [6] 2.616337e+02 1.072740e-01 1.019701e-01 3.635963e+00 2.209186e-01

``` r
rchisq(10,1)# 10 números con grados de libertad de gl=1
```

    ##  [1] 0.01141421 1.46837663 1.11414328 2.15372386 0.40586775 2.46808812
    ##  [7] 1.42015794 0.02852595 0.03414423 0.28342896

# 2 Ajuste de los valores generados por R a la funciones de densidad de las distribuciones teóricas

En este apartado se crea la función de densidad a través de la función
normal con parámetros x(valores típificados), \(\mu\) (la media) y
\(\sigma\) (la desviación típica).

``` r
normal<- function(x, mu, sigma){
  n<-(1/sqrt((2*pi*sigma^2)))*exp(-(x-mu)^2/(2*sigma^2))
  
}



x<- seq(-10, 10, 0.1)#genero valores en x

y.ecuacion<-normal(x,0,4)#aplico la funcion
```

``` r
plot(y.ecuacion~x, type="l")#creo el gráfico
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico normal -1.png" alt="Gráfico distribución normal"  />

<p class="caption">

Gráfico distribución normal

</p>

</div>

``` r
##Ahora con la función de rnorm

y.generador<- rnorm(10000, 0, 4)

y.generador<- y.generador[y.generador>-10 & y.generador<10]

d<-density(y.generador)#para que se vea mejor que un histograma
```

``` r
plot(d)
lines(y.ecuacion~x, col="green")#superpongo lo obtenido con la formula
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico comparativo rnorm con función normal-1.png" alt="gráfico comparativo rnorm con función normal"  />

<p class="caption">

gráfico comparativo rnorm con función normal

</p>

</div>

``` r
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
```

``` r
d <- density(y.generador.10)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/rnorm aumentando el número gráficos-1.png" alt="rnorm aumentanto número comparando con función normall"  />

<p class="caption">

rnorm aumentanto número comparando con función normall

</p>

</div>

``` r
d <- density(y.generador.100)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/rnorm aumentando el número gráficos-2.png" alt="rnorm aumentanto número comparando con función normall"  />

<p class="caption">

rnorm aumentanto número comparando con función normall

</p>

</div>

``` r
d <- density(y.generador.1000)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/rnorm aumentando el número gráficos-3.png" alt="rnorm aumentanto número comparando con función normall"  />

<p class="caption">

rnorm aumentanto número comparando con función normall

</p>

</div>

``` r
d <- density(y.generador.10000)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/rnorm aumentando el número gráficos-4.png" alt="rnorm aumentanto número comparando con función normall"  />

<p class="caption">

rnorm aumentanto número comparando con función normall

</p>

</div>

## 2.1 Ejercicios con las ditribuciones uniforme, ji-cuadrado, F Snhedecor-Fisher y T-Student.

Aquí aplicamos la misma estrategia pero para las distribuciones que
aparecen en el título del apartado.

### 2.1.1 Uniforme

``` r
uniforme<-function(x,a,b){
 return(ifelse(x>a & x<b, 1/(b-a), 0))
  
}

x<- seq(-2,12,0.1)
y.ecuacion<-uniforme(x,0,10)#aplico funcion


plot(y.ecuacion~x, type="l")#creo el gráfico
```

![](ejercicio2--Rmarkdown-_files/figure-gfm/uniforme-1.png)<!-- -->

``` r
y.generador.10000<- runif(10000,0,10)
```

``` r
d <- density(y.generador.10000)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico uniforme-1.png" alt="gráfico distribución uniforme"  />

<p class="caption">

gráfico distribución uniforme

</p>

</div>

### 2.1.2 T-student

``` r
t <- function(x, gl){
a <- gamma((gl+1)/2) / (sqrt(gl*pi)*gamma(gl/2))
x <- a*(1+(x**2)/gl)**(-(gl+1)/2)
return(x)
}

x<- seq(-4,4,0.1)

y.ecuacion<-t(x,10)#aplico funcion

plot(y.ecuacion~x, type="l")#creo el gráfico
```

![](ejercicio2--Rmarkdown-_files/figure-gfm/T-student-1.png)<!-- -->

``` r
y.generador<- rt(10000,10)
```

``` r
d <- density(y.generador)
plot(d, main="")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico T-student-1.png" alt="gráfico distribución T-student"  />

<p class="caption">

gráfico distribución T-student

</p>

</div>

### 2.1.3 Ji-cuadrado

``` r
X <- function(x, gl){
a <- (1 / ( 2**(gl/2)*gamma(gl/2)))*x**((gl/2)-1)*exp(-x/2)
return(ifelse(x>0,a,0))
}

x<- seq(0, 4, 0.01)
y.ecuacion<-X(x,1)
plot(y.ecuacion~x,type="l")
```

![](ejercicio2--Rmarkdown-_files/figure-gfm/ji-cuadrado-1.png)<!-- -->

``` r
y.generador<-rchisq(1000,1)
```

``` r
d<- density(y.generador)
plot(d,main= "")
lines(y.ecuacion~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico ji-cuadrado-1.png" alt="gráfico ji-cuadrado"  />

<p class="caption">

gráfico ji-cuadrado

</p>

</div>

### 2.1.4 F de Snedecor

``` r
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
```

![](ejercicio2--Rmarkdown-_files/figure-gfm/F%20de%20Snedecor-1.png)<!-- -->

``` r
y.generador<-rf(n = 10000,df1 = 10,df2 = 10)

y.generador<- recorta(y.generador, 0, 10)
d<- density(y.generador)
```

``` r
plot(d,main="")
lines(y.ecuacion~x,col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico F de Snedecor-1.png" alt="gráfico de F de Snedecor"  />

<p class="caption">

gráfico de F de Snedecor

</p>

</div>

# 3 Ley débil de los grandes números

Se demuestra aquí como va aumentando el número de n es decir de ensayos
de forma que el valor experimental se va pareciendo más y más a \(\pi\).

``` r
set.seed(26)
rbinom(1,10,0.5)
```

    ## [1] 2

``` r
rbinom(2,10,0.5)
```

    ## [1] 4 7

``` r
rbinom(4,10,0.5)
```

    ## [1] 6 4 5 6

``` r
x<-vector()
for(n in 1:1000){
  x[n]<- mean(rbinom(n,10,0.5))
}
```

``` r
plot(x, type="l", xlab = "número de repeticiones (n)")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico ejemplo grandes números-1.png" alt="gráfico ley débil grandes números"  />

<p class="caption">

gráfico ley débil grandes números

</p>

</div>

Y aquí tenemos el ejercicio que es en vez con la binomial usa la
polinomial con la misma función rbinom siendo un dado con \(\pi\) = 1/6

``` r
x<- vector()
for(n in 1:1000){
  x[n]<- mean(rbinom(n,5,1/6))
}
```

``` r
plot(x, type="l", xlab= "número de repeticiones (n)")
abline(h= (5*(1/6)), col= "green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráfico ejercio ley grandes números-1.png" alt="ejercio ley grandes números con dado"  />

<p class="caption">

ejercio ley grandes números con dado

</p>

</div>

# 4 Teorema central del límite

En este apartado se demuestra como al aumentar tamaño de la muestra
(indirectamente al aumentar los grados de libertad) las distribuciónes
teóricas tienden a tener una forma más simétrica como ocurre con la
distribución normal. A continuación se muestra con las distribuciones
teóricas de T Studentes, Ji-cuadrado y F de Snedecor.

## 4.1 Con la T de Student

``` r
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
```

``` r
plot(density(y.tgl1), main="1 gl")
lines(y.normal~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/grafico T-student teorema central del limite-1.png" alt="T-student al aumentar gl"  />

<p class="caption">

T-student al aumentar gl

</p>

</div>

``` r
plot(density(y.tgl5), main="5 gl")
lines(y.normal~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/grafico T-student teorema central del limite-2.png" alt="T-student al aumentar gl"  />

<p class="caption">

T-student al aumentar gl

</p>

</div>

``` r
plot(density(y.tgl10), main="10 gl")
lines(y.normal~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/grafico T-student teorema central del limite-3.png" alt="T-student al aumentar gl"  />

<p class="caption">

T-student al aumentar gl

</p>

</div>

``` r
plot(density(y.tgl100), main="1000 gl")
lines(y.normal~x, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/grafico T-student teorema central del limite-4.png" alt="T-student al aumentar gl"  />

<p class="caption">

T-student al aumentar gl

</p>

</div>

## 4.2 Con F de Snedecor

``` r
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
```

``` r
plot(density(y.fgl1), main="gl1 y gl2 = 1")
lines(y.teo1~x.teo1, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/graficos F teorema central del límite, -1.png" alt="Aumento simetría al aumentar gl"  />

<p class="caption">

Aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.fgl5), main="gl1 y gl2 = 5")
lines(y.teo5~x.teo2, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/graficos F teorema central del límite, -2.png" alt="Aumento simetría al aumentar gl"  />

<p class="caption">

Aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.fgl10), main="gl1 y gl2 = 10")
lines(y.teo10~x.teo3, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/graficos F teorema central del límite, -3.png" alt="Aumento simetría al aumentar gl"  />

<p class="caption">

Aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.fgl100), main="gl1 y gl2 = 100")
lines(y.teo100~x.teo4, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/graficos F teorema central del límite, -4.png" alt="Aumento simetría al aumentar gl"  />

<p class="caption">

Aumento simetría al aumentar gl

</p>

</div>

## 4.3 Con ji-cuadrado

``` r
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
```

``` r
plot(density(y.chisqgl1), main="gl = 1")
lines(y.teo1~x.teo1, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráficos teorema central del limite con ji-cuadrado, -1.png" alt="aumento simetría al aumentar gl"  />

<p class="caption">

aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.chisqgl5), main="gl = 5")
lines(y.teo5~x.teo2, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráficos teorema central del limite con ji-cuadrado, -2.png" alt="aumento simetría al aumentar gl"  />

<p class="caption">

aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.chisqgl10), main="gl = 10")
lines(y.teo10~x.teo3, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráficos teorema central del limite con ji-cuadrado, -3.png" alt="aumento simetría al aumentar gl"  />

<p class="caption">

aumento simetría al aumentar gl

</p>

</div>

``` r
plot(density(y.chisqgl100z), main="gl = 100")
lines(y.teo100~x.teo4, col="green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio2--Rmarkdown-_files/figure-gfm/gráficos teorema central del limite con ji-cuadrado, -4.png" alt="aumento simetría al aumentar gl"  />

<p class="caption">

aumento simetría al aumentar gl

</p>

</div>
