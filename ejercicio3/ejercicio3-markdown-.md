Martingala y eficiencia en el código
================
Javier Saugar López









  - [1 Martingala](#martingala)
      - [1.1 Función](#función)
      - [1.2 Modificando el valor
        probabilidad](#modificando-el-valor-probabilidad)
      - [1.3 Modificando el valor de
        bolsa](#modificando-el-valor-de-bolsa)
      - [1.4 Cambiando el límite de la
        mesa](#cambiando-el-límite-de-la-mesa)
  - [2 Rendimiento a la hora de
    progamar](#rendimiento-a-la-hora-de-progamar)
      - [2.1 Establecer longitud vector previamente frente a no
        establecerla](#establecer-longitud-vector-previamente-frente-a-no-establecerla)
      - [2.2 Utilizar lenguaje lógico frente a utilizar
        if](#utilizar-lenguaje-lógico-frente-a-utilizar-if)
      - [2.3 ¿Qué bucle es más eficiente?](#qué-bucle-es-más-eficiente)
      - [2.4 ¿Aply o bucle for?](#aply-o-bucle-for)
      - [2.5 Función mean frente función describe del paquete
        psych](#función-mean-frente-función-describe-del-paquete-psych)

# 1 Martingala

En este apartado se desarrolla una función de martingala. Martingala
consiste en un juego de azar en el que existe 37 casillas todas con un
color salvo la casilla 0, teniendo la mitad rojo y la mitad azul. Si se
obtiene un número del color que se ha apostado se suma la apuesta al
dinero inicial. Si se pierde se resta la apuesta al dinero oficial y se
dobla la apuesta. Como se demuestra en este apartado no es rentable ya
que como se desarrollará en uno de los apartados la martingala depende
de la probabilidad del 18/37 por lo que es muy probable que se obtengan
pérdidas en vez de ganancias

## 1.1 Función

``` r
martingala<- function(bolsa= 100, apuesta=10, limite= 50, probabilidad = 18/37){
  ganado<- bolsa
  while(apuesta<=limite| apuesta<=bolsa){
    
    azar<- rbinom(1, 1, probabilidad)
    
    if(azar== 0){
      ganado<- ganado-apuesta  
      apuesta<-2*apuesta
    }else{
      ganado<- apuesta+ganado
      apuesta<- apuesta
    }
    
  }
  res<- ((ganado-bolsa)/bolsa)*100
  return(res)
}
```

Como se puede ver se seguirá jugando siempre y cuando la apuesta se
igual o menor al límite establecido (máximo apostado) o si la apuesta es
menor o igual a la bolsa (dinero inicial). El resultado está expresado
en ganancia porcentual (o perdida).

``` r
set.seed(1)
martingala(100, 10, 50, 18/37)
```

    ## [1] 250

Es un ejemplo de un solo ensayo para que se pueda observar el resultado
obtenido por la función, fijando una semilla.

A continuación, se muestra que ocurriría si aplicamos martingala 1000
veces creando un valor medio y un intervalo de confianza mostrando lo
poco rentable que es este juego. Como se puede observar, se mantienen
los parámetros fijos de la función bolsa, apuesta, límite y
probabilidad.

``` r
data1<- data.frame(vector(mode= "numeric", length = 1000))

for(i in 1:1000){
  data1[i,1]<- martingala(100, 10, 50, 18/37)
}


media<- mean(data1[,1])
desviacion<- sd(data1[,1])/sqrt(1000)
intsup<- media+1.96*desviacion
intinf<- media-1.96*desviacion

sprintf("Media: %.2f- Desviación:%.2f- Intervalo superior:%.2f- Intervalo inferior:%.2f", media, desviacion, intsup, intinf)
```

    ## [1] "Media: -10.97- Desviación:3.94- Intervalo superior:-3.24- Intervalo inferior:-18.70"

Ahora vamos a variar algunos de estos parámetros para ver que pasa.

## 1.2 Modificando el valor probabilidad

En este caso lo que se ha hecho es hallar un valor medio e intervalo de
1000 casos como en el caso anterior pero esta vez lo hemos hecho por
cada valor de probabilidad establecido, en concreto valores de 0.2 a 0.6
con un intervalo de 0.01 entre cada valor.

``` r
probabilidad<- seq(0.2, 0.6, 0.01)
data2<- matrix(vector(mode = "numeric", length = 41000), ncol = length(probabilidad), nrow= 1000)
for(i in 1:length(probabilidad)){
 for(j in 1:1000){
  data2[j,i]<- martingala(100, 10, 50, probabilidad[i])
 }
}


for(i in 1 : length(probabilidad)){
  for(j in 1 : 1000){
data2[j,i]<-martingala(100, 10, 50, probabilidad[i])
  }
}  

data2<-as.data.frame(data2)


for(i in 1 : length(probabilidad)){
  names(data2)[i]<- sprintf("probabilidad- %.2f", probabilidad[i])
}



media<- vector(mode="numeric", length = length(probabilidad))
intinf<- vector(mode="numeric", length = length(probabilidad))
intsup<-vector(mode="numeric", length = length(probabilidad))

for(i in 1: length(probabilidad)){
 media[i]<- mean(data2[[i]])
  intsup[i]<- mean(data2[[i]])+1.96* (sd(data2[[i]])/sqrt(nrow(data2)))
  intinf[i]<- mean(data2[[i]])-1.96* (sd(data2[[i]])/sqrt(nrow(data2)))
}

data3<-data.frame(probabilidad,media,intinf, intsup)
```

Como se puede ver en el gráfico justo de bajo el valor medio aumenta a
medida que aumenta la probabilidad es una relación lineal directa. La
franja roja es el intervalo de confianza obtenido para cada valor medio.

``` r
require(ggplot2)
```

    ## Loading required package: ggplot2

``` r
ggplot(data3, aes(x= probabilidad, y= media))+
  geom_ribbon(aes(ymin=intinf, ymax=intsup), fill= "red")+
  geom_line()+
  geom_hline(yintercept = 0, color="blue")
```

<div class="figure" style="text-align: center">

<img src="ejercicio3-Rmarkdown-_files/figure-gfm/gráfico probabilidad -1.jpeg" alt="Ganancias medias conforme cambia la probabilidad"  />

<p class="caption">

Ganancias medias conforme cambia la probabilidad

</p>

</div>

## 1.3 Modificando el valor de bolsa

En este caso se ha hecho lo mismo que el anterior pero variando el
parámetro de bolsa. El hecho de que la función martingala este en
valores de ganancia porcentual nos hace no equivocarnos al interpretar
el gráfico ya que, como es evidente, cuanto mayor sea el dinero inicial
mayor será el dinero final aunque halla pérdidas por eso al expresarlo
en ganancias y pérdidas el resultado final es más fácil de interpretar
el gráfico.

``` r
bolsa<- seq(10,200, 5)

tabla4<- matrix( vector(mode= "numeric", length = length(bolsa)*1000), nrow= 1000, ncol= length(bolsa))

for(i in 1: length(bolsa)){
  for(j in 1: 1000){
    tabla4[j,i]<- martingala(bolsa[i])
  }
}

tabla4<- as.data.frame(tabla4)

for(i in 1 : length(bolsa)){
  names(tabla4)[i]<- sprintf("dinero inicio- %3d", bolsa[i])
}



media<- vector(mode="numeric", length = length(bolsa))
intinf<- vector(mode="numeric", length = length(bolsa))
intsup<-vector(mode="numeric", length = length(bolsa))
for(i in 1: length(bolsa)){
 media[i]<- mean(tabla4[[i]])
 intsup[i]<- mean(tabla4[[i]])+1.96* (sd(tabla4[[i]])/sqrt(nrow(tabla4)))
 intinf[i]<- mean(tabla4[[i]])-1.96* (sd(tabla4[[i]])/sqrt(nrow(tabla4)))
}

tabla5<-data.frame(bolsa,media,intsup,intinf)
```

Como se ha expresado previamente, el gráfico permite ver que aumentar el
dinero inicial, la bolsa, no hace que aumenta la ganancia media.

``` r
ggplot(tabla5,aes(x=bolsa, y=media))+
  geom_ribbon(aes(ymax=intsup, ymin=intinf), fill="red")+
  geom_line()+
  geom_hline(yintercept=0, color= "blue")
```

<div class="figure" style="text-align: center">

<img src="ejercicio3-Rmarkdown-_files/figure-gfm/gráfico bolsa -1.jpeg" alt="Ganancias medias conforme cambia el valor de la bolsa"  />

<p class="caption">

Ganancias medias conforme cambia el valor de la bolsa

</p>

</div>

## 1.4 Cambiando el límite de la mesa

Y por último tenemos el ejemplo de que pasaría si modificamos el valor
del límite establecido para apostar. Como se puede ver en el gráfico no
existe un aumento en el valor medio más bien se queda estancado entorno
a pérdidas cercanas a 0, pérdidas pequeñas.

``` r
limite<- seq(50, 500, 10)

tabla6<- matrix( vector(mode= "numeric", length= length(limite)*1000), nrow = 1000, ncol = length(limite))

for(i in 1: length(limite)){
  for(j in 1: 1000){
    tabla6[j,i]<- martingala(limite = limite[i])
  }
}

tabla6<- as.data.frame(tabla6)

for(i in 1 : length(limite)){
  names(tabla6)[i]<- sprintf("limite establecido de mesa- %3d", limite[i])
}


media<- vector(mode="numeric", length = length(limite))
intinf<- vector(mode="numeric", length = length(limite))
intsup<-vector(mode="numeric", length = length(limite))
for(i in 1: length(limite)){
 media[i]<- mean(tabla6[[i]])
  intsup[i]<- mean(tabla6[[i]])+ 1.96*sd(tabla6[[i]]/sqrt(nrow(tabla6)))
  intinf[i]<- mean(tabla6[[i]])- 1.96*sd(tabla6[[i]]/sqrt(nrow(tabla6)))
}

tabla7<- data.frame(limite,media,intsup,intinf)
```

``` r
ggplot(tabla7, aes(x= limite, y=media))+
  geom_ribbon(aes(ymax=intsup,ymin=intinf), fill="red")+
  geom_line()+
  geom_hline(yintercept = 0, color= "green")
```

<div class="figure" style="text-align: center">

<img src="ejercicio3-Rmarkdown-_files/figure-gfm/gráfico límite -1.jpeg" alt="Ganancias medias conforme cambia el valor del límite"  />

<p class="caption">

Ganancias medias conforme cambia el valor del límite

</p>

</div>

# 2 Rendimiento a la hora de progamar

En este apartado del documento nos centraremos en que funciones, bucles
o estrategias serán más eficientes a la hora de solventar los problemas
planteados o ejecutar una simulación con el menor coste posible.

## 2.1 Establecer longitud vector previamente frente a no establecerla

Como se puede ver si establecemos previamente la longitud del vector el
tiempo que tarda en rellenarnos el vector será menor ya que tiene de
primeras ese espacio reservado en la memoria y no será necesario que lo
vaya creando cada vez que rellena un número en el vector como pasa en el
caso de que no establezcamos la longitud previamente.

``` r
k <- 10000000
v1<- vector()
v2<- vector(length = k)
system.time({
for(i in 1:k){
  v1[i]<- i
}         
})
```

    ##    user  system elapsed 
    ##    2.32    0.06    2.39

``` r
system.time({
for(i in 1:k){
  v2[i]<- i
}
})
```

    ##    user  system elapsed 
    ##    0.61    0.00    0.61

## 2.2 Utilizar lenguaje lógico frente a utilizar if

Como se puede ver es muchos más rápido utilizar el lenguaje lógico
frente a utilizar if ya que no sólo requerimos de if si no además de un
bucle.

``` r
k <- 10000000
v3<- 1:k
system.time({for(i in 1:length(v3)){
  if(v3[i]<30000){
    v3[i]<-TRUE
    }else{
    v3[i]<- FALSE
    } 
  } 
v3<-as.logical(v3)})
```

    ##    user  system elapsed 
    ##    1.59    0.02    1.61

``` r
system.time({v3<-v3<30000})
```

    ##    user  system elapsed 
    ##    0.03    0.00    0.03

## 2.3 ¿Qué bucle es más eficiente?

Como se puede ver el más lento es while.

``` r
k <- 10000000
suma<- 0

system.time({for(i in 1:k){
  suma<- suma+i
}})
```

    ##    user  system elapsed 
    ##    0.27    0.00    0.27

``` r
system.time({i<- 1
while(i<= k){
  suma<- suma + i
  i<- 1 +i
}})
```

    ##    user  system elapsed 
    ##    0.56    0.01    0.57

``` r
system.time({
repeat{
 suma<- suma + i
 i<- i +1
 if(i> k) break
 }
 })
```

    ##    user  system elapsed 
    ##    0.01    0.00    0.02

## 2.4 ¿Aply o bucle for?

Como se puede ver es más eficiente utilizar un bucle for a utilizar una
función apply.

``` r
m1<- matrix(1:10000000, nrow= 1000000, ncol = 10)

system.time({apply(m1,2,mean)})
```

    ##    user  system elapsed 
    ##    0.04    0.02    0.06

``` r
system.time({for(i in 1:10){
  print(mean(m1[,i]))
}})
```

    ## [1] 500000.5
    ## [1] 1500001
    ## [1] 2500001
    ## [1] 3500001
    ## [1] 4500001
    ## [1] 5500001
    ## [1] 6500001
    ## [1] 7500001
    ## [1] 8500001
    ## [1] 9500001

    ##    user  system elapsed 
    ##    0.02    0.01    0.03

## 2.5 Función mean frente función describe del paquete psych

Como se puede ver la función mean es más eficiente si sólo nos interesa
saber la media claro.

``` r
system.time({mean(1:10000000)})
```

    ##    user  system elapsed 
    ##    0.05    0.00    0.05

``` r
library("psych")
```

    ## 
    ## Attaching package: 'psych'

    ## The following objects are masked from 'package:ggplot2':
    ## 
    ##     %+%, alpha

``` r
system.time({
  describe(1:10000000)})
```

    ##    user  system elapsed 
    ##    2.83    0.62    3.45
