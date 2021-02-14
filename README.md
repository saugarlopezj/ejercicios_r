# ejercicios_r
En este repositorio se incluirán ejercicios con R. Se irá añadiendo cada ejercicio en un fichero independiente.

## Ejercicios

### 2.Ejercicio 2:  

#### 2.1.Descripción
En este ejercicio se muestra que R puede simular distribuciones teóricas a través de las funciones como rnorm, rchi, runif o rt. También se comprueba la ley débil de los grandes números que consiste en que a medida que aumentamos el número de ensayos el valor obtenido se partece cada vez más al esperado, en este caso se ha realizado con la binomial por lo que el valor obtenido se parecerá más a pi . También se demuestra el Teorema del límite central como al aumentar el número de valores las distribución se parece más a la distribución normal. En este caso se ha hecho con la distribución ji-cuadrado, con la T de Student y con la F de Snedecor viendo que al aumentar los grados de libertad la forma de la distribución se vuelve más simétrica.

#### 2.2.Librería
N/A

#### 2.3.Ejecución
La ejecución del mismo se encuentra en este enlace. [Ver markdown](ejercicio2--markdown-.md)

#### 2.4.Codigo
El código utilizado en el markdown en este enlace. [Ver código](ejercicio2.R)


###  3.Ejercicio 3: 

#### 3.1.Descripción
En este ejercicio se desarrolla como funciona el juego Martingala a través de una función. Martingala es un juego de azar en el que se juega en una ruleta donde hay 37 números, de ellos 36 tiene un color (18 un color y otros 18 otro). Apostando a un color en particular. La característica principal de este juego es que cuando se pierde la apuesta, además de descontar ese dinero del dinero inicial se dobla la apuesta. A parte de realizar una función se comprueba a través de simulaciones que ocurriría si aumentamos la probabilidad de acertar (siendo de 18/37), el dinero de inicio (la bolsa) o el límite máximo de dinero apostado. Por otro lado, también compara distintos códigos y funciones entre sí comprobando cual es más eficiente en según que situación utilizando la función system.time(). 


#### 3.2.Librerías
Se utilizó la librería "psych" al comparar la función describe() con mean() y "ggplot2" para realizar los gráficos de las simulaciones de martingala.
 

#### 3.3.Ejecución
El resultado final se encuentra en este enlace. [Ver markdown](ejercicio3-markdown-.md)

#### 3.4.Código
Se puede consultar el código utilizado aquí. [Ver código](ejercicio3.R)


