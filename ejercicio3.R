

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



set.seed(1)
martingala(100, 10, 50, 18/37)


data1<- data.frame(vector(mode= "numeric", length = 1000))

for(i in 1:1000){
  data1[i,1]<- martingala(100, 10, 50, 18/37)
}


media<- mean(data1[,1])
desviacion<- sd(data1[,1])/sqrt(1000)
intsup<- media+1.96*desviacion
intinf<- media-1.96*desviacion

sprintf("Media: %.2f- Desviación:%.2f- Intervalo superior:%.2f- Intervalo inferior:%.2f", media, desviacion, intsup, intinf)

## Modificando el valor probabilidad

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


require(ggplot2)
ggplot(data3, aes(x= probabilidad, y= media))+
  geom_errorbar(aes(ymin=intinf, ymax=intsup))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 0)

require(ggplot2)
ggplot(data3, aes(x= probabilidad, y= media))+
  geom_ribbon(aes(ymin=intinf, ymax=intsup), fill= "grey50")+
  geom_line()+
  geom_hline(yintercept = 0, color="blue")




## Modificando el valor de bolsa

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



ggplot(tabla5,aes(x=bolsa, y=media))+
  geom_ribbon(aes(ymax=intsup, ymin=intinf), fill="red")+
  geom_line()+
  geom_hline(yintercept=0, color= "blue")


## Cambiando el límite de la mesa

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


ggplot(tabla7, aes(x= limite, y=media))+
  geom_ribbon(aes(ymax=intsup,ymin=intinf), fill="red")+
  geom_line()+
  geom_hline(yintercept = 0, color= "green")


### Rendimiento a la hora de progamar

#### 6. 
k <- 10000000
v1<- vector()
v2<- vector(length = k)
system.time({
for(i in 1:k){
  v1[i]<- i
}         
})


system.time({
for(i in 1:k){
  v2[i]<- i
}
})


###7.
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




system.time({v3<-v3<30000})


###8.
k <- 10000000
v4<- vector("numeric")

system.time({for(i in 1:k){
  v4[i]<-i
}})


system.time({i<- 1
while(i<= k){
  v4[i]<-i
  i<- 1+i
}})


system.time({i<-0
repeat{
 i<-i+1
 v4[i]<- i
 if(i==k){
  break
 }
 }})

###9
m1<- matrix(1:10000000, nrow= 1000000, ncol = 10)

system.time({apply(m1,2,mean)})

system.time({for(i in 1:10){
  print(mean(m1[,i]))
}})


###10.
system.time({mean(1:10000000)})
library("psych")
system.time({
  describe(1:10000000)})


