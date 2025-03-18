#Solucion Pregunta 1

x <- c(0,1)
fx <- c(0.68,0.32)

cbind(x,fx)

plot(x,fx,ylim = c(0,1),pch=16,col="red")
lines(x,fx,type="h",col="red")

mu <- sum(x*fx);mu

sigmassq <- sum((x-mu)^2*fx);sigmassq

#########


n <- 43
Y <- function(i)(sum(sample(x,n,prob=fx,replace=TRUE)))
Y(2)

m<-40000

encuestas <- sapply(1:m,Y)


fi <- table(encuestas)/m
Fi <- cumsum(fi)

cbind(2:29,fi,Fi)
barplot(table(sapply(1:m,Y)))

dbinom(13,43,0.32) #Respuesta


##########

resultados <- 0:43
fy <- dbinom(resultados,43,0.32);fy
Fy <- cumsum(fy)
cbind(resultados,fy,Fy)

mu <- sum(resultados*fy);mu
43*0.32 #Teoria

sigmassq <- sum(resultados-mu)^2*fy;sigmassq

plot(resultados,fy,ylim = c(0,0.2),pch=16,col="red")
lines(resultados,fy,type="h",col="red")

#Respuesta 
resultados <- 0:44
fy <- dbinom(resultados,44,0.32);fy
Fy <- cumsum(fy)
cbind(resultados,fy,Fy)

pbinom(16,44,0.32) 
plot(resultados,Fy,type="s",col="red")

############

dbinom(10,23,0.68)

23*0.68

resultados <-0:23
fy <- dbinom(resultados,23,0.68)

mu <- sum(resultados*fy) ; mu #Valor esperado

24*0.68*0.32
sum((resultados-mu)^2*fy) #Varianza

Fy <- pbinom(resultados,24,0.68)
plot(resultados,Fy,type="s",col="red")

qbinom(0.25,24,0.68) # Primer quantil

  
