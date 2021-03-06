install.packages("knitr")
install.packages("numDeriv")
source('~/Copy/Libro/NwRphSimult.R')
test()
install.packages("swirl")
library(swirl)
rm(list=ls())
source('~/Mega/Libro/DesemplEducSup.R')
?plot
plot(rnorm(5))
plot(rnorm(5),1)
plot(rnorm(5),rep(1,5)
)
?plot
desempl.educ.sup$prcnt[1]
trunc(desempl.educ.sup$prcnt[1])
source('~/Mega/Libro/DesemplEducSup.R')
tinf:tinf+5
tinf:(tinf+5)
source('~/Mega/Libro/DesemplEducSup.R')
source('~/Mega/Libro/DesemplEducSup.R')
?heat.colors
source('~/Mega/Libro/DesemplEducSup.R')
source('~/Mega/Libro/DesemplEducSup.R')
source('~/Mega/Libro/EjemploPoissons.R')
??factorial
factorial(3)
dpois(12,10)
(10^12/factorial(12))*exp(-10)
?dpois
source('~/Mega/Libro/EjemploPoissons.R')
arr <- array(1:20, c(4,5))
arr
arr%%10==0
arr[arr%%10==0,1]
arr[,1]%%10==0
arr[,2]%%10==0
arr[,3]%%10==0
arr[,4]%%10==0
arr[,5]%%10==0
Reduce("|", arr[,1]%%10==0)
Reduce("|", arr[,3]%%10==0)
Reduce("|", arr[,5]%%10==0)
T %in% arr[,5]%%10==0
T %in% arr[,2]%%10==0
arr[,2]%%10==0
T %in% (arr[,2]%%10==0)
T %in% (arr[,3]%%10==0)
a <- 1:3
b <- 4:6
sum(a*b)
a %*% b
c <- 8:10
sum(b*c)
m <- rbind(a,c)
m
m %*% b
install.packages("RColorBrewer")
library(RColorBrewer)
plot(1:8, pch=15, col=1:8)
plot(1:9, pch=15, col=1:9)
?par
plot(1:9, pch=15, cex=3 col=1:9)
plot(1:9, pch=15, cex=3, col=1:9)
palette(brewer.pal(15, "Set3"))
palette(brewer.pal(12, "Set3"))
plot(1:9, pch=15, cex=3 col=1:9)
plot(1:9, pch=15, cex=3, col=1:9)
?readline
?persp
?strsplit
aa <- function(a="uno", b="dos") {
print (a)
print (b)
}
aa()
aa(list(a="xx"))
aa('list(a="xx"))
??dotted
f <- function() x
formals(f)
formals(f) <- al <- alist(x = , y = 2+3, ... = )
f
a1
require(stats); require(graphics)
length(formals(lm))      # the number of formal arguments
names(formals(boxplot))  # formal arguments names
f <- function(x) a+b
formals(f) <- alist(a = , b = 3) # function(a, b = 3) a+b
f(2) # result = 5
f
formals(f)
alist(x = , y = 2+3, ... = )
f
f(alist(a=5, b=8))
f alist(a=5, b=8)
??"repeat"
?assign
# Ejamplo de graficacion 3D
# g <- function(x) x/2
g <- function(x) x
# h <- function(y) (1+3*y^2)/2
h <- function(y) y
f <- function(x,y) g(x)*h(y)
x <- seq(-1,1, length.out=16)
y <- seq(-1,1, length.out=16)
z <- outer(x,y,f)
titulo="Paraboloide hiperbólico"
x11()
theta=30
phi=10
repeat {
ss <- readline("SU-VAR>")
if (length(ss)==0) break
aa <- strsplit(ss, "=")
assign(aa[1],as.numeric(aa[2]))
persp(x,y,z,theta = theta, phi = phi, expand=0.7, col = "lightblue",
ltheta = 120, shade = 0.75, ticktype = "detailed",
xlab = "X", ylab = "Y", zlab = "z", main=titulo
) -> res # Se guarda la transformacion
}
class(aa)
aa
# Ejamplo de graficacion 3D
# g <- function(x) x/2
g <- function(x) x
# h <- function(y) (1+3*y^2)/2
h <- function(y) y
f <- function(x,y) g(x)*h(y)
x <- seq(-1,1, length.out=16)
y <- seq(-1,1, length.out=16)
z <- outer(x,y,f)
titulo="Paraboloide hiperbólico"
x11()
theta=30
phi=10
repeat {
ss <- readline("SU-VAR>")
if (length(ss)==0) break
aa <- strsplit(ss, "=")
assign(aa[[1]][1],as.numeric(aa[[1]][2]))
persp(x,y,z,theta = theta, phi = phi, expand=0.7, col = "lightblue",
ltheta = 120, shade = 0.75, ticktype = "detailed",
xlab = "X", ylab = "Y", zlab = "z", main=titulo
) -> res # Se guarda la transformacion
}
ss
??nchars
nchars(ss)
nchar(ss)
# SCRIPT
# -----------
rm(list=ls()) # comando para limpar a memória do R
#---------------------------------------�----------------------------------
# Entrada de dados
#---------------------------------------�----------------------------------
Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)
ex1=as.data.frame(cbind(Y,X1,X2)); ex1
attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------�-------------------
require(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 3D
#---------------------------------------�-------------------
require(Rcmdr)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
?scatter3d
?library
library(help=rgl)
library(help=car)
# SCRIPT
# -----------
rm(list=ls()) # comando para limpar a memória do R
#---------------------------------------�----------------------------------
# Entrada de dados
#---------------------------------------�----------------------------------
Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)
ex1=as.data.frame(cbind(Y,X1,X2)); ex1
attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------�-------------------
require(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 3D
#---------------------------------------�-------------------
library(splines)
library(car)
library(rgl)
library(mgcv)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
rm(list=ls()) # comando para limpar a memória do R
#---------------------------------------�----------------------------------
# Entrada de dados
#---------------------------------------�----------------------------------
Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)
ex1=as.data.frame(cbind(Y,X1,X2)); ex1
attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------�-------------------
require(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")
library(rgl)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
library(help=rgl)
??scatter3d
library(car)
library(help=car)
library(car)
rm(list=ls()) # comando para limpar a memória do R
#---------------------------------------�----------------------------------
# Entrada de dados
#---------------------------------------�----------------------------------
Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)
ex1=as.data.frame(cbind(Y,X1,X2)); ex1
attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------�-------------------
require(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 3D
#---------------------------------------�-------------------
library(car)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
rm(list=ls()) # comando para limpar a memória do R
#---------------------------------------�----------------------------------
# Entrada de dados
#---------------------------------------�----------------------------------
Y=c(16.68,11.5,12.03,14.88,13.75,18.11,8.0,17.83,79.24,21.5,40.33,21,13.5,
19.75,24,29,15.35,19,9.5,35.1,17.9,52.32,18.75,19.83,10.75)
X1=c(7,3,3,4,6,7,2,7,30,5,16,10,4,6,9,10,6,7,3,17,10,26,9,8,4)
X2=c(560,220,340,80,150,330,110,210,1460,605,688,215,255,462,448,776,200,
132,36,770,140,810,450,635,150)
ex1=as.data.frame(cbind(Y,X1,X2)); ex1
attach(ex1) # anexando os dados na memória do R
names(ex1)
dim(ex1)
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 2D
#---------------------------------------�-------------------
library(scatterplot3d)
fig = scatterplot3d(X1,X2,Y, box=F, type='p', lwd=1, pch=19,
xlim=c(0,50), ylim=c(0,1500), zlim=c(0,150))
plano = lm(Y~X1+X2)
fig$plane3d(plano, lty.box = "solid")
#---------------------------------------�-------------------
# Gráfico com as 3 variáveis em estudo em 3D
#---------------------------------------�-------------------
#>> require(Rcmdr)
library(car)
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
?scatter3d
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=1, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
scatter3d(X1, Y, X2, ellipsoid=F, surface=T, point.col='yellow',
bg='black', sphere.size=1.2, revolutions=2, axis.col='white',
xlab="Quantidade Estocada", ylab="Tempo (min)", zlab="Distancia (pes)")
?screen
setwd("~/Mega/BID2012")
source('~/Mega/BID2012/PonderaVoronoi.R')
gc
gcc
source("PonderaVoronoi.R")
bdir <- "CUENCAS"
cuencas <- list.files(bdir) # orden alfabético, para el caso es suficiente
cc <- cuencas[1]
cc
cdir <- paste0(bdir, "/", cc)
# Archivo con la definición de las estaciones
fname <- paste0(cdir, "/Estaciones.txt")
# Las estaciones:
ee <- read.table(fname, header=T)
# Geometría de la cuenca:
fname <- paste0(cdir, "/Geometria.txt")
gcc <- read.table(fname)
ee
ord(ee$id)
order(ee$id)
ii <- order(ee$id)
ee$id[ii]
head(gcc)
pesos <- PonderaVoronoi(ee[,2:3],gg)
pesos <- PonderaVoronoi(ee[,2:3],gcc)
pesos
sum(pesos)
plot(vvp)
length(vvp)
points(ee[,2:3])
vvp
sapply(vvp@polygons, function(elt) length(elt@Polygons))
tercero <- vvp@polygons[[3]]
tercero
tercero@area
sapply(tercero@Polygons, function(e) e@area)
sum (sapply(tercero@Polygons, function(e) e@area))
vvp@polygons[[3]]@area
source('~/Mega/BID2012/PonderaVoronoi.R')
test()
sum(test())
cc
cdir
fname <- paste0(cdir, "/Estaciones.txt")
# Las estaciones:
ee <- read.table(fname, header=T)
# Geometría de la cuenca:
fname <- paste0(cdir, "/Geometria.txt")
gcc <- read.table(fname)
pesos <- PonderaVoronoi(ee[,2:3],gcc)
pesos
sum(pesos)
source('~/Mega/BID2012/PonderaVoronoi.R')
as.data.frame(pesos)
source('~/Mega/BID2012/PesosEstaciones.R')
source('~/Mega/BID2012/PesosEstaciones.R')
tt <- read.table("CUENCAS//YAQUI/Geometria.txt")
plot(tt, type="l")
tt[1,]==tt[length(tt),]
length(tt)
tt[1,]==tt[nrow(tt),]
?dist
points(27.117, -110.0094, pch=15, col="red")
plot(tt, type="l", xlim=c(27,27.2), ylim=c(-110.3, -109.9))
points(27.117, -110.0094, pch=15, col="red")
plot(tt, type="l", xlim=c(27.1,27.15), ylim=c(-110.13, -109.93))
points(27.117, -110.0094, pch=15, col="red")
plot(tt, type="l", xlim=c(27.115,27.12), ylim=c(-110.05, -109.97))
points(27.117, -110.0094, pch=15, col="red")
plot(tt, type="l", xlim=c(27.114,27.118), ylim=c(-110.05, -109.97))
points(27.117, -110.0094, pch=15, col="red")
uu <- 27.114 <= tt[,1] & tt[,1] <= 27.118 & -110.02 <= tt[,2] & tt[,2] <= 110
uu
which(uu)
ii <- which(uu)
?lag
jj <- lag(ii)
jj
jj <- ii[-1]
jj
dff <- data.frame(i=ii[-length(ii)],j=jj)
dff
dffa <- dff[c(10,15,24,31,40)*-1]
dffa
dffa <- dff[c(10,15,24,31,40)*-1,]
dffa
?segments
segments(0,0,1,1)
segments(0,0,1,1)
plot(tt, type="n", xlim=c(27.114,27.118), ylim=c(-110.05, -109.97))
savehistory("~/Mega/BID2012/historia.R")
