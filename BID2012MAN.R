require("xlsx")
require("plotrix")
require("lattice")

#Construcc�n de "info", para ejecutar las funciones. Los valores van entre comillas ("").
# "info" tiene las siguientes variables:

# -cobertura -->  Colocar si el calculo ser� por archivo (estaci�n) o todo un directorio (cuenca).
#Lo que se debe poner es: estacion o cuenca.
# Ejemplo:
#         info$cobertura <- "estacion"

# -path      -->  Colocar la ubicaci�n de la estaci�n o cuenca.
#Ejemplo:
#         info$path <- "/CUENCAS/BRAVO-SAN-JUAN/19012.dat"

# -tipop     -->  Se refiere al tipo de operaci�n.
# Estas son las opciones a elegir: mensual, trimestral, anual, maxima, media, minima
# Ejemplo:
#         info$tipop <- "trimestral"

# -out      -->  Se coloca el nombre del archivo de salida. 
# Ejemplo:
#         info$out<-"./ExlTEMPORAL.xls"

#Cargar las fucniones :
#require("BID2012.R")

#Mandar a llamar una funci�n:
#ff.1(info)

#====================================================
# ************* EMPIEZA ENCABEZADO ******************
# ENCABEZADO PARA USAR ESTE CONJUNTO (CATÁLOGO) DE
# FUNCIONES DENTRO DEL SISTEMA "cliMTA" 
# El usuario podrá cambiar las definiciones marcadas
# con (***) y podrá incluso agregar nuevos elementos
# en lo marcado con (***...***). Ninguna otra instruc-
# ción deberá ser cambiada, so pena de que catálogo de
# funciones no se ejecute correctamente en el contexto
# de cliMTA.
# ***************************************************
#
# Auxiliar para entregar los valores en una lista
Values <- function(lst) {
  return (unname(unlist(lst)))
}

# ----------------------------
# DEFINICIÓN DE CONSTANTES:

Presentacion <- "                  Procesando catálogo:" # (***) Introduzca aquí su propia presentación

indir <- "./CUENCAS" # (***) Directorio inicial para búsqueda de archivos de datos

# Se decide si seleccionar un archivo o todo un directorio
# que contiene un conjunto de archivos a operar sobre ellos.
# A esto le llamaremos cobertura y solo tiene dos elementos
# Aquí se definen las dos categorías
#                 +--------------------------+-------------------------+
#                 |    POR ARCHIVOS          |  POR DIRECTORIOS        |
#                 +--------------------------+-------------------------+
cobertura <- 
  data.frame(
      itm=	c(	"estacion",		"cuenca"		), # (***) 
      txt=	c(	"Operar por estación", 	"Operar por CUENCA"	), # (***)
      tipf=	c(	".dat",			".dat"			), # (***) Tipo de archivos uniforme
    row.names=	c(	"archivo", 		"directorio"		), # <OJO: NO CAMBIAR
    stringsAsFactors = FALSE
  )
# Cuál es el default de este frame?
CoberturaDef <- "archivo" # (***) Sólo dos opciones: archivo o directorio

# Número total de operaciones en el paquete:
NFuncs <- 21 # (***) 

# texto de las funciones:
txtF <- c(
  "Lluvia por 1 día",			# 01 (***) 
  "Lluvia por 5 días",			# 02 (***) 
  "Lluvia por 10 días",			# 03 (***) 
  "Lluvia por 1 mes",			# 04 (***) 
  "Lluvia por 3 meses",			# 05 (***) 
  "Lluvia por 1 año",			# 06 (***) 
  "Días sin lluvia",			# 07 (***) 
  "Duración 1 día > 90%",		# 08 (***) 
  "Duración 5 días > 90%",		# 09 (***) 
  "Duración 10 días > 90%",		# 10 (***) 
  "Duración 1 día > 95%",		# 11 (***) 
  "Duración 5 días > 95%",		# 12 (***) 
  "Duración 10 días > 95%",		# 13 (***) 
  "Promedio estacional",		# 14 (***) 
  "Promedio anual",			# 15 (***) 
  "Días con Temp >= 40°C",		# 16 (***) 
  "Días con Temp >= 90%",		# 17 (***) 
  "Días con Temp >= 95%",		# 18 (***) 
  "Días con Temp <= 0°C",		# 19 (***) 
  "Días con Temp <= 10%",		# 20 (***) 
  "Días con Temp <= 5%"			# 21 (***...***) Tantos como NFuncs
)

# Nombre de archivo excel de salida por default:
ExcelFile <- "ExlTEMPORAL.xls" #  (***) Default

# Categorías de la estructura de información:
InfoCats <-c("cobertura", "path", "tipop", "out")
# Construcción de la estructura de información:
info <- list()
length(info) <- length(InfoCats)
names(info) <- InfoCats
infoDefaults <- c(cobertura[CoberturaDef,"itm"], "", "", ExcelFile)

# ------------------------
# Las variables que se manejarán:
vars <- c("precipitacion", "temperatura") # (***) 
# índice del default:
indDefvar = 1  # (***) Elegir uno de los índices válidos de vars
#--------------------------
# Los tipos de operaciones, según cada variables definidas
# anteriormente en "vars"
tipoOp <- list()
tipoOp[[vars[1]]] <- c("mensual", "trimestral", "anual")	# (***) 
tipoOp[[vars[2]]] <- c("maxima", "media", "minima")		# (***...***) agragar un elem. por c/elem en vars

# Se arma una lista lineal por los tipos de operaciones anteriores
tipoOpsList <- Values(tipoOp) # Es un vector c("mensual", "trimestral",... "maxima", ...)

# Selector de operaciones:
# Cada tipoOp tiene acceso a ciertas funciones en el catálogo de funciones, aquí se 
# indican ellas:
selector <- list()
length(selector) <- length(tipoOpsList)
names(selector) <- tipoOpsList
selector[1:length(selector)] <- list(
  1:4, 		# mensual 	(***)
  c(1:5,7:13),	# trimestral 	(***)
  1:13, 	# anual 	(***)
  14:18, 	# maxima 	(***)
  14:18, 	# media 	(***)
  14:21		# minima 	(***...***) Tantos grupos como elementos tenga tipoOpsList
)
#
# 
# ************* TERMINA ENCABEZADO ******************
#====================================================

ff.1<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,1,2)
}

ff.2<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,5,2)
}

ff.3<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,10,2)
}

ff.4<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,30,2)
}

ff.5<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,90,2)
}

ff.6<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasLluvia(datVar,opt,360,2)
}

ff.7<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  diasNoLluvia(datVar,opt,2)
}

ff.8<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,90)
  diasLluvia(datVar,opt,1,valor)
}

ff.9<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,90)
  diasLluvia(datVar,opt,5,valor)
}

ff.10<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,90)
  diasLluvia(datVar,opt,10,valor)
}

ff.11<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,95)
  diasLluvia(datVar,opt,1,valor)
}

ff.12<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,95)
  diasLluvia(datVar,opt,5,valor)
}

ff.13<-function(info){
  datVar<-leerDat(info,4)
  opt<-info[["tipop"]]
  valor<-buscarGrado(datVar,95)
  diasLluvia(datVar,opt,10,valor)
}

ff.14<-function(info){
  # fff(info)
  ifelse(info[["tipop"]]=="maxima",numcolum<-5,NA)
  ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  ifelse(info[["tipop"]]=="media",numcolum<-7,NA)
  datVar<-leerDat(info,numcolum)
  prom(datVar,"trim",info)
}

ff.15<-function(info){
  ifelse(info[["tipop"]]=="maxima",numcolum<-5,NA)
  ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  ifelse(info[["tipop"]]=="media",numcolum<-7,NA)
  datVar<-leerDat(info,numcolum)
  prom(datVar,"anual",info)
}

ff.16<-function(info){
  ifelse(info[["tipop"]]=="maxima",numcolum<-5,NA)
  ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  ifelse(info[["tipop"]]=="media",numcolum<-7,NA)
  datVar<-leerDat(info,numcolum)
  contarDias(datVar,40,info,"mayor")
}

ff.17<-function(info){
  ifelse(info[["tipop"]]=="maxima",numcolum<-5,NA)
  ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  ifelse(info[["tipop"]]=="media",numcolum<-7,NA)
  datVar<-leerDat(info,numcolum)
  grado<-buscarGrado(datVar,90)
  contarDias(datVar,grado,info,"mayor")
}

ff.18<-function(info){
  ifelse(info[["tipop"]]=="maxima",numcolum<-5,NA)
  ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  ifelse(info[["tipop"]]=="media",numcolum<-7,NA)
  datVar<-leerDat(info,numcolum)
  grado<-buscarGrado(datVar,95)
  contarDias(datVar,grado,info,"mayor")
}

ff.19<-function(info){
  #ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  datVar<-leerDat(info,6)
  contarDias(datVar,0,info,"menor")
}

ff.20<-function(info){
  #ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  datVar<-leerDat(info,6)
  grado<-buscarGrado(datVar,10)
  contarDias(datVar,grado,info,"menor")
}

ff.21<-function(info){
  #ifelse(info[["tipop"]]=="minima",numcolum<-6,NA)
  datVar<-leerDat(info,6)
  grado<-buscarGrado(datVar,5)
  contarDias(datVar,grado,info,"menor")
}


leerDat<-function(info,numcolum){
    if(info[["cobertura"]]=="estacion"){
       estacion<-read.table(info[["path"]])
       V7<-rowMeans(estacion[,5:6],na.rm=TRUE)
       estacion$V7<-V7
       colData<-c("year","mes","dia","Pre","Tmax","Tmin","Tavg")
       colData<-c(colData[1:3],colData[numcolum])
       estacion<-data.frame(estacion[,1:3],estacion[,numcolum])
       colnames(estacion)<-colData
    }
    else{
	store<-list()
        ruta2<-paste("ls ",info[["path"]],sep="")
	archivos<-system(ruta2,intern=TRUE) 
	for(j in 1:length(archivos)){
	    #Lee archivo, y se guarda en un dataframe
	    archi<-paste("estaciones<-read.table('",info[["path"]],"/",archivos[j],"')",sep="")
	    eval(parse(text=archi))
	    #Se agrupan las estaciones en una lista  
	    archi1<-paste("store[length(store)+1]<-list(est",j,"=estaciones)",sep="")
	    eval(parse(text=archi1))
	}
	#Combinar los frames de la lista en un solo frame
	cuenca <- do.call(rbind,store)
	estacion<-aggregate(cuenca[,4:6], list(cuenca$V3,cuenca$V2,cuenca$V1), mean,na.rm=TRUE)
        V7<-rowMeans(estacion[,5:6],na.rm=TRUE)
        estacion$V7<-V7
	colData<-c("dia","mes","year","Pre","Tmax","Tmin","Tavg")
	colData<-c(colData[1:3],colData[numcolum])
        estacion<-data.frame(estacion[,1:3],estacion[,numcolum])
        colnames(estacion)<-colData
    }
    return(estacion)
}

#var: Tmax,Tmin,Tavg
prom<-function(estacion,opt,info){
     r2<-1;años<-c(1970:1999)
     out<-info[["out"]]
     varname<-"Temperatura"
     nam2<-list("Media","Desv","Int")
     for(l in 70:99){
         ejL <-paste("lista19",l,"<-numeric()",sep="")
	 eval(parse(text=ejL))
     }
    
     for(a in 70:99){
	   ejA<-paste("index",a,".cuenca<-which(estacion$year%in%19",a,")",sep="")
	   eval(parse(text=ejA))
	   #Para buscarlo por meses, es necesario guardar un dataframe con esos indices
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
     }
  
     if(opt=="trim"){
       Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  #Estaciones del año
       nam<-list("Invierno","Primavera","Verano","Otoño")
       nameEst<-list("Inv","Prim","Ver","Oto")
       colm<-c("1:3","4:6","7:9","10:12")
       for(a in 70:99){
	   r<-1	   
	   #Trimestre (estaciones del año)------------------------
	   for(b in 1:4){
	       ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
	       eval(parse(text=ejB))
	       ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
	       eval(parse(text=ejDatFr2))
	      
	      #***Si es invierno. buscar del año pasado y agregarlo a la variable  
	      if(a>70 & nameEst[b]=="Invierno"){
		  idxInv<-a-1
		  ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
	          eval(parse(text=ejInv))
		 
		  #Se guarda como dataframe
		  ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
	          eval(parse(text=ejG))
		  #Unir dataframe
		  ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
	          eval(parse(text=ejU))
	      }
	      
	      #Por variable: Pre($V4), Tmx($V5), Tm($V6), PromT($V7)
	      ejC <- paste("media<-mean(dat",a,nameEst[b],"estacion[,4]);desviacion<-sd(dat",a,nameEst[b],"estacion[,4]);num.datos<-nrow(dat",a,nameEst[b],"estacion);tc<-qt(0.975,(num.datos-1));intervalo<-tc*(desviacion/sqrt(num.datos-1))",sep="")
	      eval(parse(text=ejC))
	      fill <- paste("lista19",a,"[",r,"]<-c(media)",sep="");eval(parse(text=fill))
	      fill2 <- paste("lista19",a,"[",r+1,"]<-c(desviacion)",sep="");eval(parse(text=fill2)) 
	      fill3 <- paste("lista19",a,"[",r+2,"]<-c(intervalo)",sep="");eval(parse(text=fill3))
	      r=r+3      #Para guardar toda en la lista (Invierno1970:inv,pri,ver,ot)
	   } #estaciones del año
       }#años
       estacion.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       
       #cambiar el nombre de las columnas
       for(id in 1:4){   #Estaciones del año
          for(e in 1:3){	#Media,Desv,Inv
	      cambName <- paste("names(estacion.Trimestral)[",r2,"]<-","\"",nameEst[id],names(estacion[4]),".",nam2[e],"\"",sep="");eval(parse(text=cambName))  
	      r2=r2+1 
	  }
      }
      #cambiar el nombre de filas
      rownames(estacion.Trimestral)<-años
      ejW2<- paste("write.xlsx(round(estacion.Trimestral[,",colm[1],"],digits=2),file=out,sheetName =\"",nameEst[1],"\")",sep="")
      eval(parse(text=ejW2))
      for(id in 2:4){    #Tener archivo por estacion del año 
	  ejW2<- paste("write.xlsx(round(estacion.Trimestral[,",colm[id],"],digits=2),file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="")
	  eval(parse(text=ejW2))
      }
      #Mostrar Graficas
      indexEst<-c(1,4,7,10)
      indexInt<-c(3,6,9,12)
      info2<-info[["tipop"]]
      ifelse(info2=="maxima",info2<-"máxima",NA)
      ifelse(info2=="minima",info2<-"mínima",NA)
      for(g in 1:4){
     	  x11()
	  titulo<-paste(nam[g]," ",varname,"-",info2)
	  graficar(estacion.Trimestral[,indexEst[g]],titulo,"°C")
	  ajusteLin(estacion.Trimestral[,indexEst[g]])
          inter(estacion.Trimestral[,indexEst[g]],estacion.Trimestral[,indexInt[g]])
      }
   }#trim
     else{ #-----anual
       for(a in 70:99){
	   r<-1
           ejC <- paste("media<-mean(dat",a,"estacion[,4]);desviacion<-sd(dat",a,"estacion[,4]);num.datos<-nrow(dat",a,"estacion);tc<-qt(0.975,(num.datos-1));intervalo<-tc*(desviacion/sqrt(num.datos-1))",sep="")
	   eval(parse(text=ejC))
           fill <- paste("lista19",a,"[",r,"]<-c(media)",sep="");eval(parse(text=fill))
           fill2 <- paste("lista19",a,"[",r+1,"]<-c(desviacion)",sep="");eval(parse(text=fill2)) 
	   fill3 <- paste("lista19",a,"[",r+2,"]<-c(intervalo)",sep="");eval(parse(text=fill3))
	   r=r+3  
       }#años
       estacion.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       for(e in 1:3){	#Media,Desv,Inv
           cambName <- paste("names(estacion.Anual)[",r2,"]<-","\"",names(estacion[4]),".",nam2[e],"\"",sep="")
	   eval(parse(text=cambName))  
	   r2=r2+1 
       }
       #cambiar el nombre de filas
       rownames(estacion.Anual)<-años
       write.xlsx(round(estacion.Anual,digits=2),file=out,sheetName ="Anual")
       #Mostrar Graficas
       info2<-info[["tipop"]]
       ifelse(info2=="maxima",info2<-"máxima",NA)
       ifelse(info2=="minima",info2<-"mínima",NA)
       titulo<-paste("Anual ",varname,"-",info2)
       graficar(estacion.Anual[,1],titulo,"°C")
       ajusteLin(estacion.Anual[,1])
       inter(estacion.Anual[,1],estacion.Anual[,3])
  }#anual
}#funcion

contarDias<-function(estacion,grados,info,signo){
    out<-info[["out"]]
    años<-c(1970:1999)
    ifelse(signo=="mayor",signo<-">",signo<-"<")
    #Buscar por cada año
    for(a in 70:99){
      ejA<-paste("index",a,".estacion<-which(estacion$year%in%19",a,")",sep="");eval(parse(text=ejA))
      ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".estacion,])",sep="");eval(parse(text=ejDatFr))
    
      ejC <- paste("temp<-dat",a,"estacion[,4]",signo,"=grados;temp<-sum(temp)",sep="");eval(parse(text=ejC))
      fill <- paste("lista19",a,"<-temp",sep="");eval(parse(text=fill)) 
    }#años
    
    tempM.estacion<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
    grados2<-paste(grados,"°C")
    colnames(tempM.estacion)<-grados2  
    rownames(tempM.estacion)<-años 
    write.xlsx(tempM.estacion,file=out,sheetName =info[["tipop"]])
    #Mostrar Graficas
    info2<-info[["tipop"]]
    ifelse(info2=="maxima",info2<-"máxima",NA)
    ifelse(info2=="minima",info2<-"mínima",NA)
    titulo<-paste("Temperatura -",info2)
    graficar(tempM.estacion[,1],titulo,"días")
    ajusteLin(tempM.estacion[,1])
}

#porc= 90,95 .. 
buscarGrado<-function(estacion,porc){
    p<-porc/100
    estacion.ord<-cbind(sort(estacion[,4]))
    indxest<-round(length(estacion.ord)*p)
    temp<-estacion.ord[indxest]
    return(temp)
}

graficar<-function(datos,titulo,ylab){
    plot(c(1970:1999),datos,main=titulo,ylab=ylab,xlab="años",type="b",pch=17,col="blue")
}#funcion

graficarAll<-function(datos,titulo,ylab,camp){
   colm <- eval(parse(text=camp))
   #limites
   ej<-paste("y1<-round(max(datos[,",camp,"]));y2<-floor(min(datos[,",camp,"]));r<-y1-y2;rs<-40-r;ifelse(rs>0,y1<-y1+rs,NA);ylim<-\"c(y2,y1)\"",sep="");eval(parse(text=ej))
   ej<-paste("plot(c(1970:1999),datos[,",colm[1],"],main=\"",titulo,"\",ylab=\"",ylab,"\",col=1,ylim=",ylim,",xlab=\"años\",type=\"b\",pch=17)",sep="");eval(parse(text=ej))
   for(i in 2:length(colm)){ #length(colm)
      ej<-paste("lines(c(1970:1999),datos[,",colm[i],"],col=",i,",type=\"o\",pch=17)",sep="");eval(parse(text=ej))
  }
}

#Valor de y, para el ajuste
daty<-function(a,m,datx){
    m*datx+a
}

ajusteLin<-function(datos){
    x<-cbind(c(1),c(1970:1999))
    ajuste<-lm(datos~x);yy<-daty(ajuste$coe[[1]],ajuste$coe[[3]],c(1970:1999))
    ss<-summary(ajuste)
    subtitulo<-paste("Y = ",round(ajuste$coe[[1]],digits=2),"+",round(ajuste$coe[[3]],digits=2),"X, [error intercep=",round(ss$coe[1,2],digits=2),", error tenden=",round(ss$coe[2,2],digits=2),"]")
    lines(c(1970:1999),yy,col="green",type="l")
    mtext(subtitulo,side=3)
}

inter<-function(y,intervalo){
    plotCI(c(1970:1999),y,uiw=intervalo,col="red",add=TRUE)
}

diasLluvia<-function(estacion,opt,dias,valor){
      años<-c(1970:1999)
      dd<-paste(dias,"día (s) - ",valor, "mm/día")
      info2<-"precipitación"
      out<-info[["out"]]
      for(l in 70:99){  #  <--- Crea listas
         ejL <-paste("lista19",l,"<-numeric()",sep="")
	 eval(parse(text=ejL))
      }

      for(a in 70:99){  #  <--- Crea dataframes por año
	   ejA<-paste("index",a,".cuenca<-which(estacion$year%in%19",a,")",sep="")
	   eval(parse(text=ejA))
	   #Para buscarlo por meses, es necesario guardar un dataframe con esos indices
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
      }

    if(opt=="trimestral"){
       Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  #Estaciones del año
       nam<-list("Invierno","Primavera","Verano","Otoño")
       nameEst<-list("Inv","Prim","Ver","Oto")
       for(a in 70:99){
	   r<-1
	   #Trimestre (estaciones del año) <-----Genera dataframes por trimestre
	   for(b in 1:4){
	       ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
	       eval(parse(text=ejB))
	       ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
	       eval(parse(text=ejDatFr2))
	      
	      #Si es invierno. buscar del año pasado y agregarlo al dataframe 
	      if(a>70 & nameEst[b]=="Invierno"){
		  idxInv<-a-1
		  ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
	          eval(parse(text=ejInv))
		 
		  #Se guarda como dataframe
		  ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
	          eval(parse(text=ejG))
		  #Unir dataframe
		  ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
	          eval(parse(text=ejU))
	      }
	      #Dias de LLuvia <-------- Busca los dias de lluvia
	      ejC <- paste("Lluvia<-dat",a,nameEst[b],"estacion[,4]>",valor,";Ni<-as.integer(Lluvia)",sep="");eval(parse(text=ejC))
              if(length(Ni)==0){
                     d<-NaN
              }
              else{
	          L<-conjunta(Ni)
                  d<-contarEventos(L,dias)
              }
              fill <- paste("lista19",a,"[",r,"]<-d",sep="");eval(parse(text=fill))
	      r=r+1 #Para guardar toda en la lista (Invierno1970:inv,pri,ver,ot)
	   } #estaciones del año
       }#años
       Lluvia.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       
       #cambiar el nombre de las columnas 
       for(t in 1:4){ 
	    cambName <- paste("names(Lluvia.Trimestral)[",t,"]<-","\"",dd,"\"",sep="")
            eval(parse(text=cambName))
       }
       #cambiar el nombre de filas
       rownames(Lluvia.Trimestral)<-años
       write.xlsx(Lluvia.Trimestral[1],file=out,sheetName ="Inv")
       for(id in 2:4){    #Tener archivo por estacion del año 
	  ejW2<- paste("write.xlsx(Lluvia.Trimestral[",id,"],file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="")
	  eval(parse(text=ejW2))
       }
      #Mostrar Graficas
      for(g in 1:4){
     	  x11()
	  titulo<-paste(nam[g]," - ",info2," (",valor,"mm día⁻¹)")
	  graficar(Lluvia.Trimestral[,g],titulo,"días")
      }
    }#trim

    else if(opt=="mensual"){
       rM<-1
       mes<-c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")
       for(a in 70:99){
	   r<-1
	   for(m in 1:12){
	      ejB<-paste("index",a,mes[m],"<-which(dat",a,"estacion$mes%in%",m,")",sep="");eval(parse(text=ejB))
	      ejDatFr2<-paste("dat",a,mes[m],"<-data.frame(dat",a,"estacion[index",a,mes[m],",])",sep="");eval(parse(text=ejDatFr2))
	      ejC <- paste("Lluvia<-dat",a,mes[m],"[,4]>",valor,";Ni<-as.integer(Lluvia)",sep="");eval(parse(text=ejC))
              if(length(Ni)==0){
                     d<-NaN
              }
              else{
                    L<-conjunta(Ni)
                    d<-contarEventos(L,dias)
              }
	      fill <- paste("lista19",a,"[",r,"]<-d",sep="");eval(parse(text=fill))
	      r=r+1 
	   }#meses
       }#años
       Lluvia.Mensual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       #cambiar el nombre de las columnas -- mensual
       for(im in 1:12){ 
           cambName <- paste("names(Lluvia.Mensual)[",rM,"]<-\"",dd,"\"",sep="")
           eval(parse(text=cambName))   
           rM=rM+1
       }
       rownames(Lluvia.Mensual)<-años
       ejW2<- paste("write.xlsx(Lluvia.Mensual[1],file=out,sheetName =\"",mes[1],"\")",sep="")
       eval(parse(text=ejW2))
       for(id in 2:12){    #Tener archivo por estacion del año 
           ejW2<- paste("write.xlsx(Lluvia.Mensual[",id,"],file=out,append=TRUE,sheetName =\"",mes[id],"\")",sep="")
	   eval(parse(text=ejW2))
      }
      #Mostrar Graficas
      for(g in 1:12){
     	  x11()
	  titulo<-paste(mes[g]," - ",info2," (",valor,"mm día⁻¹)")
	  graficar(Lluvia.Mensual[,g],titulo,"días")
      }
   }#mes
    else if(opt=="anual"){
	for(a in 70:99){
	   r<-1
           ejC <- paste("Lluvia<-dat",a,"estacion[,4]>",valor,";Ni<-as.integer(Lluvia)",sep="");eval(parse(text=ejC))
           if(length(Ni)==0){
                     d<-NaN
           }
           else{
                     L<-conjunta(Ni)
                     d<-contarEventos(L,dias)
           }
           fill <- paste("lista19",a,"[",r,"]<-d",sep="");eval(parse(text=fill)) 
       }#años
       Lluvia.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       #cambiar el nombre de las columnas -- mensual
       colnames(Lluvia.Anual)<-dd   
       rownames(Lluvia.Anual)<-años
       write.xlsx(Lluvia.Anual,file=out,sheetName ="Anual")
       #Mostrar Graficas
       titulo<-paste("Anual - ",info2," (",valor,"mm día⁻¹)")
       graficar(Lluvia.Anual[,1],titulo,"días")
    }#anual
}#funcion

diasNoLluvia<-function(estacion,opt,valor){
      my.colors = colorRampPalette(c("light green", "yellow", "orange", "red"))
      años<-c(1970:1999);r2<-1
      dias<-list("1 día","5 días","10 días","20 días","30 días","50 días","90 días")
      info2<-"precipitación";out<-info[["out"]]
      for(l in 70:99){  #  <--- Crea listas
         ejL <-paste("lista19",l,"<-numeric()",sep="")
	 eval(parse(text=ejL))
      }

      for(a in 70:99){  #  <--- Crea dataframes por año
	   ejA<-paste("index",a,".cuenca<-which(estacion$year%in%19",a,")",sep="")
	   eval(parse(text=ejA))
	   #Para buscarlo por meses, es necesario guardar un dataframe con esos indices
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
      }
      if(opt=="trimestral"){
          Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  #Estaciones del año
	  nam<-list("Invierno","Primavera","Verano","Otoño")
	  nameEst<-list("Inv","Prim","Ver","Oto")
          colm<-c("1:90","91:180","181:270","271:360")
          
	  for(a in 70:99){
	      r<-1;cc<-1;cc2<-90
	      #Trimestre (estaciones del año) <-----Genera dataframes por trimestre
	      for(b in 1:4){
		  ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
		  eval(parse(text=ejB))
		  ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
		  eval(parse(text=ejDatFr2))
		  
		  #Si es invierno. buscar del año pasado y agregarlo al dataframe 
		  if(a>70 & nameEst[b]=="Inv"){
		      idxInv<-a-1
		      ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
		      eval(parse(text=ejInv))
		    
		      #Se guarda como dataframe
		      ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
		      eval(parse(text=ejG))
		      #Unir dataframe
		      ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
		      eval(parse(text=ejU))
		  }
		  #Dias de LLuvia <-------- Busca los dias de lluvia
		  ejC <- paste("NoLluvia<-dat",a,nameEst[b],"estacion[,4]<",valor,";Ni<-as.integer(NoLluvia)",sep="");eval(parse(text=ejC))
                  #fff.msg(Ni[1:5])
                  if(length(Ni)==0){
                     d<-NaN
                  }
                  else{
		    L<-conjunta(Ni)
		    d<-EventosNoLluvia(L,90)
		  }
		  #Contar los dias: 1,5,10,20,30,50,90
		#  d1<-contarEventos(L,1);d5<-contarEventos(L,5);d10<-contarEventos(L,10);d20<-contarEventos(L,20);d30<-contarEventos(L,30);d50<-contarEventos(L,50);d90<-contarEventos(L,90)
		  fill <- paste("lista19",a,"[",cc,":",cc2,"]<-d",sep="")
		  eval(parse(text=fill))
                  cc=cc+90
                  cc2=cc2+90
	      } #estaciones del año 
	  }#años
	  NoLluvia.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
	  #cambiar el nombre de las columnas -- trimestral
	  for(id in 1:4){   
	     for(d in 1:90){	#dias
	          cambName <- paste("names(NoLluvia.Trimestral)[",r2,"]<-","\"",d,"\"",sep="");eval(parse(text=cambName)) 
		  r2=r2+1
		}
	    }
	  #cambiar el nombre de filas
	  rownames(NoLluvia.Trimestral)<-años 
  
	  write.xlsx(NoLluvia.Trimestral[,1:90],file=out,sheetName ="Inv")
	  for(id in 2:4){            
	      #Tener archivo por estacion del año 
	      ejW2<- paste("write.xlsx(NoLluvia.Trimestral[,",colm[id],"],file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="");eval(parse(text=ejW2))
	  }
          mm<-as.matrix(NoLluvia.Trimestral)
	  #Mostrar Graficas
          cc<-1;cc2<-90
	  for(g in 1:4){
     	    x11()
	    titulo<-paste(nam[g]," - ",info2)
            par(mar=c(5,4,4,6))
            maxx<-max(mm[,cc:cc2],na.rm=TRUE)
            ColorLevels <- seq(0, maxx, length=90)
           # fff.in()
            image(años,c(1:90),mm[,cc:cc2],col=my.colors((maxx+1)),zlim=c(0,maxx),ylab="rango",main=titulo)
            par(mar=c(5,32.5,4,1),new=TRUE)
	    image(1,ColorLevels,matrix(data=ColorLevels,ncol=length(ColorLevels),nrow=1),xlab="",ylab="",xaxt="n",col=my.colors((maxx+1)))
            #levelplot(mm[,cc:cc2],colorkey=TRUE,region=TRUE,rowvalues=años,colvalues=c(1:90))
            cc=cc+90
            cc2=cc2+90
	    #fff.out()
          }
      }#trim   

      else if(opt=="anual"){
	  for(a in 70:99){
	   r<-1
           ejC <- paste("NoLluvia<-dat",a,"estacion[,4]<",valor,";Ni<-as.integer(NoLluvia)",sep="");eval(parse(text=ejC))
           if(length(Ni)==0){
              d<-NaN
           }
           else{
	       L<-conjunta(Ni)
	       d<-EventosNoLluvia(L,365)
	   }
	   fill <- paste("lista19",a,"<-d",sep="");eval(parse(text=fill))
       }#años

       NoLluvia.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       #cambiar el nombre de las columnas -- mensual
       for(d in 1:365){	#dias
	          cambName <- paste("names(NoLluvia.Anual)[",d,"]<-","\"",d,"\"",sep="");eval(parse(text=cambName)) 
       }
       rownames(NoLluvia.Anual)<-años
       write.xlsx(NoLluvia.Anual,file=out,sheetName ="Anual")
       mm<-as.matrix(NoLluvia.Anual)
       #Mostrar Graficas
       titulo<-paste("Anual - ",info2)
       par(mar=c(5,4,4,6))
       maxx<-max(mm,na.rm=TRUE)
       ColorLevels <- seq(0, maxx, length=365)
       image(años,c(1:365),mm,col=my.colors((maxx+1)),zlim=c(0,maxx),ylab="rango",main=titulo)
       par(mar=c(5,32.5,4,1),new=TRUE)
       image(1,ColorLevels,matrix(data=ColorLevels,ncol=length(ColorLevels),nrow=1),xlab="",ylab="",xaxt="n",col=my.colors((maxx+1)))
      }#anual

}#funcion

conjunta <- function(Ni) {
  
  s <- 0 #; fff.msg(length(Ni))
  L <- rep(0,length(Ni))
  #fff.msg(Ni[1:5])
  for (i in 1:length(Ni)) {
    ifelse(is.na(Ni[i]),Ni[i]<-0,NA)
    if (Ni[i] == 0) {
      if (i != 1){
      L[i-1] <- s
      }
      s <- 0
      
    } 
    else {
      s <- s+1
    }
  }
  L[i] <- s
  return (L)
}


contarEventos<-function(L,dias){
  indx <- which(L>=dias)
  N <- L[indx]
  x <- N-dias+1
  x <- sum(x)
  return(x)
}

EventosNoLluvia<-function(L,dias){
  indx <- which(L>0)
  N <- L[indx]
  corte <- c(0.5:(dias+0.5))
  h<-hist(N,breaks=corte,plot=FALSE)
  counts<-h[["counts"]]
  return(counts)
}




