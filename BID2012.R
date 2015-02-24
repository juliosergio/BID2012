require("xlsx")
require("plotrix")
require("lattice")
Encoding("UTF-8")
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

Presentacion <- "                  Procesando funciones de" # (***) Introduzca aquí­ su propia presentación

indir <- "./CUENCAS" # (***) Directorio inicial para búsqueda de archivos de datos

# Se decide si seleccionar un archivo o todo un directorio
# que contiene un conjunto de archivos a operar sobre ellos.
# A esto le llamaremos cobertura y solo tiene dos elementos
# Aquí­ se definen las dos categorí­as
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
NFuncs <- 22 # (***) 

# texto de las funciones:
txtF <- c(
  "Lluvia por 1 día",			# 01 (***) 
  "Lluvia por 5 días",			# 02 (***) 
  "Lluvia por 10 días",			# 03 (***) 
  "Lluvia por 1 mes",			# 04 (***) 
  "Lluvia por 3 meses",			# 05 (***) 
  "Lluvia por 1 año",			# 06 (***) 
  "Days without rain",			# 07 (***) 
  "Duración 1 día > 90%",		# 08 (***) 
  "Duración 5 días > 90%",		# 09 (***) 
  "Duración 10 días > 90%",		# 10 (***) 
  "Duración 1 día  > 95%",		# 11 (***) 
  "Duración 5 días > 95%",		# 12 (***) 
  "Duración 10 días > 95%",		# 13 (***) 
  "Promedio estacional",		# 14 (***) 
  "Promedio anual",			# 15 (***) 
  "Días con Temp >= 40 °C",		# 16 (***) 
  "Días con Temp >= 90%",		# 17 (***) 
  "Días con Temp >= 95%",		# 18 (***) 
  "Días con Temp <= 0 °C",		# 19 (***) 
  "Días con Temp <= 10%",		# 20 (***) 
  "Días con Temp <= 5%",		# 21 (***...***) Tantos como NFuncs
  "Lluvia Acumulada" 			#22
)

# Nombre de archivo excel de salida por default:
ExcelFile <- "ExlTEMPORAL.xlsx" #  (***) Default

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
# Í­ndice del default:
indDefvar = 1  # (***) Elegir uno de los Índices válidos de vars
#--------------------------
# Los tipos de operaciones, según cada variables definidas
# anteriormente en "vars"
tipoOp <- list()
tipoOp[[vars[1]]] <- c("mensual", "trimestral", "anual")	# (***) 
tipoOp[[vars[2]]] <- c("maxima", "media", "minima")		# (***...***) agragar un elem. por c/elem en vars

# Se arma una lista lineal por los tipos de operaciones anteriores
tipoOpsList <- Values(tipoOp) # Es un vector c("mensual", "trimestral",... "maxima", ...)

# Selector de operaciones:
# Cada tipoOp tiene acceso a ciertas funciones en el catálogo de funciones, aquí­ se 
# indican ellas:
selector <- list()
length(selector) <- length(tipoOpsList)
names(selector) <- tipoOpsList
selector[1:length(selector)] <- list(
  1:4, 		# mensual 	(***)
  c(1:5,7:13,22),	# trimestral 	(***)
  c(1:13,22), 	# anual 	(***)
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
  datVar<-leerDat(info,6)
  contarDias(datVar,0,info,"menor")
}

ff.20<-function(info){
  datVar<-leerDat(info,6)
  grado<-buscarGrado(datVar,10)
  contarDias(datVar,grado,info,"menor")
}

ff.21<-function(info){
  datVar<-leerDat(info,6)
  grado<-buscarGrado(datVar,5)
  contarDias(datVar,grado,info,"menor")
}

ff.22<-function(info){
 datVar<-leerDat(info,4)
 opt<-info[["tipop"]]
 promLluvia(datVar,opt)
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
        ruta2<-paste("dir  /b ","\"",info[["path"]],"\"",sep="")
	archivos<-shell(ruta2,intern=TRUE) 
	for(j in 1:length(archivos)){
	   
	    archi<-paste("estaciones<-read.table('",info[["path"]],"/",archivos[j],"')",sep="")
	    eval(parse(text=archi))
	   
	    archi1<-paste("store[length(store)+1]<-list(est",j,"=estaciones)",sep="")
	    eval(parse(text=archi1))
	}
	
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
	   
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
     }
  
     if(opt=="trim"){
       Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  
       nam<-list("Invierno","Primavera","Verano","Otoño")
       nameEst<-list("Inv","Prim","Ver","Oto")
       colm<-c("1:3","4:6","7:9","10:12")
       for(a in 70:99){
	   r<-1	   
	  
	   for(b in 1:4){
	       ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
	       eval(parse(text=ejB))
	       ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
	       eval(parse(text=ejDatFr2))
	      
	      
	      if(a>70 & nameEst[b]=="Invierno"){
		  idxInv<-a-1
		  ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
	          eval(parse(text=ejInv))
		 
		 
		  ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
	          eval(parse(text=ejG))
		 
		  ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
	          eval(parse(text=ejU))
	      }
	      
	     
	      ejC <- paste("media<-mean(dat",a,nameEst[b],"estacion[,4]);desviacion<-sd(dat",a,nameEst[b],"estacion[,4]);num.datos<-nrow(dat",a,nameEst[b],"estacion);tc<-qt(0.975,(num.datos-1));intervalo<-tc*(desviacion/sqrt(num.datos-1))",sep="")
	      eval(parse(text=ejC))
	      fill <- paste("lista19",a,"[",r,"]<-c(media)",sep="");eval(parse(text=fill))
	      fill2 <- paste("lista19",a,"[",r+1,"]<-c(desviacion)",sep="");eval(parse(text=fill2)) 
	      fill3 <- paste("lista19",a,"[",r+2,"]<-c(intervalo)",sep="");eval(parse(text=fill3))
	      r=r+3      
	   } 
       }
       estacion.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       
       
       for(id in 1:4){   
          for(e in 1:3){
	      cambName <- paste("names(estacion.Trimestral)[",r2,"]<-","\"",nameEst[id],names(estacion[4]),".",nam2[e],"\"",sep="");eval(parse(text=cambName))  
	      r2=r2+1 
	  }
      }
      
      rownames(estacion.Trimestral)<-años
      ejW2<- paste("write.xlsx(round(estacion.Trimestral[,",colm[1],"],digits=2),file=out,sheetName =\"",nameEst[1],"\")",sep="")
      eval(parse(text=ejW2))
      for(id in 2:4){    
	  ejW2<- paste("write.xlsx(round(estacion.Trimestral[,",colm[id],"],digits=2),file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="")
	  eval(parse(text=ejW2))
      }
     
      indexEst<-c(1,4,7,10)
      indexInt<-c(3,6,9,12)
      info2<-info[["tipop"]]
      ifelse(info2=="maxima",info2<-"máxima",NA)
      ifelse(info2=="minima",info2<-"mínima",NA)
      for(g in 1:4){
     	  x11()
	  titulo<-paste(nam[g]," ",varname,"-",info2)
	  graficar(estacion.Trimestral[,indexEst[g]],titulo," °C")
	  ajusteLin(estacion.Trimestral[,indexEst[g]])
          inter(estacion.Trimestral[,indexEst[g]],estacion.Trimestral[,indexInt[g]])
      }
   }
     else{ 
       for(a in 70:99){
	   r<-1
           ejC <- paste("media<-mean(dat",a,"estacion[,4]);desviacion<-sd(dat",a,"estacion[,4]);num.datos<-nrow(dat",a,"estacion);tc<-qt(0.975,(num.datos-1));intervalo<-tc*(desviacion/sqrt(num.datos-1))",sep="")
	   eval(parse(text=ejC))
           fill <- paste("lista19",a,"[",r,"]<-c(media)",sep="");eval(parse(text=fill))
           fill2 <- paste("lista19",a,"[",r+1,"]<-c(desviacion)",sep="");eval(parse(text=fill2)) 
	   fill3 <- paste("lista19",a,"[",r+2,"]<-c(intervalo)",sep="");eval(parse(text=fill3))
	   r=r+3  
       }
       estacion.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       for(e in 1:3){	
           cambName <- paste("names(estacion.Anual)[",r2,"]<-","\"",names(estacion[4]),".",nam2[e],"\"",sep="")
	   eval(parse(text=cambName))  
	   r2=r2+1 
       }
      
       rownames(estacion.Anual)<-años
       write.xlsx(round(estacion.Anual,digits=2),file=out,sheetName ="Anual")
       
       info2<-info[["tipop"]]
       ifelse(info2=="maxima",info2<-"máxima",NA)
       ifelse(info2=="minima",info2<-"mínima",NA)
       titulo<-paste("Anual ",varname,"-",info2)
       graficar(estacion.Anual[,1],titulo," °C")
       ajusteLin(estacion.Anual[,1])
       inter(estacion.Anual[,1],estacion.Anual[,3])
  }
}

contarDias<-function(estacion,grados,info,signo){
    out<-info[["out"]]
    años<-c(1970:1999)
    ifelse(signo=="mayor",signo<-">",signo<-"<")
    
    for(a in 70:99){
      ejA<-paste("index",a,".estacion<-which(estacion$year%in%19",a,")",sep="");eval(parse(text=ejA))
      ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".estacion,])",sep="");eval(parse(text=ejDatFr))
    
      ejC <- paste("temp<-dat",a,"estacion[,4]",signo,"=grados;temp<-sum(temp)",sep="");eval(parse(text=ejC))
      fill <- paste("lista19",a,"<-temp",sep="");eval(parse(text=fill)) 
    }
    
    tempM.estacion<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
    grados2<-paste(grados," °C")
    colnames(tempM.estacion)<-grados2  
    rownames(tempM.estacion)<-años 
    write.xlsx(tempM.estacion,file=out,sheetName =info[["tipop"]])
    
    info2<-info[["tipop"]]
    ifelse(info2=="maxima",info2<-"máxima",NA)
    ifelse(info2=="minima",info2<-"mí­nima",NA)
    titulo<-paste("Temperatura -",info2)
    graficar(tempM.estacion[,1],titulo,"días")
    ajusteLin(tempM.estacion[,1])
}

buscarGrado<-function(estacion,porc){
    p<-porc/100
    estacion.ord<-cbind(sort(estacion[,4]))
    indxest<-round(length(estacion.ord)*p)
    temp<-round(estacion.ord[indxest],digits=2)
    return(temp)
}

graficar<-function(datos,titulo,ylab){
    plot(c(1970:1999),datos,main=titulo,ylab=ylab,xlab="años",type="b",pch=17,col="blue")
}

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
      dd<-paste(dias,"dí­a (s) - ",valor, "mm/dí­a")
      info2<-"precipitación"
      out<-info[["out"]]
      for(l in 70:99){  
         ejL <-paste("lista19",l,"<-numeric()",sep="")
	 eval(parse(text=ejL))
      }

      for(a in 70:99){  
	   ejA<-paste("index",a,".cuenca<-which(estacion$year%in%19",a,")",sep="")
	   eval(parse(text=ejA))
	   
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
      }

    if(opt=="trimestral"){
       Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  
       nam<-list("Invierno","Primavera","Verano","Otoño")
       nameEst<-list("Inv","Prim","Ver","Oto")
       for(a in 70:99){
	   r<-1
	   
	   for(b in 1:4){
	       ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
	       eval(parse(text=ejB))
	       ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
	       eval(parse(text=ejDatFr2))
	      
	      
	      if(a>70 & nameEst[b]=="Inv"){
		  idxInv<-a-1
		  ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
	          eval(parse(text=ejInv))
		 
		  
		  ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
	          eval(parse(text=ejG))
		  
		  ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
	          eval(parse(text=ejU))
	      }
	      
	      ejC <- paste("Lluvia<-dat",a,nameEst[b],"estacion[,4]>",valor,";Ni<-as.integer(Lluvia)",sep="");eval(parse(text=ejC))
              if(length(Ni)==0){
                     d<-NaN
              }
              else{
	          L<-conjunta(Ni)
                  d<-contarEventos(L,dias)
              }
              fill <- paste("lista19",a,"[",r,"]<-d",sep="");eval(parse(text=fill))
	      r=r+1 
	   } 
       }
       Lluvia.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       
      
       for(t in 1:4){ 
	    cambName <- paste("names(Lluvia.Trimestral)[",t,"]<-","\"",dd,"\"",sep="")
            eval(parse(text=cambName))
       }
       
       rownames(Lluvia.Trimestral)<-años
       write.xlsx(Lluvia.Trimestral[1],file=out,sheetName ="Inv")
       for(id in 2:4){   
	  ejW2<- paste("write.xlsx(Lluvia.Trimestral[",id,"],file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="")
	  eval(parse(text=ejW2))
       }
     
      for(g in 1:4){
     	  x11()
	  titulo<-paste(nam[g]," - ",info2," (",valor,"mm/día)")
	  graficar(Lluvia.Trimestral[,g],titulo,"días")
        ajusteLin(Lluvia.Trimestral[,g])
      }
    }

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
	   }
       }
       Lluvia.Mensual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
      
       for(im in 1:12){ 
           cambName <- paste("names(Lluvia.Mensual)[",rM,"]<-\"",dd,"\"",sep="")
           eval(parse(text=cambName))   
           rM=rM+1
       }
       rownames(Lluvia.Mensual)<-años
       ejW2<- paste("write.xlsx(Lluvia.Mensual[1],file=out,sheetName =\"",mes[1],"\")",sep="")
       eval(parse(text=ejW2))
       for(id in 2:12){    
           ejW2<- paste("write.xlsx(Lluvia.Mensual[",id,"],file=out,append=TRUE,sheetName =\"",mes[id],"\")",sep="")
	   eval(parse(text=ejW2))
      }
      
      for(g in 1:12){
     	  x11()
	  titulo<-paste(mes[g]," - ",info2," (",valor,"mm/días)")
	  graficar(Lluvia.Mensual[,g],titulo,"días")
        ajusteLin(Lluvia.Mensual[,g])
      }
   }
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
       }
       Lluvia.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
      
       colnames(Lluvia.Anual)<-dd   
       rownames(Lluvia.Anual)<-años
       write.xlsx(Lluvia.Anual,file=out,sheetName ="Anual")
     
       titulo<-paste("Anual - ",info2," (",valor,"mm/día)")
       graficar(Lluvia.Anual[,1],titulo,"días")
       ajusteLin(Lluvia.Anual[,1])
    }
}

diasNoLluvia<-function(estacion,opt,valor){
      my.colors = colorRampPalette(c("white", "green", "blue", "purple","red"))
      años<-c(1970:1999);r2<-1
      info2<-"precipitación";out<-info[["out"]]
      for(l in 70:99){  
         ejL <-paste("lista19",l,"<-numeric()",sep="")
	 eval(parse(text=ejL))
      }

      for(a in 70:99){  
	   ejA<-paste("index",a,".cuenca<-which(estacion$year%in%19",a,")",sep="")
	   eval(parse(text=ejA))
	  
	   ejDatFr<-paste("dat",a,"estacion<-data.frame(estacion[index",a,".cuenca,])",sep="")
	   eval(parse(text=ejDatFr))
      }
      if(opt=="trimestral"){
          Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  
	  nam<-list("Invierno","Primavera","Verano","Otoño")
	  nameEst<-list("Inv","Prim","Ver","Oto")
          colm<-c("1:90","91:180","181:270","271:360")
          
	  for(a in 70:99){
	      r<-1;cc<-1;cc2<-90
	     
	      for(b in 1:4){
		  ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
		  eval(parse(text=ejB))
		  ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
		  eval(parse(text=ejDatFr2))
		  
		  
		  if(a>70 & nameEst[b]=="Inv"){
		      idxInv<-a-1
		      ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
		      eval(parse(text=ejInv))
		    
		      ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
		      eval(parse(text=ejG))
		      
		      ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
		      eval(parse(text=ejU))
		  }
		  
		  ejC <- paste("NoLluvia<-dat",a,nameEst[b],"estacion[,4]<",valor,";Ni<-as.integer(NoLluvia)",sep="");eval(parse(text=ejC))
                 if(length(Ni)==0){
                     d<-NaN
                  }
                  else{
		    L<-conjunta(Ni)
		    d<-EventosNoLluvia(L,length(L))
		  }
		 
		
		  fill <- paste("lista19",a,"[",cc,":",cc2,"]<-d",sep="")
		  eval(parse(text=fill))
                  cc=cc+90
                  cc2=cc2+90
	      } 
	  }
	  NoLluvia.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
	 
	  for(id in 1:4){   
	     for(d in 1:90){	
	          cambName <- paste("names(NoLluvia.Trimestral)[",r2,"]<-","\"",d,"\"",sep="");eval(parse(text=cambName)) 
		  r2=r2+1
		}
	    }
	
	  rownames(NoLluvia.Trimestral)<-años 
  
	  write.xlsx(NoLluvia.Trimestral[,1:90],file=out,sheetName ="Inv")
	  for(id in 2:4){            
	      
	      ejW2<- paste("write.xlsx(NoLluvia.Trimestral[,",colm[id],"],file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="");eval(parse(text=ejW2))
	  }
          mm<-as.matrix(NoLluvia.Trimestral)
	
          cc<-1;cc2<-90
	  for(g in 1:4){
     	    windows(width=7,height=7,rescale="fixed")
	    titulo<-paste(nam[g]," - ",info2)
            par(mar=c(5,4,4,6))
            maxx<-max(mm[,cc:cc2],na.rm=TRUE)
            ColorLevels <- seq(0, maxx, length=90)
          
            image(años,c(1:90),mm[,cc:cc2],col=my.colors((maxx+1)),zlim=c(0,maxx),ylab="rango",main=titulo)
            par(mar=c(5,32.5,4,1),new=TRUE)
	    image(1,ColorLevels,matrix(data=ColorLevels,ncol=length(ColorLevels),nrow=1),xlab="",ylab="",xaxt="n",col=my.colors((maxx+1)))
            
            cc=cc+90
            cc2=cc2+90
	    
          }
      }  

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
       }

       NoLluvia.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))
       
       for(d in 1:365){	
	          cambName <- paste("names(NoLluvia.Anual)[",d,"]<-","\"",d,"\"",sep="");eval(parse(text=cambName)) 
       }
       rownames(NoLluvia.Anual)<-años
       write.xlsx(NoLluvia.Anual,file=out,sheetName ="Anual")
       mm<-as.matrix(NoLluvia.Anual)
      
       titulo<-paste("Anual - ",info2)
       par(mar=c(5,4,4,6))
       maxx<-max(mm,na.rm=TRUE)
       ColorLevels <- seq(0, maxx, length=365)
       image(años,c(1:365),mm,col=my.colors((maxx+1)),zlim=c(0,maxx),ylim=c(0,100),ylab="rango",main=titulo)
       par(mar=c(5,32.5,4,1),new=TRUE)
       image(1,ColorLevels,matrix(data=ColorLevels,ncol=length(ColorLevels),nrow=1),xlab="",ylab="",xaxt="n",col=my.colors((maxx+1)))
      }

}

conjunta <- function(Ni) {
  s <- 0 
  L <- rep(0,length(Ni))
 
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
  i <- which(N>90) 
  N[i]=90
  corte <- c(0.5:(dias+0.5))
  h<-hist(N,breaks=corte,plot=FALSE)
  counts<-h[["counts"]]
  return(counts)
}

sy <- function(se,xo,xi) {
   xmean<-mean(xi)
   sumX<-sum((xi-xmean)^2)
   sy<-se*(1+1/length(xi)+((xo-xmean)^2)/sumX)
   sy<-sqrt(sy)
   return(sy)    
}

ajusteAnova<-function(datos,nam,msj){
   x<-c(1970:1999)
   titulo<-paste("Lluvia Acumulada - ",nam," [mm/",msj,"]",sep="")
   ylab<-paste("Precipitación Acumulada [mm/",msj,"]")
   ajuste<-lm(datos~x)
   ss<-summary(ajuste)
   subtitulo<-paste("Y = ",round(ajuste$coe[[1]],digits=4),"+",round(ajuste$coe[[2]],digits=4),"X, [error intercep=",round(ss$coe[1,2],digits=4),", error tenden=",round(ss$coe[2,2],digits=4),"]")
   anov<-anova(ajuste)
   se<- anov$Mean[2]
   syf<-0
   for (ii in (1:30)) {
	syf[ii]<-sy(se,x[ii],x)
   }
   syf<-1.96*syf
   recta<-ajuste$coef[[2]]*x+ajuste$coef[[1]]
   recta1<-recta+syf
   recta2<-recta-syf
   if(msj=="trimestre"){
	limMax <- min(recta2) + 1100 #PAP ... 600 
   	limMin <- min(recta2) - 10
   	ylim <- c(limMin,limMax)
   }
   else{
	limMax <- max(recta1) + 20 #20 PAPALOAPAN
   	limMin <- min(recta2) - 10
   	ylim <- c(limMin,limMax)
   }
   plot(x,datos,main=titulo,type="o",pch=17,xlab="años",ylab=ylab,ylim=ylim)
   lines(x,recta1,col='blue')
   lines(x,recta2,col='blue')
   lines(x,recta,col="red",type="l")
   mtext(subtitulo,side=3)
}

promLluvia<-function(estacion,opt){
     r2<-1;x<-c(1970:1999)
     out<-info[["out"]]
     varname<-"Precipitación"
     
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

  if(opt=="trimestral"){
       Inv<-c(1,2);Prim<-c(3:5);Ver<-c(6:8);Oto<-c(9:11)  
       nam<-list("Invierno","Primavera","Verano","Otoño")
       nameEst<-list("Inv","Prim","Ver","Oto")
       colm<-c("1:3","4:6","7:9","10:12")
       for(a in 70:99){
	   r<-1	   
	   for(b in 1:4){
	       ejB<-paste("index",a,nameEst[b],".cuenca<-which(dat",a,"estacion$mes%in%",nameEst[b],")",sep="")
	       eval(parse(text=ejB))
	       ejDatFr2<-paste("dat",a,nameEst[b],"estacion<-data.frame(dat",a,"estacion[index",a,nameEst[b],".cuenca,])",sep="")
	       eval(parse(text=ejDatFr2))
	     
	      if(a>70 & nameEst[b]=="Invierno"){
		  idxInv<-a-1
		  ejInv<-paste("indexinvierno",idxInv,".cuenca<-which(dat",idxInv,"estacion$mes%in%12)",sep="")
	          eval(parse(text=ejInv))
		 
		  ejG<-paste("dat",idxInv,"invierno.estacion<-data.frame(dat",idxInv,"estacion[indexinvierno",idxInv,".cuenca,])",sep="")
	          eval(parse(text=ejG))
		 
		  ejU<-paste("dat",a,nameEst[b],"estacion<-data.frame(rbind(dat",a,nameEst[b],"estacion,dat",idxInv,"invierno.estacion))",sep="")
	          eval(parse(text=ejU))
	      }
	      
	     
	      ejC <- paste("suma<-sum(dat",a,nameEst[b],"estacion[,4])",sep="")
	      eval(parse(text=ejC))
	      fill <- paste("lista19",a,"[",r,"]<-suma",sep="");eval(parse(text=fill))
	      r=r+1
	   }
       }
       estacion.Trimestral<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       
      
       for(id in 1:4){   
            cambName <- paste("names(estacion.Trimestral)[",id,"]<-","\"",nameEst[id],".LluviaAc\"",sep="");eval(parse(text=cambName))  
	     r2=r2+1 
     }
     
      rownames(estacion.Trimestral)<-x
      ejW2<- paste("write.xlsx(round(estacion.Trimestral[1],digits=2),file=out,sheetName =\"",nameEst[1],"\")",sep="");eval(parse(text=ejW2))
      for(id in 2:4){    
	 ejW2<- paste("write.xlsx(round(estacion.Trimestral[",id,"],digits=2),file=out,append=TRUE,sheetName =\"",nameEst[id],"\")",sep="");eval(parse(text=ejW2))
      }
           
     for(e in 1:4){ 
	 x11()
	 ajusteAnova(estacion.Trimestral[,e],nam[e],"trimestre")
     }
  }
  else{
     for(a in 70:99){
           ejC <- paste("suma<-sum(dat",a,"estacion[,4])",sep="")
	     eval(parse(text=ejC))
           fill <- paste("lista19",a,"<-suma",sep="");eval(parse(text=fill))
           
       }
       estacion.Anual<-data.frame(rbind(lista1970,lista1971,lista1972,lista1973,lista1974,lista1975,lista1976,lista1977,lista1978,lista1979,lista1980,lista1981,lista1982,lista1983,lista1984,lista1985,lista1986,lista1987,lista1988,lista1989,lista1990,lista1991,lista1992,lista1993,lista1994,lista1995,lista1996,lista1997,lista1998,lista1999))      
       cambName <- paste("names(estacion.Anual)[1]<-","\"Lluvia Acumulada\"",sep="");eval(parse(text=cambName))  
       rownames(estacion.Anual)<-x
       write.xlsx(estacion.Anual,file=out,sheetName="Anual")
	 ajusteAnova(estacion.Anual[,1],"Anual","anual") 
  }
}




