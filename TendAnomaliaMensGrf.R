#====================================
# TendAnomaliaMensGrf.R
#
#   Calcula y grafica las tendencias
#   en las anomalias mensuales, una por
#   gráfica mes y variable para todas
#   las cuencas
#====================================
library(dplyr)
library(bitops) # operaciones con bits

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTabla.txt")
dirGraf <- paste0(glob, "/GRAFICOS/TendenciasMensual/") # Directorio de gráficos
graphics.off()
gpar <- list(mar=c(5.1, 4.1, 4.1, 3.15*4.1), xpd=F)

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")
# Los colores que usaré para cada cuenca
colores <- colors()[c(12,31,35,41,90,100,121,456,496,653)] # diez colores

# Función para graficar: selecciona ya sea "curve con add=F" o 
# "curve con add=T"
gfun <- function (...) {
    mask <- as.integer(bitShiftL(1L,gii)) # gi==0,1,..
    if (as.integer(bitAnd(gflag,mask))==0L) {
        #>> print(paste0("PLOT--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        # Se prende el bit 'gi' en la variable global
        gflag <<- as.integer(bitOr(gflag,mask)); 
        curve(..., add=F)
        #>> print(paste0("      gfB:",gflag))
    } else {
        #>> print(paste0("LINES--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        curve(..., add=T)   
    }       
}

# Se incializa con:
gflag <- 0L
gii <- 0 # se incrementará: 0,1,2,...

# La gran tabla que incluye "todo":
MegaT <-  tbl_df(read.table(fname, header=T))
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (mes, cuenca, anio) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by(mes, cuenca, anio) %>% 
    summarise(ppAcc = mean(ppAcc), mTmax = mean(mTmax), mTmin =mean(mTmin))
# =================================================

# Formulas:

f1 <- ppAcc ~ anio # La precipitación depende del tiempo
f2 <- mTmax ~ anio # La temp. max depende del tiempo
f3 <- mTmin ~ anio # La temp. min depende del tiempo


# Para cada mes:
for (mm in 1:12) { # índices de meses
    # Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
    # nombres estarán compuestos por el mes y c/variable
    gnamePP <- paste0(dirGraf, Meses[mm], "_Tnd_PP.pdf") 
    gnameTmax <- paste0(dirGraf, Meses[mm], "_Tnd_Tmax.pdf") 
    gnameTmin <- paste0(dirGraf, Meses[mm], "_Tnd_Tmin.pdf")
    # Los abriré en tal orden que quede el que me interesa como activo:
    pdf(gnamePP) 
    par(gpar) # los parámetros van por dispositivo
    pdf(gnameTmax)
    par(gpar)
    pdf(gnameTmin) # Este es el que quda activo al principio del ciclo (ultimo)
    par(gpar)
    
    # Filtraré la información por mes:
    mtt <- MegaT %>% filter(mes==mm)
    #  esto para averiguar los rangos de las variables
    expn <- 1.25
    yr.pp <- range(mtt$ppAcc)*expn
    yr.Tmax <- range(mtt$mTmax)*expn
    yr.Tmin <- range(mtt$mTmin)*expn
    
    # Se inicializan los plots para cada mes
    gflag <- 0L
    for (ii in 1:nc) { # varía sobre 1..número de cuencas 
        cc <- cuencas[ii]
        col <- colores[ii] # color para curva de cuenca
        # Se extrae la información que nos interesa (se filtra por cuenca)
        tt <- mtt %>% filter(cuenca==cc)
        # Rango de las X con espacio para leyenda
        xr <- range(tt$anio) + c(0,1)
        
        #----------
        # primer variable -precipitación-
        #   paso al siguiente dispositivo gráfico: (el primero)
        dev.set(dev.next())
        gii <- 0
        #>> yr <- range(tt$ppAcc)*1.09
        # Ajuste lineal:
        ff1 <- function(x) predict(lm(f1, data=tt), 
                                   newdata=data.frame(anio=x),
                                   type="response")
        gfun(ff1, main=paste0(Meses[mm], "-Tendencia-Precip(mm)"),
             col=col, lwd=1, xlim=xr, ylim=yr.pp)
        
        #----------
        # segunda variable -Temp.max -
        #   paso al siguiente dispositivo gráfico:
        dev.set(dev.next())
        gii <- 1
        #>> yr <- range(tt$mTmax)*1.09
        # Ajuste lineal:
        ff2 <- function(x) predict(lm(f2, data=tt), 
                                   newdata=data.frame(anio=x),
                                   type="response")
        gfun(ff2, main=paste0(Meses[mm], "-Tendencia-Tmax(gr)"),
             col=col, lwd=1, xlim=xr, ylim=yr.Tmax)
        
        #----------
        # tercera variable -Temp.min -
        #   paso al siguiente dispositivo gráfico:
        dev.set(dev.next())
        gii <- 2
        #>> yr <- range(tt$mTmin)*1.09
        # Ajuste lineal:
        ff3 <- function(x) predict(lm(f3, data=tt), 
                                   newdata=data.frame(anio=x),
                                   type="response")
        gfun(ff3, main=paste0(Meses[mm], "-Tendencia-Tmin(gr)"),
             col=col, lwd=1, xlim=xr, ylim=yr.Tmin)
        
    }
    # Antes de cerrar los dispositivos gráficos se añaden las 
    # leyendas
    for (ii in 1:3) {
        dev.set(dev.next())
        # segments(xr[1], 0, xr[2], 0, lty="dotdash", lwd=2, col="navyblue")
        abline (h=0, lty="dotdash", lwd=2, col="navyblue")
        par(xpd=T)
        legend("topright", inset=c(-0.71,0.2), 
               legend=cuencas, col=colores, pch=15,
               title="Cuencas", bty="n")
        par(xpd=F)
    }
    # se cierran todos los dispositivos gráficos:
    graphics.off()
}
