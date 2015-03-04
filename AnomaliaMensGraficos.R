#====================================
# AnomaliaGraficos.R
#
#   Grafica Resumenes de Anomalias mensuales
#   en gráficos por cuenca.
#   Habrá un gráfico por variable, y mes,
#   * Se hará promedio aritmético por cuenca
#   * Se graficará una serie de tiempo por cuenca
#   Entonces habrá 12 gráficos por variable, con
#   10 curvas cada uno.
#====================================
library(dplyr)
library(bitops) # operaciones con bits

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTabla.txt")
dirGraf <- paste0(glob, "/GRAFICOS/AnomaliasMensual/") # Directorio de gráficos

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")
# Los colores que usaré para cada cuenca
colores <- colors()[c(12,31,35,41,90,100,121,164,456,493)] # diez colores

graphics.off()
# Función para graficar
gfun <- function (...) {
    mask <- as.integer(bitShiftL(1L,gii)) # gi==0,1,..
    if (as.integer(bitAnd(gflag,mask))==0L) {
        #>> print(paste0("PLOT--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        # Se prende el bit 'gi' en la variable global
        gflag <<- as.integer(bitOr(gflag,mask)); 
        plot(..., type="l")
        #>> print(paste0("      gfB:",gflag))
    } else {
        #>> print(paste0("LINES--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        lines(...)   
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

# Para cada mes:
for (mm in 1:12) { # índices de meses
    # Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
    # nombres estarán compuestos por el mes y c/variable
    gnamePP <- paste0(dirGraf, Meses[mm], "_PP.pdf") 
    gnameTmax <- paste0(dirGraf, Meses[mm], "_Tmax.pdf") 
    gnameTmin <- paste0(dirGraf, Meses[mm], "_Tmin.pdf")
    # Los abriré en tal orden que quede el que me interesa como activo:
    pdf(gnamePP) 
    pdf(gnameTmax)
    pdf(gnameTmin) # Este es el que quda activo al principio del ciclo (ultimo)
    
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
        xr <- range(tt$anio) + c(0,31)
        
        #----------
        # primer variable -precipitación-
        #   paso al siguiente dispositivo gráfico: (el primero)
        dev.set(dev.next())
        gii <- 0
        #>> yr <- range(tt$ppAcc)*1.09
        gfun(x=tt$anio, y=tt$ppAcc, main=paste0(Meses[mm], "-Precip(mm)"),
             col=col, lwd=1, xlim=xr, ylim=yr.pp)
        
        #----------
        # segunda variable -Temp.max -
        #   paso al siguiente dispositivo gráfico:
        dev.set(dev.next())
        gii <- 1
        #>> yr <- range(tt$mTmax)*1.09
        gfun(x=tt$anio, y=tt$mTmax, main=paste0(Meses[mm], "-Tmax(gr)"),
             col=col, lwd=1, xlim=xr, ylim=yr.Tmax)
        
        #----------
        # tercera variable -Temp.min -
        #   paso al siguiente dispositivo gráfico:
        dev.set(dev.next())
        gii <- 2
        #>> yr <- range(tt$mTmin)*1.09
        gfun(x=tt$anio, y=tt$mTmin, main=paste0(Meses[mm], "-Tmin(gr)"),
             col=col, lwd=1, xlim=xr, ylim=yr.Tmin)
        
    }
    # Antes de cerrar los dispositivos gráficos se añaden las 
    # leyendas
    for (ii in 1:3) {
        dev.set(dev.next())
        abline(h=0, col="red")
        legend("bottomright", legend=cuencas, col=colores, pch=15)
    }
    # se cierran todos los dispositivos gráficos:
    graphics.off()
}



