#====================================
# PendientesTendMensGrf.R
#
#   Calcula las tendencias
#   en las anomalias mensuales, y
#   grafica por variable (mes, pendiente) para cada
#   una de las cuencas en cuestion
#====================================
library(dplyr)
library(bitops) # operaciones con bits

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTabla.RData")
dirGraf <- paste0(glob, "/GRAFICOS/PendientesMensual/") # Directorio de gráficos
graphics.off()
gpar <- list(mar=c(5.1, 4.1, 4.1, 3.15*4.1), xpd=F)

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")
# Los colores que usaré para cada cuenca
colores <- colors()[c(12,31,35,41,90,100,121,456,496,653)] # diez colores

# Función para graficar: selecciona ya sea "plot" o "lines"
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
#    ya no se leerá de un archivo de texto con read.table
#REMOVED>> MegaT <-  tbl_df(read.table(fname, header=T))
load(fname) # Contiene MegaT generada con   HacerMegaTabla.R
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


ffmod <- function(ddf, v) {
    mm <- lm(ddf[[v]] ~ ddf$anio)
    coef(mm)[2]
}

# +--- ***OTRA ALTERNATIVA*** ---+
# |        usando dplyr          |
fff <- function (x, y) {
    # función sólo en términos 
    # de las columnas para usar
    # con "dplyr"
    mm <- lm(y ~ x)
    coef(mm)[2]
}
# |                              |
# +-FIN ***OTRA ALTERNATIVA***  -+



# Rango de las Y 
yr <- c(-0.1, 0.1)
yr.pp <- c(-2.5, 2.5)

# Se inicializan los plots 
gflag <- 0L
# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "pend_Tnd_PP.pdf") 
gnameTmax <- paste0(dirGraf, "pend_Tnd_Tmax.pdf") 
gnameTmin <- paste0(dirGraf, "pend_Tnd_Tmin.pdf")
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP) 
par(gpar) # los parámetros van por dispositivo
pdf(gnameTmax)
par(gpar)
pdf(gnameTmin) # Este es el que quda activo al principio del ciclo (ultimo)
par(gpar)

# Para cada cuenca:
for (ii in 1:nc) { # varía sobre 1..número de cuencas 
    cc <- cuencas[ii]
    col <- colores[ii] # color para curva de cuenca
    
    # Se extrae la información que nos interesa (se filtra por cuenca)
    tt <- MegaT %>% filter(cuenca==cc)
    
    # Lo partimos por meses
    xx <- split(tt, tt$mes)
    
    # Las quebradas de pendientes, para esta cuenca
    aApp <- sapply(xx, ffmod, "ppAcc")
    aTmax <- sapply(xx, ffmod, "mTmax")
    aTmin <- sapply(xx, ffmod, "mTmin")
    # +----- ***OTRA ALTERNATIVA*** ------+
    # |           usando dplyr            |    
    # |     aquí usaré tt y no xx         |
    ttr <- tt %>% 
        group_by(mes) %>%
        summarise(aApp=fff(anio, ppAcc), 
                  aTmax=fff(anio,mTmax), 
                  aTmin=fff(anio,mTmin))   
    # | compare las columnas mpp, mmTmax  |
    # | y mmTmin, con los vectores        |
    # | aApp, aTmax y aTmin de arriba     |
    # +---FIN ***OTRA ALTERNATIVA***  ----+
    
    #----------
    # primer variable -precipitación-
    #   paso al siguiente dispositivo gráfico: (el primero)
    dev.set(dev.next())
    gii <- 0
    gfun(aApp, main="Pendientes-Tendencia-Precip",
         xlab="meses", ylab="pendiente", 
         col=col, lwd=1, ylim=yr.pp, xaxt="n")
    
    #----------
    # segunda variable -Tmax-
    #   paso al siguiente dispositivo gráfico: (el primero)
    dev.set(dev.next())
    gii <- 1
    gfun(aTmax, main="Pendientes-Tendencia-Tmax",
         xlab="meses", ylab="pendiente", 
         col=col, lwd=1, ylim=yr, xaxt="n")
 
    #----------
    # tercera variable -Tmin-
    #   paso al siguiente dispositivo gráfico: (el primero)
    dev.set(dev.next())
    gii <- 2
    gfun(aTmin, main="Pendientes-Tendencia-Tmin",
         xlab="meses", ylab="pendiente", 
         col=col, lwd=1, ylim=yr, xaxt="n")
}

# +----- ***AUN OTRA ALTERNATIVA MAS*** ------+
# |               usando dplyr                |    
# |     aquí usaré MegaT n no tt y ni xx      |
ttrr <- MegaT %>%
        group_by(cuenca, mes) %>%
        summarise(
            aApp=fff(anio, ppAcc),
            aTmax=fff(anio,mTmax),
            aTmin=fff(anio,mTmin))
# |  la tabla resultante solo hay que         |
# |  separarla con filter o split por cuenca  |
# +-FIN- ***AUN OTRA ALTERNATIVA MAS*** ------+



# Antes de cerrar los dispositivos gráficos se añaden las 
# leyendas
for (ii in 1:3) {
    dev.set(dev.next())
    #>> print(paste0("CUR:", dev.cur()))
    axis(1, at=1:12, lab=Meses, las=2)
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

