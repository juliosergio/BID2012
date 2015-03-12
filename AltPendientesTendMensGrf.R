#====================================
# AltPendientesTendMensGrf.R
#
#   Calcula las tendencias
#   en las anomalias mensuales, y
#   grafica por variable (mes, pendiente) para cada
#   una de las cuencas en cuestion
#   --PRESENTACIÓN GRÁFICA ALTERNATIVA--
#====================================
library(dplyr)
library(bitops) # operaciones con bits

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTabla.RData")
dirGraf <- paste0(glob, "/GRAFICOS/PendientesMensual/") # Directorio de gráficos

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")

# +--- ***FUNCIÓN DE MODELO*** ---+
# |        usando dplyr           |
fff <- function (x, y) {
    # función sólo en términos 
    # de las columnas para usar
    # con "dplyr"
    mm <- lm(y ~ x)
    coef(mm)[2]
}
# |                               |
# +-FIN ***FUNCIÓN DE MODELO***  -+


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
# +----- ***USARE ESTA ALTERNATIVA*** ------+
# |               usando dplyr              |    
# |             aquí usaré MegaT            |
ttrr <- MegaT %>%
    group_by(cuenca, mes) %>%
    summarise(
        aApp=fff(anio, ppAcc),
        aTmax=fff(anio,mTmax),
        aTmin=fff(anio,mTmin))
# |  la tabla resultante solo hay que       |
# |  separarla con filter o split           |
# |  por cuenca                             |
# +-FIN- ****USARE ESTA ALTERNATIVA*** -----+

# +-------- Rango de las Y, por variable ----------+
# +--------+-------------+-------------+-----------+
# |        |   precip    |   Tmax      |   Tmin    |
yr <- list(c(-2.5, 2.5), c(-0.1, 0.1), c(-0.1, 0.1))

titles <- c(
    "Pendientes-Tendencia-Precipitación (mm/año)", 
    "Pendientes-Tendencia-Temp. Máx. (°/año)",
    "Pendientes-Tendencia-Temp. Min. (°/año)"
)

# Se inicializan los plots 
graphics.off()
gpar <- list(mfrow=c(12,1), mar=c(0.95, 4.1, 0.1, 2.1))
# gpar <- list(mfrow=c(5,1), mar=c(0.95, 4.1, 0.1, 2.1))

# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "AltPend_Tnd_PP.pdf") 
gnameTmax <- paste0(dirGraf, "AltPend_Tnd_Tmax.pdf") 
gnameTmin <- paste0(dirGraf, "AltPend_Tnd_Tmin.pdf")
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP, width=7, height=10.06) 
par(gpar) # los parámetros van por dispositivo
pdf(gnameTmax, width=7, height=10.06)
par(gpar)
pdf(gnameTmin, width=7, height=10.06) # Este es el que quda activo al principio del ciclo (ultimo)
par(gpar)


# Antes de cerrar los dispositivos gráficos se añaden las 
# leyendas
for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 
    plot(c(0,1), c(0,1), axes=F, xlab="", ylab="", type="n")
    # points(0.5, 0)
    text(0.5, 0.3, titles[jj], cex=1.5)
    
    # Para cada cuenca:
    for (ii in 1:nc) { # varía sobre 1..número de cuencas 
        cc <- cuencas[ii]
        # Nos interesa la información correspondiente a la cuenca
        tt <- ttrr %>% 
            filter(cuenca==cc) %>%
            ungroup %>% 
            select(mes, 2+jj) # El mes y la variable correspondiente
        
        # tit <- if(ii==1) titles[jj] else ""
        
        plot(tt, #>> main=tit,
             ylab=LETTERS[ii], xlab="", type="b", ylim=yr[[jj]], xaxt="n")
        abline (h=0, lty="dotdash", lwd=2)  
        
        if (ii==nc)
            axis(1, at=1:12, lab=Meses, las=2)
    }
    plot(c(0,1), c(0,1), axes=F, xlab="", ylab="CUENCAS", type="n")
    abline(h=0.7)
    legend(-0.05, 0.65, 
           legend=paste0(LETTERS[1:3], ": ", cuencas[1:3]), bty="n")
    legend(0.2, 0.65,
           legend=paste0(LETTERS[4:5], ": ", cuencas[4:5]), bty="n")
    legend(0.50, 0.65,
           legend=paste0(LETTERS[6:7], ": ", cuencas[6:7]), bty="n")
    legend(0.8, 0.65,
           legend=paste0(LETTERS[8:10], ": ", cuencas[8:10]), bty="n")
    #text(0.5, 0.1, "CUENCAS", cex=1.5)  
}
# se cierran todos los dispositivos gráficos:
graphics.off()
