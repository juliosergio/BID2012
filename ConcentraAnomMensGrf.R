#====================================
# ConcentraAnomMensGrf.R
#
#   Concentra en un gráfico por
#   cuenca y mes, las las anomalias mensuales para
#   las estaciones de la cuenca (por variable).
#====================================
library(dplyr)
library(bitops) # operaciones con bits
# Directorio de información global
glob <- "GLOBAL" # Éste contiene la MegaTabla
fname <- paste0(glob, "/MegaTabla.RData")
# Directorio base de cuencas:
bdir <- "CUENCAS/"

#YANO>> Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
#YANO>>            "Jul","Ago","Sep","Oct","Nov","Dic")
Meses <- month.abb

# La gran tabla que incluye "todo":
load(fname) # Contiene MegaT generada con   AltHacerMegaTabla.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# Los colores que usaré para cada estación
colores <- colors()[c(12,31,35,41,90,100,121,456,496,653)] # diez colores
# Función para graficar: selecciona ya sea "plot" o "lines"
gfun <- function (...) {
    mask <- as.integer(bitShiftL(1L,gii)) # gi==0,1,..
    if (as.integer(bitAnd(gflag,mask))==0L) {
        #DBG> print(paste0("PLOT--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        # Se prende el bit 'gi' en la variable global
        gflag <<- as.integer(bitOr(gflag,mask));
        plot(...) #>>, type="l")
        #DBG> print(paste0(" gfB:",gflag))
    } else {
        #DBG> print(paste0("LINES--- m:",mask," gfA:",gflag," dv:",dev.cur()))
        lines(...)
    }
}
# Se incializa con:
gflag <- 0L
gii <- 0 # se incrementará: 0,1,2,...
graphics.off()
gpar <- list(mar=c(5.1, 4.1, 4.1, 1.10*4.1), xpd=F)

# Nombres para gráficos por variable
gn <- c("PP", "Tmax", "Tmin")
# Títulos por variable
gt <- c("Precipitation", "Max Temp", "Min Temp")
uu <- c("mm", "°C", "°C")

# rango en x
xr <- c(1970,2010)


# Nos vamos por cuencas
for (cc in cuencas) {
    dname <- paste0(bdir, cc, "/") # dir de trabajo
    # hacemos ciclo también por meses
    for (mm in 1:12) {
        prefix <- paste0(dname, "ConcAnom_", mm, "_")
        # nos queda tabla con las estaciones de dicha cuenca:
        ttt <- MegaT %>% filter(cuenca==cc, mes==mm)
        # Cuáles son las estaciones de dicha cuenca?
        Ests <- unique(ttt$est)
        # Para cada variable (un gráfico por variable)
        gflag <- 0L # Una nueva serie de dispositivos
        for (ii in 1:3) {
            # rango de las Y
            yr <- range(select(ttt, 2+ii)) # sólo la variable
            # nombre del dispositivo:
            ndis <- paste0(prefix, gn[ii], ".pdf")
            # se abre dispositivo:
            pdf(ndis)
            par(gpar) # los parámetros van por dispositivo
            # Un nuevo dispositivo:
            gii <- ii - 1 # van de 0 a 2 en este caso
            # una curva en el gráfico por Estación
            tit <- paste0(gt[ii], ":", Meses[mm])
            stit <- paste0("Cuenca: ", cc)
            for (jj in 1:length(Ests)) {
                ee <- Ests[jj]
                # La subtabla correspondiente:
                # (las variables empiezan a partir de col 3 de ttt)
                st <- ttt %>% filter(est==ee) %>%
                    select(anio, 2+ii)
                gfun(st, type="b", xlab="years", ylab=uu[ii], 
                     main= tit, sub=stit, pch=16, col=colores[jj],
                     xlim=xr, ylim=yr
                     )
            }
            # Las leyendas:
            par(xpd=T)
            legend("topright", inset=c(-0.17,0.2),
                   legend=Ests, col=colores, pch=16,
                   title="Estaciones", bty="n")
            par(xpd=F)
            # se cierra dispositivo gráfico
            dev.off()
        }
        
    }
}



