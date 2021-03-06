#====================================
# A_AltPendientesTendMensGrf.R
#
#   Calcula las tendencias
#   en las anomalias mensuales, y
#   grafica por variable (mes, pendiente) para cada
#   una de las cuencas en cuestion
#   --PRESENTACIÓN GRÁFICA ALTERNATIVA--
#====================================
library(dplyr)

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/AltMegaTabla.RData")
dirGraf <- paste0(glob, "/GRAFICOS/PendientesMensual/") # Directorio de gráficos

#YANO>> Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
#YANO>>            "Jul","Ago","Sep","Oct","Nov","Dic")
Meses <- month.abb

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
load(fname) # Contiene MegaT generada con   AltHacerMegaTabla.R
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

# +========== CALCULO DE CORRELACIONES ===========+
# |               Tmax vs Precip                  |
# +===============================================+
ttt <- ttrr %>% ungroup %>%
    group_by(cuenca) %>%
    summarise(corr=cor(aTmax, aApp))

# Escribimos a un archivo
write.csv(ttt, file=paste0(dirGraf, "AltCorr_Tmax_vs_Precip.csv"))


# +-------- Rango de las Y, por variable ----------+
rr <- range(c(range(ttrr$aTmax),range(ttrr$aTmin)))
# +--------+---------------+-----------+----------+---------+
# |        |     precip    |   Tmax    |   Tmin   |   Tmp   |
yr <- list(  c(-0.3, 0.2)  ,    rr     ,  rr      ,   rr    )
# yr <- list(range(ttrr$aApp),    rr     ,  rr      ,   rr    )

titles <- c(
    "", 
    "Maximun Temperature Tendency",
    "Minimum Temperature Tendency",
    ""
)

# Unidades de la escala:
usc <- list( "", "", "", ""
    #expression(paste("% ",  Year^-1)), 
    #expression("°C per decade"),
    #expression("°C per decade"),
    #expression("°C per decade")
)

# Se inicializan los plots 
graphics.off()
# Para la creación de un conjunto de ventanas de graficación,
# mediante los parámetros primitivos de graficación par(fig, new)
# con el siguiente arreglo:
#       +-----------------------------------+ (1,1)
#       |                11  h=0.5/11.1     |
#       +--+-----------------------------+--+
#       |  |              1  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |  |              2  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |  |              3  h=1/11.1    |  |
#       |  +-----------------------------+  |
#       |12:              :              :13|
#       |  |                             |  |
#       |  +-----------------------------+  |
#       |  |             10  h=1.6/11.1  |  |
#       +--+-----------------------------+--+
#    (0,0)
#
# La ventana 10, correspondiente el primer índice del siguiente vector,
# lleva un espacio extra para etiquetas (su altura es del doble)
# Vector de alturas:
Yinc <- c(1.6, rep(1,9))/11.1 
# Las acumulo
Yinc <- Reduce('+', Yinc, accumulate=T)
# orden inverso
Yinc <- Yinc[10:1] # Ys de las 1as. 10 ventanas
# Matriz de definición (10 ventanas):
Mm <- cbind(left=1/15, right=14/15, bottom=c(Yinc[2:10],0), top=Yinc)
# Ahora agregamos las ventanas, superior y laterales, 11, 12 y 13:
Mm <- rbind(Mm, 
      # +--------+--------+--------+--------+
      # | left   | right  | bottom |   top  |
      # +--------+--------+--------+--------+
       c(   0    ,   1    , Yinc[1],    1   ),
       c(   0    ,  1/15  ,   0    , Yinc[1]),
       c(  14/15 ,   1    ,   0    , Yinc[1])
      )
# Mm contiene la definición de las 13 ventanas necesarias. Cada dispositivo
# (archivo) se divide de acuerdo con esa definición.

#>> gpar <- list(mar=c(0.3, 4.1, 0.1, 2.1))
#>> gpar1 <- list(mar=c(3, 4.1, 0.1, 2.1))
gpar <- list(mar=c(0.3, 4.1, 0.17, 4.1))
gpar1 <- list(mar=c(3, 4.1, 0.17, 4.1))

# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "A_AltPend_Tnd_PP.pdf") 
gnameTmax <- paste0(dirGraf, "A_AltPend_Tnd_Tmax.pdf") 
gnameTmin <- paste0(dirGraf, "A_AltPend_Tnd_Tmin.pdf")
gnameTmp <- paste0(dirGraf, "A_AltPend_Tnd_Tmp.pdf")


# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP, width=7, height=9.11) 
#YANO>> par(gpar) # los parámetros van por dispositivo
pdf(gnameTmax, width=7, height=9.11)
#YANO>> par(gpar)
pdf(gnameTmin, width=7, height=9.11) 
#YANO>> par(gpar)
pdf(gnameTmp, width=7, height=9.11) # Combinado de temperaturas

for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 
    # Ahora creamos las ventanas:
    #YANO>> split.screen(Mm) # Divide todo el espacio del dispositivo
    # En la ventana superior (11) va el título:
    #YANO>> screen(11); 
    # Ventana 11
    par(fig=Mm[11,])
    par(gpar)
    plot(c(0,1), c(0,1), ylab="", axes=F, type="n")
    # points(0.5, 0)
    text(0.5, 0.5, titles[jj], cex=1.5)
    
    # Para cada cuenca:
    for (ii in 1:nc) { # varía sobre 1..número de cuencas 
        # La gráfica se dibuja en la ventana correspondiente a la cuenca
        # que son las numeradas de 1:10 (nc)
        #YANO>> screen(ii); 
        # Ventanas 1 a 10 (ii):
        par(fig=Mm[ii,], new=T)
        par(if (ii==nc) gpar1 else gpar)
        cc <- cuencas[ii]
        # Nos interesa la información correspondiente a la cuenca
        tt <- ttrr %>% 
            filter(cuenca==cc) %>%
            ungroup %>% 
            select(mes, 2+jj) # El mes y la variable correspondiente
        
        # tit <- if(ii==1) titles[jj] else ""
        
        plot(tt, #>> main=tit,
             ylab=letters[ii], xlab="", type="b", ylim=yr[[jj]], axes=F,
             frame=T)
        abline (h=0, lty="dotdash", lwd=2)
        grid(lwd=1)
        tics <- c(yr[[jj]][1], 0, yr[[jj]][2])
        axis(4, at=tics, lab=tics, las=2, cex.axis=0.7)
        
        if (ii==nc)
            axis(1, at=1:12, lab=Meses, las=2)
    }
    # Ventana lateral izquierda
    #YANO>> screen(12); 
    # Ventana 12
    #(Por Articulo)--YANO>>par(fig=Mm[12,], new=T)
    #(Por Articulo)--YANO>>par(mar=c(0.1,0.1,0.1,0.1))
    #(Por Articulo)--YANO>>plot(c(0,1), c(0,1), axes=F, type="n")
    #(Por Articulo)--YANO>>text(0.5, 0.5, "WATERSHEDS", srt=90, cex=1.2)
    # Ventana lateral derecha
    #YANO>> screen(13); 
    # Ventana 13
    par(fig=Mm[13,], new=T)
    par(mar=c(0.1,0.1,0.1,0.1))
    plot(c(0,1), c(0,1), axes=F, type="n")
    text(0.5, 0.5, usc[[jj]], srt=90, cex=1.2)
    #YANO>> abline(h=0.7)
    #YANO>> legend(-0.05, 0.65, 
    #YANO>>        legend=paste0(letters[1:3], ": ", cuencas[1:3]), bty="n")
    #YANO>> legend(0.2, 0.65,
    #YANO>>        legend=paste0(letters[4:5], ": ", cuencas[4:5]), bty="n")
    #YANO>> legend(0.50, 0.65,
    #YANO>>        legend=paste0(letters[6:7], ": ", cuencas[6:7]), bty="n")
    #YANO>> legend(0.8, 0.65,
    #YANO>>        legend=paste0(letters[8:10], ": ", cuencas[8:10]), bty="n")
    #YANO>> text(0.5, 0.1, "CUENCAS", cex=1.5)  
}
# El gráfico combinado de temperaturas:
dev.set(dev.next()) # El cuarto dispositivo 
# Ventana 11
par(fig=Mm[11,])
par(gpar)
plot(c(0,1), c(0,1), ylab="", axes=F, type="n")
# points(0.5, 0)
text(0.5, 0.5, titles[4], cex=1.5)

# Para cada cuenca:
for (ii in 1:nc) { # varía sobre 1..número de cuencas 
    # La gráfica se dibuja en la ventana correspondiente a la cuenca
    # que son las numeradas de 1:10 (nc)
    # Ventanas 1 a 10 (ii):
    par(fig=Mm[ii,], new=T)
    par(if (ii==nc) gpar1 else gpar)
    cc <- cuencas[ii]
    # Nos interesa la información correspondiente a la cuenca
    tt <- ttrr %>% 
        filter(cuenca==cc) %>%
        ungroup %>% 
        select(mes, aTmax, aTmin) # El mes y las variables de temperatura
    
    plot(tt$aTmax, 
         ylab=letters[ii], xlab="", type="b", ylim=yr[[4]], 
         axes=F, frame=T)
    lines(tt$aTmin, type="b", pch=20)
    # abline (h=0, lty="dotdash", lwd=2)
    grid(lwd=1)
    # tics <- c(yr[[jj]][1], 0, yr[[jj]][2])
    axis(4, las=2, cex.axis=0.7) # at=tics, lab=tics, 
    
    if (ii==nc)
        axis(1, at=1:12, lab=Meses, las=2)
}
# Ventana lateral izquierda
#YANO>> screen(12); 
# Ventana 12
#(Por Articulo)--YANO>>par(fig=Mm[12,], new=T)
#(Por Articulo)--YANO>>par(mar=c(0.1,0.1,0.1,0.1))
#(Por Articulo)--YANO>>plot(c(0,1), c(0,1), axes=F, type="n")
#(Por Articulo)--YANO>>text(0.5, 0.5, "WATERSHEDS", srt=90, cex=1.2)
# Ventana lateral derecha
#YANO>> screen(13); 
# Ventana 13
par(fig=Mm[13,], new=T)
par(mar=c(0.1,0.1,0.1,0.1))
plot(c(0,1), c(0,1), axes=F, type="n")
text(0.5, 0.5, usc[[4]], srt=90, cex=1.2)


# se cierran todos los dispositivos gráficos:
graphics.off()
