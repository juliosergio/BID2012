#====================================
# GraficosClimaAnual.R
#
#   Hace el concentrado de gráficos
#   de clima
#====================================
library(dplyr)

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTClimaAnual.RData")
dirGraf <- paste0(glob, "/GRAFICOS/ClimaAnual/") # Directorio de gráficos

# La gran tabla que incluye "todo":
load(fname) # Contiene MegaT generada con   HacerMegaTClima.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)

# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (cuenca) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by( cuenca ) %>% 
    summarise(aApp = mean(mApp), aTmax = mean(mmTmax), aTmin =mean(mmTmin))


titles <- c(
    "Precipitation Climatology", 
    "Maximun Temperature Climatology",
    "Minimum Temperature Climatology"
)

# Unidades de la escala:
usc <- list("mm" , "°C", "°C")

# Se inicializan los plots 
graphics.off()
# Se abrirán tres dispositivos gráficos (archivos pdf), cuyos
# nombres estarán compuestos por c/variable
gnamePP <- paste0(dirGraf, "ClimaAnual_PP.pdf") 
gnameTmax <- paste0(dirGraf, "ClimaAnual_Tmax.pdf") 
gnameTmin <- paste0(dirGraf, "ClimaAnual_Tmin.pdf")
# Los abriré en tal orden que quede el que me interesa como activo:
pdf(gnamePP) 
pdf(gnameTmax)
pdf(gnameTmin) # Este es el que quda activo al principio del ciclo (ultimo)


# Antes de cerrar los dispositivos gráficos se añaden las 
# leyendas
for (jj in 1:3) { # Un archivo gráfico por variable
    dev.set(dev.next()) # Un dispositivo 
    barplot(MegaT[[1+jj]], main=titles[jj] , 
            names=LETTERS[1:10], xlab="WATERSHEDS", ylab=usc[jj])
    
}
# se cierran todos los dispositivos gráficos:
graphics.off()
