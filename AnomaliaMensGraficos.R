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

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/MegaTabla.txt")

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")


# La gran tabla que incluye "todo":
MegaT <-  tbl_df(read.table(fname, header=T))


