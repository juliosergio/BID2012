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
# Directorio base
bdir <- "CUENCAS"

Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
           "Jul","Ago","Sep","Oct","Nov","Dic")

cuencas <- list.files(bdir)

# Haré una gran tabla que incluya todo:
MegaT <- NULL
icuenca <- 1 # el índice de la cuenca
for (cc in cuencas) {
    #>> print("======")
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Los archivos en cada cuenca que terminan en ".._AnomMens.txt":
    files <- system2("ls", paste0(cdir, "/*_AnomMens.txt"), stdout=T)
    # Haré tablitas por cuenca
    miniT <- NULL
    for (ff in files) {
        # El nombre del archivo sin el apéndice ".._AnomMens.txt"
        bare <- strsplit(ff, "_AnomMens." , fixed=T)[[1]][1] # Sin "_AnomMens.txt"
        # El nombre de la estación:
        ename <- tail(strsplit(bare, "/", fixed=T)[[1]], 1) # último elemento
        #>> print(ename)
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T))
        # Agregamos una columna con el nombre de la estación
        tt <- tt %>% mutate(est=as.integer(ename))
        # Ahora agregamos esta a la mini tabla total de cada cuenca
        miniT <- bind_rows(miniT, tt)
    }
    # La miniT está completa ahora procedamos agregando el índice de la 
    # cuenca
    miniT <- miniT %>% mutate(cuenca=icuenca)
    # Ahora se agrega esta a la mega tabla
    MegaT <- bind_rows(MegaT, miniT)
    icuenca <- icuenca+1 # la siguiente cuenca
}
# Ahora convertir la columna MegaT$cuenca a factor
MegaT$cuenca <- factor(MegaT$cuenca, levels=1:(icuenca-1))
levels(MegaT$cuenca) <- cuencas


