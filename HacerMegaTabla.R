#====================================
# HacerMegaTabla.R
#
#   Hace y guarda como archivo una
#   "MegaTabla" con toda la información
#   del proy.
#====================================
library(dplyr)
# Directorio base
bdir <- "CUENCAS"

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla

cuencas <- list.files(bdir) # orden alfabético
# Nececitamos ordenar las cuencas de acuerdo con cierta ubicación 
# geográfica

# Para el orden correcto:
#          1      2      3      4      5      6     7     8       9     10
#        BRAV.. LAJA  LERM-SL LERM-T MOCT.. PAPL.. STG.. SOTO..  VRD.. YAQ..
# ---------------
source("ord.txt") # Establece variable "ord" con el orden correcto
# Reordenamos:
cuencas <- cuencas[ord]

# Haré una gran tabla que incluya todo:
MegaT <- NULL
icuenca <- 1 # el índice (inicial) de la cuenca
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

# Ahora se procedía a guardar la MegaTabla en un archivo de texto, 
# ya no lo haré así puesto que no respeta el orden de los levels en 
# los factores: 
#REMOVED>> fname <- paste0(glob, "/MegaTabla.txt")
#REMOVED>> write.table(MegaT, fname, row.names=F)

# Ahora salvaré la tabla en un objeto binario de R: 
fname <- paste0(glob, "/MegaTabla.RData")
save(MegaT, file=fname)

