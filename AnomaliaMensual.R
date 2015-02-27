#====================================
# AnomaliaMensual.R
#
#   Calcula Anomalias mensuales:
#   restamos medias de la climatologia a 
#   cada tabla de resumen mensual
#====================================
library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Los archivos en cada cuenca que terminan en ".._Rmens.txt":
    files <- system2("ls", paste0(cdir, "/*_Rmens.txt"), stdout=T)
    for (ff in files) {
        # El nombre del archivo sin el apéndice ".._Rmens.txt"
        bare <- strsplit(ff, "_Rmens." , fixed=T)[[1]][1] # Sin "_Rmens.txt"
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T)) 
        # Ahora leemos la climatología mensual
        postfijo <- "_ClimaMens.txt"
        fclima <- paste0(bare, postfijo)
        tclima <- read.table(fclima, header=T) # Tiene 12 renglones 
        # La operación de resta, agrupada por (anio: --12 meses--)
        tt <- tt %>% 
            group_by(anio) %>%
            mutate(
                ppAcc =  ppAcc - tclima$mApp, 
                mTmax =  mTmax - tclima$mmTmax,
                mTmin =  mTmin - tclima$mmTmin
            )  
        # Nuevo nombre del archivo:
        postfijo <- "_AnomMens.txt"
        newname <- paste0(bare, postfijo)
        # reescribimos la tabla, en el mismo directorio, con el nuevo nombre:
        write.table(tt, newname, row.names=F)
    }
}
