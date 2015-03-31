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
Cl_glob <- "GLOBAL/MegaTClimaAnual.RData"
load(Cl_glob) # Contiene MegaT generada con   ClimaAnual.R

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Los archivos en cada cuenca que terminan en ".._Ranual.txt":
    files <- system2("ls", paste0(cdir, "/*_Ranual.txt"), stdout=T)
    for (ff in files) {
        # El nombre del archivo sin el apéndice ".._Ranual.txt"
        bare <- strsplit(ff, "_Ranual." , fixed=T)[[1]][1] # Sin "_Ranual.txt"
        # El nombre de la estación:
        ename <- tail(strsplit(bare, "/", fixed=T)[[1]], 1) # último elemento
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T)) 
        # Ahora leemos la climatología mensual para esa estación
        # podría filtrar por cuenca y estación, pero como las 
        # estaciones no se repiten, bastará con filtrar por estación:
        tclima <- filter(MegaT, est==ename) # Tiene 1 renglón 
        # La operación de resta, agrupada por (anio: --12 meses--)
        tt <- tt %>% 
            mutate(
                ppAcc =  100*(ppAcc - tclima$mApp)/tclima$mApp, # Esta va en %
                mTmax =  (mTmax - tclima$mmTmax)*10, # Por década
                mTmin =  (mTmin - tclima$mmTmin)*10  # Por década
            )
        # Nuevo nombre del archivo:
        postfijo <- "_AnomAnual.txt"
        newname <- paste0(bare, postfijo)
        # reescribimos la tabla, en el mismo directorio, con el nuevo nombre:
        write.table(tt, newname, row.names=F)
    }
}
