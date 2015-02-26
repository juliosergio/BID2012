#====================================
# ResumenMensual.R
#
#   Hace el resumen por (año,mes)
#====================================
library(dplyr)
# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Los archivos en cada cuenca que terminan en "..e.txt":
    files <- system2("ls", paste0(cdir, "/*e.txt"), stdout=T)
    for (ff in files) {
        # El nombre del archivo sin el apéndice "..e.txt"
        bare <- strsplit(ff, "e." , fixed=T)[[1]][1] # Sin "e.txt"
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T)) 
        # La operación de resumen, agrupada por (anio,mes)
        tt <- tt %>% 
            group_by(anio, mes) %>%
                summarise(ppAcc=sum(pp), mTmax=mean(tmax), mTmin=mean(tmin))  
        # Nuevo nombre del archivo:
        postfijo <- "_Rmens.txt"
        newname <- paste0(bare, postfijo)
        # reescribimos la tabla, en el mismo directorio, con el nuevo nombre:
        write.table(tt, newname, row.names=F)
    }
}
