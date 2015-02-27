#====================================
# IdentificaMesesFaltantes.R
#
#   Identifica los meses faltantes en
#   los datos. Entrega una tabla con los
#   meses faltantes por año (anio, mes)
#====================================
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
        tt <- read.table(ff, header=T)
        # Los meses de cada año que no están en los datos:
        ww <- by(
            tt[,1:2], # cols (anio,mes)
            tt$anio,  # separados por años
            function(e) {
                dd=setdiff(1:12, e$mes) # diferencia de conjuntos
                data.frame(
                    anio=rep(e$anio, length.out=length(dd)),
                    mes=dd
                )
            }
        )
        # La nueva tabla será aplastar toda la estructura anterior en 
        # una sola tabla, así:
        nt <- do.call(rbind, ww)
        # Nuevo nombre del archivo:
        postfijo <- "_MesesFaltan.txt"
        newname <- paste0(bare, postfijo)
        # reescribimos la tabla, en el mismo directorio, con el nuevo nombre:
        write.table(nt, newname, row.names=F)
    }
}
