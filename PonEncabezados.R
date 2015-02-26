#====================================
# PonEncabezados.R
#
#   Poner encabezados a cada uno de
#   los archivos de datos;
#      archivos de tipo .dat
#   se cambiarán a tipo .txt pero
#   con encabezados.
#====================================

# Los encabezados son comunes para todos, 
hd <- c("anio", "mes", "dia", "pp", "tmax", "tmin")

# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # print ("=========")
    cdir <- paste0(bdir, "/", cc)
    # print (cdir)
    files <- list.files(cdir)
    files <- files[grep("\\.dat", files)] # Sólo los .dat
    for (ff in files) {
        bare <- strsplit(ff, "." , fixed=T)[[1]][1] # Sin ".dat"
        prefix <- paste0(cdir, "/")
        fnam <- paste0(prefix, ff)
        # Se abre cada tabla (no traen header)
        tt <- read.table(fnam)
        # agregamos header
        names(tt) <- hd
        # print(head(tt,3))
        # Va de regreso a archivo, pero ahora .txt con headers
        newname <- paste0(prefix, bare, "e.txt")
        # print(newname)
        write.table(tt, newname, row.names=F)
    }
}
