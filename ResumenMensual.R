#====================================
# ResumenMensual.R
#
#   Hace el resumen por (año,mes)
#====================================

# Directorio base
bdir <- "CUENCAS"

cuencas <- list.files(bdir)

for (cc in cuencas) {
    # print ("=========")
    cdir <- paste0(bdir, "/", cc)
    # print (cdir)
    #>> files <- list.files(cdir)
    #>> files <- files[grep("\\.dat", files)] # Sólo los .dat
    files <- system2("ls", paste0(cdir, "/*txt"), stdout=T)
    for (ff in files) {
        bare <- strsplit(ff, "." , fixed=T)[[1]][1] # Sin ".txt"
        #>> prefix <- paste0(cdir, "/") # Ya no es necesario
        #>> fnam <- paste0(prefix, ff) # Ya no es necesario
        tt <- read.table(ff) #>> fnam)
        # OPERACIONES (crea nuevo tt)
        postfijo <- "XXX.txt"
        newname <- paste0(prefix, bare, postfijo)
        # print(newname)
        write.table(tt, newname, row.names=F)
    }
}
