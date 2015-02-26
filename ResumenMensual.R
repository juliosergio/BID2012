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
    # print ("=========")
    cdir <- paste0(bdir, "/", cc)
    # print (cdir)
    #>> files <- list.files(cdir)
    #>> files <- files[grep("\\.dat", files)] # Sólo los .dat
    files <- system2("ls", paste0(cdir, "/*e.txt"), stdout=T)
    for (ff in files) {
        bare <- strsplit(ff, "e." , fixed=T)[[1]][1] # Sin "e.txt"
        #>> prefix <- paste0(cdir, "/") # Ya no es necesario
        #>> fnam <- paste0(prefix, ff) # Ya no es necesario
        tt <- tbl_df(read.table(ff, header=T)) #>> fnam)
        tt <- tt %>% 
            group_by(anio, mes) %>%
                summarise(ppAcc=sum(pp), mTmax=mean(tmax), mTmin=mean(tmin))        
        # OPERACIONES (crea nuevo tt)
        postfijo <- "_Rmens.txt"
        newname <- paste0(bare, postfijo)
        # print(newname)
        write.table(tt, newname, row.names=F)
    }
}
