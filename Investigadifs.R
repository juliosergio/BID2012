#====================================
# InvestigaDifs.R
#
#   Investiga diferencias en 
#   listados de estaciones
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
    names <- as.integer(sapply(files, function(ff) strsplit(ff, ".", fixed=T)[[1]][1]))
    tte <- read.table(paste0(cdir,"/Estaciones.txt"), header=T)
    tte <- tte[order(tte$id),]
    print ("=================================================")
    print (cc)
    print ("--Tabla:")
    mask <- tte$id %in% names
    print(tte$id[!mask])
    print("--Datos:")
    mask <- names %in% tte$id
    print(names[!mask])
}
