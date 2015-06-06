# ===================================================
# JSS:
#    PesosEstaciones.R
# PROPOSITO:
#    Por cuenca, encuentra los pesos asignados a cada
#    estación; Esta información quedará en un archivo
#    PesosEstaciones.txt, en el directorio correspondiente
#    a la cuenca.
# ===================================================
source("PonderaVoronoi.R")
bdir <- "CUENCAS"
cuencas <- list.files(bdir) # orden alfabético, para el caso es suficiente

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    # Archivo con la definición de las estaciones
    fname <- paste0(cdir, "/Estaciones.txt")
    # Las estaciones:
    ee <- read.table(fname, header=T)
    # Geometría de la cuenca:
    fname <- paste0(cdir, "/Geometria.txt")
    gcc <- read.table(fname)
    pesos <- PonderaVoronoi(ee[,2:3],gcc)
    
    # En el proyecto, las estaciones se manejan en un orden 
    # alfanumérico (o numérico en este caso), ese es el orden
    # en que se deberán escribir los pesos de las estaciones
    ii <- order(ee$id)
    # reordenamos
    pesos <- pesos[ii]
    # Lo guardamos en un archivo:
    fname <- paste0(cdir, "/PesoEstaciones.txt")
    write.table(pesos, fname, row.names = F)
}

