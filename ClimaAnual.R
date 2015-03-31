#====================================
# ClimaAnual.R
#
#   Calcula climatología anual (año)
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
#        YAQ..  BRAV.. SOTO..  STG  LERM-SL LERM-T MOCT.. LAJA  VRD..  PAPL
ord <- c( 10  ,  1  ,   8   ,   7  ,   3   ,   4  ,  5  ,  2   ,  9  ,  6 )
# Reordenamos:
cuencas <- cuencas[ord]

# Haré una gran tabla que incluya todo:
MegaT <- NULL
icuenca <- 1 # el índice (inicial) de la cuenca

for (cc in cuencas) {
    # Directorio c/cuenca:
    cdir <- paste0(bdir, "/", cc)
    
    # Haré tablitas por cuenca
    miniT <- NULL
    
    # Los archivos en cada cuenca que terminan en ".._Ranual.txt":
    files <- system2("ls", paste0(cdir, "/*_Ranual.txt"), stdout=T)
    for (ff in files) { # Uno por estación
        # El nombre del archivo sin el apéndice ".._Ranual.txt"
        bare <- strsplit(ff, "_Ranual." , fixed=T)[[1]][1] # Sin "_Ranual.txt"
        # El nombre de la estación:
        ename <- tail(strsplit(bare, "/", fixed=T)[[1]], 1) # último elemento
        # Se lee el archivo como una tabla de "dplyr"
        tt <- tbl_df(read.table(ff, header=T)) 
        # La operación de resumen, (un renglón)
        tt <- tt %>% 
            summarise(mApp=mean(ppAcc), 
                      sdApp=sd(ppAcc),
                      mmTmax=mean(mTmax),
                      sdTmax=sd(mTmax),
                      mmTmin=mean(mTmin),
                      sdTmin=sd(mTmin))  %>%
            # Agregamos una columna con el nombre de la estación
            mutate(est=as.integer(ename))
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

# Ahora salvaré la tabla en un objeto binario de R: 
fname <- paste0(glob, "/MegaTClimaAnual.RData")
save(MegaT, file=fname)

