#====================================
# AplicarMannKendallMensual.R
#
#   Calcula los índices de Mann-Kendall
#   en las anomalias mensuales, 
#====================================
library(dplyr)
library(Kendall)

# Directorio de información global
glob <- "GLOBAL" # En este se guardará la MegaTabla
fname <- paste0(glob, "/AltMegaTabla.RData")
dirGraf <- paste0(glob, "/GRAFICOS/PendientesMensual/") # Directorio de gráficos

# Directorio de cuencas:
dirCC <- "CUENCAS/"
source("promCuenca.R")


#YANO>> Meses <- c("Ene","Feb","Mar","Abr","May","Jun",
#YANO>>            "Jul","Ago","Sep","Oct","Nov","Dic")
Meses <- month.abb



# La gran tabla que incluye "todo":
#    ya no se leerá de un archivo de texto con read.table
#REMOVED>> MegaT <-  tbl_df(read.table(fname, header=T))
load(fname) # Contiene MegaT generada con   AltHacerMegaTabla.R
# Averiguamos las cuencas
cuencas <- levels(MegaT$cuenca)
nc <- length(cuencas)


# =================================================
# MÉDULA DE LOS CÁLCULOS
# Agrupemos por (mes, cuenca, anio) y hagamos el 
# resumen de los datos
MegaT <- MegaT %>% 
    group_by(mes, cuenca, anio) %>% 
    summarise(
        ppAcc = promCuenca(cuenca, est, ppAcc), 
        mTmax = promCuenca(cuenca, est, mTmax), 
        mTmin = promCuenca(cuenca, est, mTmin)
    )

ff <- function(ss) {
    # Aplica Mann-Kendall a una serie de tiempo y 
    # extrae sus valores
    w <- MannKendall(ss)
    c(S=w$S, tau=w$tau, var=w$varS, pvalue=w$sl)
}

ll <- list(Prec=NULL, Tmax=NULL, Tmin=NULL)
for (ii in 1:nc) {
    cc <- cuencas[ii]
    for (m in 1:12) {
        tt <- MegaT %>% filter(cuenca==cc, mes==m)
        for (vv in 4:6) { # variables
            mk <- c(cuenca=ii, mes=m, ff(tt[[vv]]))
            ll[[vv-3]] <- rbind(ll[[vv-3]], mk)
        }     
    }
}

for (nn in names(ll)) {
    rr <- ll[[nn]]
    rownames(rr) <- NULL
    write.table(rr, paste0(glob, "/", "MK-", nn, ".txt"), row.names=F)
    # Además filtraremos la tabla por pvalue < 0.05
    rr <- rr[rr[,"pvalue"] < 0.05,]
    write.table(rr, paste0(glob, "/", "MK-", nn, "-FILTERED.txt"), row.names=F)
}




