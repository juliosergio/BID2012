# ============================================
# JSS:
#   PromediaPond.R
# PROPOSITO:
#   Calcula un promedio ponderado de acuerdo
#   con un vector de pesos
# ===========================================

# Leer vector de pesos global
vpesos <- NULL

leePesos <- function(fname) {
    read.table(fname, header=T)$x
}

# Llamar, p.ej., así
# vpesos <- leePesos("CUENCAS/BRAVO-SAN-JUAN/PesoEstaciones.txt")

PromediaPond <- function(vvals, vp = vpesos) {
    sum(vvals*vpesos)
}

