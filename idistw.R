# =========================
# idistw:
#   Función para interpolar mediante el método
#   del inverso de la distancia.
# ========================

# ===
# Función para el cálculo de distancia euclidiana
# entre dos puntos - los puntos serán simplemente 
# dos vectores numéricos de la misma longitud
dstE <- function (p1, p2, n=2) {
    # p1, p2: puntos
    # n: potencia a la que se eleva la distancia
    #    a la salida
    #----
    # suma de diferencias cuadradas
    Sp <- sum((p1 - p2)^2)
    if (n==2) return(Sp)
    # exponente para todas las otras potencias:
    e <- n/2
    return (Sp^e)
}

pondera <- function(vpon, vvals) {
    # función auxiliar de ponderación
    ii <- which(vpon==0) 
    if (length(ii) != 0) return(vvals[ii[1]])
    # vector de pesos
    vpon <- 1/vpon # inverso
    sum(vpon*vvals)/sum(vpon)
}

idistw <- function(where, at, f_at, pow=2, fdist=dstE, ...) {
    # Función para calculo de promedio ponderado por el
    # inverso de la distancia
    # where: conjunto de puntos donde se quiere la interpolación
    #        dados como una matriz, cuyas columnas son las coordenadas
    #        y cuyos renglones representan cada uno de los puntos
    #        individuales.
    # at:    matriz de puntos para los que se conoce o se puede 
    #        obtener el valor de la función que se quiere 
    #        interpolar. El formato es semejante al de 'where'
    #        aunque el número de renglones (o puntos pues) es
    #        diferente.
    # f_at:  Los valores de la función a interpolar, en cada uno de
    #        los puntos dados en 'at'. Aquí se puede especificar 
    #        de dos maneras: como una función que recibe como entrada
    #        un punto y arroja el valor correspondiente o simplemente
    #        como un vector de valores numéricos cuya longitud es
    #        igual al número de renglones (o puntos) de 'at'.
    # pow:   Potencia por la que se afecta el inverso de la distancia,
    #        (o sea las distancias se elevarán a este exponente antes
    #        de calcular los pesos de la ponderación).
    # fdist: función para el cálculo de la distancia, si se quiere
    #        una diferente que la euclidiana.
    # ...    argumentos extras de fdist en su caso
    #---------------
    # valores de la función
    fvals <- if(is.numeric(f_at)) f_at else apply(at, 1, f_at)
    # Uniformizamos sintaxis de fdist
    fdist <- if(identical(fdist,dstE)) dstE else {
        function(p1, p2, n) fdist(p1, p2,...)^n
    }
    # Calculo de distancias ---
    # Simplificamos función para que solo dependa de los puntos
    # en where
    ff <- function(p) apply(at, 1, function(p2) fdist(p, p2, pow))
    # Cada punto tendrá tantas distancias como elementos haya en 'at'
    # -- matriz de distancias ---
    Dists <- apply(where, 1, ff) 
    # Aquí, las distancias vienen por columnas, es decir, al primer
    # punto de where le corresponde la columna 1, y así sucesivamt
    apply(Dists, 2, pondera, fvals)
}

test <- function() {
    A <- as.matrix(read.table("tstA.txt"))
    B <- as.matrix(read.table("tstB.txt"))
    idistw(B, A, f_at=c(3,4,5,6,7,8,9,10,11))
}