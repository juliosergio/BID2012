#----------------------------------------------------
# Construcci�n del paquete de funciones por default
#----------------------------------------------------
#====================================================
# ************* EMPIEZA ENCABEZADO ******************
# ENCABEZADO PARA USAR ESTE CONJUNTO (CAT�LOGO) DE
# FUNCIONES DENTRO DEL SISTEMA "cliMTA" 
# El usuario podr� cambiar las definiciones marcadas
# con (***) y podr� incluso agregar nuevos elementos
# en lo marcado con (***...***). Ninguna otra instruc-
# ci�n deber� ser cambiada, so pena de que cat�logo de
# funciones no se ejecute correctamente en el contexto
# de cliMTA.
# ***************************************************
#
# Auxiliar para entregar los valores en una lista
Values <- function(lst) {
  return (unname(unlist(lst)))
}

# ----------------------------
# DEFINICI�N DE CONSTANTES:

Presentacion <- "                  Procesando cat�logo:" # (***) Introduzca aqu� su propia presentaci�n

indir <- "./" # (***) Directorio inicial para b�squeda de archivos de datos

# Se decide si seleccionar un archivo o todo un directorio
# que contiene un conjunto de archivos a operar sobre ellos.
# A esto le llamaremos cobertura y solo tiene dos elementos
# Aqu� se definen las dos categor�as
#                 +--------------------------+-------------------------+
#                 |    POR ARCHIVOS          |  POR DIRECTORIOS        |
#                 +--------------------------+-------------------------+
cobertura <- 
  data.frame(
      itm=	c(	"POR-ARCHIVOS",		"POR-DIRECTORIOS"	), # (***) 
      txt=	c(	"Operar por ARCH.", 	"Operar por DIRS."	), # (***)
      tipf=	c(	".txt",			".txt"			), # (***) Tipo de archivos uniforme
    row.names=	c(	"archivo", 		"directorio"		), # <OJO: NO CAMBIAR
    stringsAsFactors = FALSE
  )
# Cu�l es el default de este frame?
CoberturaDef <- "archivo" # (***) S�lo dos opciones: archivo o directorio

# N�mero total de operaciones en el paquete:
NFuncs <- 21 # (***) 

# texto de las funciones:
txtF <- c(
  "ESTA ES OP-01",			# 01 (***) 
  "ESTA ES OP-02",			# 02 (***) 
  "ESTA ES OP-03",			# 03 (***) 
  "ESTA ES OP-04",			# 04 (***) 
  "ESTA ES OP-05",			# 05 (***) 
  "ESTA ES OP-06",			# 06 (***) 
  "ESTA ES OP-07",			# 07 (***) 
  "ESTA ES OP-08",			# 08 (***) 
  "ESTA ES OP-09",			# 09 (***) 
  "ESTA ES OP-10",			# 10 (***) 
  "ESTA ES OP-11",			# 11 (***) 
  "ESTA ES OP-12",			# 12 (***) 
  "ESTA ES OP-13",			# 13 (***) 
  "ESTA ES OP-14",			# 14 (***) 
  "ESTA ES OP-15",			# 15 (***) 
  "ESTA ES OP-16",			# 16 (***) 
  "ESTA ES OP-17",			# 17 (***) 
  "ESTA ES OP-18",			# 18 (***) 
  "ESTA ES OP-19",			# 19 (***) 
  "ESTA ES OP-20",			# 20 (***) 
  "ESTA ES OP-21"			# 21 (***...***) Tantos como NFuncs
)

# Nombre de archivo excel de salida por default:
ExcelFile <- "ExlTEMPORAL.xls" #  (***) Default

# Categor�as de la estructura de informaci�n:
InfoCats <-c("cobertura", "path", "tipop", "out")
# Construcci�n de la estructura de informaci�n:
info <- list()
length(info) <- length(InfoCats)
names(info) <- InfoCats
infoDefaults <- c(cobertura[CoberturaDef,"itm"], "", "", ExcelFile)

# ------------------------
# Las variables que se manejar�n:
vars <- c("variable-1", "variable-2","variable-3") # (***) 
# �ndice del default:
indDefvar = 1  # (***) Elegir uno de los �ndices v�lidos de vars
#--------------------------
# Los tipos de operaciones, seg�n cada variables definidas
# anteriormente en "vars"
tipoOp <- list()
tipoOp[[vars[1]]] <- c("Var1-opc1", "Var1-opc2", "Var1-opc3")	# (***) 
tipoOp[[vars[2]]] <- c("Var2-opc1", "Var2-opc2", "Var2-opc3")	# (***) 
tipoOp[[vars[3]]] <- c("Var3-opc1", "Var3-opc2", "Var3-opc3")	# (***...***) agregar un elem. por c/elem en vars

# Se arma una lista lineal por los tipos de operaciones anteriores
tipoOpsList <- Values(tipoOp) # Es un vector c("mensual", "trimestral",... "maxima", ...)

# Selector de operaciones:
# Cada tipoOp tiene acceso a ciertas funciones en el cat�logo de funciones, aqu� se 
# indican ellas:
selector <- list()
length(selector) <- length(tipoOpsList)
names(selector) <- tipoOpsList
# Las siguientes se pueden dar por nombre, o como una lista
# POR NOMBRE:
# selector[["Var3-opc1"]] <- c(1,3,5,10) # (***) una de estas para cada opci�n
# o
# POR LISTA:
selector[1:length(selector)] <- list(
  1:4, 		# Var1-opc1 	(***)
  c(1:5,7:13),	# Var1-opc2 	(***)
  1:13, 	# Var1-opc3 	(***)
  14:18, 	# Var2-opc1 	(***)
  14:18, 	# Var2-opc2 	(***)
  14:21,	# Var2-opc3 	(***)
  c(1,3,5,10),	# Var3-opc1	(***)
  NULL, # >> c(1,21),	# Var3-opc2	(***) Esta la dejo fuera para mostrar como obligo al default
  20:21		# Var3-opc3  	(***...***) Tantos grupos como elementos tenga tipoOpsList
)
#
# 
# ************* TERMINA ENCABEZADO ******************
#====================================================
#
#  ------------ DEFINICIÓN DE LAS FUNCIONES ---------

fff <- function(info) {
  msg <- Reduce(paste, paste(InfoCats, info, sep=":"))
  # msg <- paste("path:", info[["path"]], "cobertura:",info[["cobertura"]], "tipop:", info[["tipop"]], "out:", info[["out"]])
  tkmessageBox(title="INFO", message=msg, icon="info", type="ok")
}

for(i in 1:NFuncs) { #-- Creamos los objetos  'ff.1', 'ff.2', ... 'ff.21' --
  nam <- paste("ff",i, sep=".") # Nombre del la funci�n
  assign(nam, fff)              # Funci�n tipo
}
