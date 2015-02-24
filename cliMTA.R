#-----------------------------------------
# CliMTA v 1.0
# FECHA: 20-Agosto-2012
# AUTORES: Julio Sergio Santana
#          Efraín Mateos Farfán
# Con la colaboración de: 
#          Aura Ivette 
# PROPÃ“SITO:
#	Sistema para la "transformación" de archivos de datos
#	por medio de la selección de entre un conjunto de operaciones
#	dadas como un catálogo de funciones en R
#------------------------------------------
require("tcltk")
require("tcltk2")
# Variables de aspecto:
fontHeading <- tkfont.create(family="times",size=40,weight="bold",slant="italic")
fontHeading1<-tkfont.create(family="times",size=20,weight="bold",slant="italic")
fontHeading2<-tkfont.create(family="times",size=14,weight="bold")
fontTextLabel <- tkfont.create(family="times",size=12)
fontFixedWidth <- tkfont.create(family="courier",size=12)

# Auxiliar para entregar los valores en una lista
Values <- function(lst) {
  return (unname(unlist(lst)))
}

# Función para terminar
done<-function(){
  tkgrab.release(tt)
  tkdestroy(tt)
  # rm(tt)
}

# funciones de utilería para avanzar en renglones y columnas
right <- function(v) {
  return( as.integer(v+c(0,1)) )
}

down <- function(v) {
  return( as.integer(c(v[1]+1,0)) )
}

# FUNCION DE UTILERÍA PARA CAPTURAR STRINGS:
CapturaString <- function(title,question,entryInit,entryWidth=25,returnValOnCancel=NULL)
{
  onOK <- function()
  {
    ReturnVal <<- tclvalue(textEntryVarTcl)
    tkgrab.release(dlg)
    tkdestroy(dlg)
    if(exists("tt")) tkfocus(tt)
  }
  onCancel <- function()
  {
    ReturnVal <<- returnValOnCancel
    tkgrab.release(dlg)
    tkdestroy(dlg)
    if(exists("tt")) tkfocus(tt)
  }
  dlg <- tktoplevel()
  # tkwm.deiconify(dlg)
  # tkgrab.set(dlg)
  # tkfocus(dlg)
  tkwm.title(dlg,title)
  textEntryVarTcl <- tclVar(paste(entryInit))
  textEntryWidget <- tkentry(dlg,width=paste(entryWidth),textvariable=textEntryVarTcl)
  tkgrid(tklabel(dlg,text="       "))
  tkgrid(tklabel(dlg,text=question),textEntryWidget,padx=5)
  tkgrid(tklabel(dlg,text="       "))
  ReturnVal <- returnValOnCancel
  OK.but     <-tkbutton(dlg,text="    OK    ",command=onOK)
  Cancel.but <-tkbutton(dlg,text=" Cancelar ",command=onCancel)
  tkgrid(OK.but,Cancel.but, sticky="nsew",padx=5)
  tkgrid(tklabel(dlg,text="    "))

  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkdestroy(dlg);if(exists("tt")) tkfocus(tt)})
  tkbind(textEntryWidget, "<Return>", onOK)
  tkfocus(dlg)
  tkwait.window(dlg)  
  return(ReturnVal)
}

DespliegaTxt <- function(title,txt) {
  # Despliega el texto contenido en el arreglo de strings "txt"
  onOK <- function()
  {
    tkgrab.release(dlg)
    tkdestroy(dlg)
    if(exists("tt")) tkfocus(tt)
  }
  dlg <- tktoplevel()
  tkwm.title(dlg,title)
  c <- tk2frame(dlg, relief="sunken", borderwidth=10, padding=c(6,6,6,6))
  scr <- tk2scrollbar(c,  orientation="vertical", command=function(...)tkyview(Mtxt,...))
  Mtxt <- tk2text(c,bg="white", yscrollcommand=function(...)tkset(scr,...))
  for (line in txt) {
    tkinsert(Mtxt,"end",paste(line,"\n"))
    # print(paste("Inserted:", line))
  }
  tkgrid(Mtxt,scr)
  tkgrid.configure(scr,sticky="ns")
  tkconfigure(Mtxt, state="disabled")
  # tkgrid(tklabel(c, text="       "))
  OK.but     <-tk2button(c,text="    OK    ",command=onOK)
  tkgrid(OK.but, sticky="nsew", padx=250)
  # tkgrid(tklabel(c, text="       "))  
  tkgrid(c, column=0, row=0, sticky="nsew")
  tkfocus(Mtxt)
  tkbind(dlg, "<Destroy>", function() {tkgrab.release(dlg);tkdestroy(dlg);if(exists("tt")) tkfocus(tt)})
  tkfocus(dlg)
  tkwait.window(dlg)
}

# Establecimiento del la base de funciones:
FuncsNam <- CapturaString("BASE DE FUNCIONES", "Su catálogo de funciones:", "defaultFuncts.R", returnValOnCancel="defaultFuncts.R")
source(FuncsNam)

# Para la estructura de información
# FileName   <-  tclVar("")
# cobertura  <-  tclVar("estacion")
# tsel       <-  tclVar("")
# ExlOut     <-  tclVar(ExcelFile)
tclvarsInfo <- lapply(infoDefaults, tclVar)
names(tclvarsInfo) <- InfoCats

op         <-  tclVar("")
vsel       <-  tclVar(vars[indDefvar])

# ALGUNAS DEPENDENCIAS DEL SISTEMA OPERATIVO
if (Sys.info()[["sysname"]] == "Linux") {
  ExcelPrgm <- "soffice" # Aquí poner la instrucción que llama a intérprete para xls
  syscmd <- system
} else {
  # Para GÃ¼indous:
  ExcelPrgm <- paste('"', readLines("exlpath.txt"), '"', sep="")
  syscmd <- shell
}

PrgmTitle <- "    cliMTA-r v 1.0    "

# ---------
# FUNCIONES CORRESPONDIENTES A CADA UNA DE LAS OPERACIONES
# Función transformadora:
trnsFunct <- function(f,a) {
  return(function() {f(a)})
}

# Funcion selectora
Selecciona <- function(s) {
  # Si no se ha definido el selector correspondiente a una opción,
  # se asume, por default, que todas las operaciones aplican
  ss <- selector[[s]]                          # selector - definido en encabezado de catálogo
  return (if (is.null(ss)) 1:NFuncs else ss)	# NFuncs   - definido en encabezado de catálogo
}

getfileN <- function() {
  # Esta función obtiene el nombre del archivo sobre el que se desea operar
  f <- tclvalue(tclvarsInfo[[2]])
  if (f == "") {
    tkmessageBox(title="ATENCIÃ“N!",message="Seleccione su archivo por favor",icon="warning",type="ok")
    return (NULL)
  }
  return (f)
}

# -----------------------------
# Las siguientes son las funciones correspondientes a cada una de
# las operaciones:

ff1 <- function() {
  if (! is.null(f <- getfileN())) {  
    msg <- paste("Esta es la operación UNO en el archivo:", f)
    tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
    syscmd(paste(ExcelPrgm, ExcelFile))
  }
}

ff2 <- function() {
  if (! is.null(f <- getfileN())) {  
    msg <- paste("Esta es la operación DOS en el archivo:", f)
    tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
    plot(c(1,2,3,4,5))
  }
}

ff3 <- function() {
  if (! is.null(f <- getfileN())) {  
    msg <- paste("Esta es la operación TRES en el archivo:", f)
    tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
  }
}

ff4 <- function() {
  if (! is.null(f <- getfileN())) {  
    msg <- paste("Esta es la operación CUATRO en el archivo:", f)
    tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
  }
}

# -----------------
# Funcioncitas de prueba

fff.in <- function() {
  msg <- "ENTRÃ‰!!"
  tkmessageBox(title="INFO", message=msg, icon="info", type="ok")
}

fff.out <- function() {
  msg <- "SALÃ!!"
  tkmessageBox(title="INFO", message=msg, icon="info", type="ok")
}

fff.msg <- function(msg) {
  tkmessageBox(title="INFO", message=as.character(msg), icon="info", type="ok")
}

# ---------------------------------------------------------------------------
#          ----------- TABLA DE FUNCIONES -----------------


# Armamos el arreglo de funciones:
FuncsArr <- list()
# Primero crearemos las funciones


for(i in 1:NFuncs) { #-- Asignamos los objetos  'ff.1', 'ff.2', ... 'ff.NFuncs' --
  nam <- paste("ff",i, sep=".") # Nombre del la función
  # -------
  # Creamos una función "vacía"
  ff <-function () {}
  # Creamos el body de la función
  ee <- substitute(
	  {
	    if (! is.null(f <- getfileN())) {
	      # msg <- paste("Esta es la operación", ii, "en el archivo:", f)
	      # tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
	      # Llamamos a la función localizada:
	      get(nf)(info)
	    }
	  },
	  list(ii=as.character(i),nf=nam)
        )
  # Le asignamos ese body a la función creada
  body(ff) <- as.call(ee)
  FuncsArr[[i]] <- ff # Ahora lo metemos al arreglo de funciones
}


# FuncsArr <- c(ff1,ff2,ff3,ff4)
# Asociamos el arreglo con los nombres de las opciones (operaciones:"vals")
# names(FuncsArr) <- vals
#----------------------------------------------------------------------------

LaunchFileSel <- function() {
  # Funcion que dispara un seleccionador de archivos
  # tclvalue(FileName) <- ""
  if (tclvalue(tclvarsInfo[[1]]) == cobertura["archivo", "itm"]) {
    tipos <- paste("{{Arch de datos:", cobertura["archivo", "itm"], "} {", cobertura["archivo","tipf"], "}} {{Todos los archivos} *}", sep="")
    tclvalue(tclvarsInfo[[2]]) <- tclvalue(tkgetOpenFile(initialdir=indir, filetypes=tipos, title="Elija archivo"))
  }
  else { # directorio
    tclvalue(tclvarsInfo[[2]]) <- tclvalue(tkchooseDirectory(initialdir=indir, mustexist="true", title="Elija directorio:"))
  }
}

LaunchFileSaveSel <- function() {
  # Funcion que dispara un seleccionador de archivos
  # tclvalue(FileName) <- ""
  tipos <- "{{Arch de Excel} {.xls .xlsx}} {{Todos los archivos} *}"
  s <- tclvalue(tkgetSaveFile(initialdir=indir, filetypes=tipos, title="Elija archivo"))    
  # Solo se cambia el valor si se ha seleccionado algo:
  if (s != "") tclvalue(tclvarsInfo[[4]]) <- s
}

ExOp <- function() {
  if ((s <- tclvalue(tkget(OpCB))) == "") {
    tkmessageBox(title="ATENCIÃ“N!",message="Seleccione su operación por favor",icon="warning",type="ok")
    return ()
  }
  if (! is.null(getfileN())) {    
    # Aquí llenamos la estructura de información
    # info[["path"]] <<- f
    # info[["cobertura"]] <<- tclvalue(cobertura)
    # info[["tipop"]] <<- tclvalue(tsel)
    # info[["out"]] <<- tclvalue(ExlOut)
    # ----
    # info[1:length(info)] <<- c(f, tclvalue(cobertura), tclvalue(tsel), tclvalue(ExlOut))
    info[1:length(info)] <<- lapply(tclvarsInfo, tclvalue)
    i <- as.integer(tclvalue(tcl(.Tk.ID(OpCB), "current"))) + 1
    s <- tclvalue(tclvarsInfo[[3]])
    # msg <- txtF[selector[[s]][i]]
    # tkmessageBox(title="INFO", message=paste("Se seleccionó:>>", msg, "<<"))
    #---
    # El índice de la función a llamar es:
    ind <- Selecciona(s)[i]
    ff <- FuncsArr[[ind]] # La función real
    # Llamamos a la función:
    ff()
    # Se ejecuta programa para desplegar el archivo de excel producido:
    syscmd(paste(ExcelPrgm, info[[4]]), wait=FALSE)
    # msg <- paste("Ejecutando->", OpChoice, " en el archivo:", tclvalue(FileName))
    # tkmessageBox(title="Operación en ejecución",message=msg,icon="info",type="ok")
  }
}

SeSeleccionoVar <- function() {
  s <- tclvalue(tkget(varsel))
  # msg <- paste("Se seleccionó:", s)
  # tkmessageBox(title="ATENCION:", message=msg, icon="info", type="ok")
  tclvalue(tclvarsInfo[[3]]) <- ""
  tclvalue(op) <- ""
  tkconfigure(OpCB, values="")
  tkconfigure(tipsel, values=tipoOp[[s]]) 
}

SeSeleccionoTip <- function() {
  s <- tclvalue(tkget(tipsel))
  # msg <- paste("Se seleccionó:", s)
  # tkmessageBox(title="ATENCION:", message=msg, icon="info", type="ok")
  tclvalue(op) <- ""
  tkconfigure(OpCB, values=txtF[Selecciona(s)])
}

DespliegaLicencia <- function() {
  DespliegaTxt("Licencia de uso de cliMTA", readLines("licencia.txt"))
}

# ================== MAIN PROGRAM =============================
# 
# directorio de trabajo:
# setwd("~/Progs/R")
# posición inicial:
pos <- c(0,0)
# tcl:> El toplevel corresponde a la raíz de ventanas methods::"."
tt <- tktoplevel()
tkwm.title(tt,PrgmTitle)
# En el siguiente frame meteremos todo:
# tcl:> ttk::frame .c -padding "3 3 12 12"
c <- tk2frame(tt, relief="sunken", borderwidth=10, padding=c(6,6,6,6))
ce <- tk2frame(c)
pos1 <- c(0,0)
#------------
imgIMTA <- tkimage.create("photo", file="logoImta.gif") # <-- Sólo acepta gifs
logoIMTA <- tk2label(ce, image=imgIMTA)
tkgrid(logoIMTA, row=pos1[1], column=pos1[2], sticky="w", padx=5, pady=5)
#------------
Titulo <- tklabel(ce, text=PrgmTitle,font=fontHeading1, foreground="blue")
pos1 <- right(pos1)
tkgrid(Titulo, row=pos1[1], column=pos1[2], sticky="nsew", padx=5, pady=5)
#------------
LicenciaBut <- tk2button(ce,text="Licencia de uso",command=DespliegaLicencia)
pos1 <- right(pos1)
tkgrid(LicenciaBut, row=pos1[1], column=pos1[2], sticky="e", padx=5, pady=5)
tkgrid(ce, row=pos[1], column=pos[2], columnspan=2)
#------------
div8 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div8, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
#--------------
pos <- down(pos)
Prsnt <- tk2label(c, text=paste(Presentacion,FuncsNam), font=fontHeading2, justify="center", foreground="#0a457d", background="gray")
tkgrid(Prsnt, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=1)
#------------
div1 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div1, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------
rb1 <- tk2radiobutton(c, text=cobertura["archivo","txt"], variable=tclvarsInfo[[1]], value=cobertura["archivo","itm"], command=function()tclvalue(tclvarsInfo[[2]])<-"")
rb2 <- tk2radiobutton(c, text=cobertura["directorio","txt"], variable=tclvarsInfo[[1]], value=cobertura["directorio","itm"], command=function()tclvalue(tclvarsInfo[[2]])<-"")
tkgrid(rb1, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
pos <- right(pos)
tkgrid(rb2, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div4 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div4, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------
infoFile <- tk2label(c, text="Archivo Seleccionado:",font=fontTextLabel)
pos <- right(pos)
# tcl:> grid .c.infoFile -column 1 -row 0 -sticky nsew -padx 5
tkgrid(infoFile, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
selFileBut <- tk2button(c,text="Seleccione Archivo/Directorio",command=LaunchFileSel)
pos <- down(pos)
tkgrid(selFileBut, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#-------------
FileNameEnt <- tk2entry(c, state="readonly", textvariable=tclvarsInfo[[2]])
pos <- right(pos)
tkgrid(FileNameEnt, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div5 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div5, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------
vartxt <- tk2label(c, text="Variable", justify="center", font=fontTextLabel)
tipotxt <- tk2label(c, text="Tipo", justify="center", font=fontTextLabel)
tkgrid(vartxt, row=pos[1], column=pos[2], sticky="es", padx=5, pady=5)
pos <- right(pos)
tkgrid(tipotxt, row=pos[1], column=pos[2], sticky="ws", padx=5, pady=5)
#------------
varsel <- tk2combobox(c, values=vars, state="readonly", textvariable=vsel)
tkbind(varsel, "<<ComboboxSelected>>", SeSeleccionoVar)
tipsel <- tk2combobox(c, values=tipoOp[[vars[indDefvar]]], state="readonly", textvariable=tclvarsInfo[[3]])
tkbind(tipsel, "<<ComboboxSelected>>", SeSeleccionoTip)
pos <- down(pos)
tkgrid(varsel, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
pos <- right(pos)
tkgrid(tipsel, row=pos[1], column=pos[2], sticky="w", padx=5, pady=5)
#------------
div6 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div6, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------
opExTxt <- tk2label(c, text="Operacion a ejecutar:", font=fontTextLabel)
pos <- down(pos)
tkgrid(opExTxt, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#-------------
OpCB <- tk2combobox(c, state="readonly", textvariable=op, width=29)
pos <- right(pos)
tkgrid(OpCB, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div7 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div7, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
pos <- down(pos)
#------------
ExlBut <- tk2button(c,text="Opcional: Excel de salida",command=LaunchFileSaveSel)
pos <- down(pos)
tkgrid(ExlBut, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#-------------
ExlEnt <- tk2entry(c, state="readonly", textvariable=tclvarsInfo[[4]])
pos <- right(pos)
tkgrid(ExlEnt, row=pos[1], column=pos[2], sticky="nsew", padx=5, pady=5)
#------------
div2 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div2, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
#--------------
pos <- down(pos)
bckgrnd <- tk2label(c, background="#0a457d")
tkgrid(bckgrnd, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=1)
ExBut <- tk2button(c, text="Ejecutar", command=ExOp)
tkgrid(ExBut, row=pos[1], column=pos[2], sticky="e", padx=5, pady=5)
#--------------
ExitBut <- tk2button(c, text="Salir", command=done)
pos <- right(pos)
tkgrid(ExitBut, row=pos[1], column=pos[2], sticky="w", padx=5, pady=5)
#------------
div3 <- tk2separator(c, orient="horizontal")
pos <- down(pos)
tkgrid(div3, row=pos[1], column=pos[2], columnspan=2, sticky="nsew", padx=5, pady=5)
#---------------
# tcl:> grid .c -column 0 -row 0 -sticky nsew
tkgrid(c, column=0, row=0, sticky="nsew")
tkfocus(tt)
