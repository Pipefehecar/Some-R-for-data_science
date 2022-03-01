
# FUNDAMENTOS DE PROGRAMACION EN R - FUNCIONES IMPORTANTES DEL RBASE

x <- c("oxigeno", "nitrogeno", "hidrogeno")

# NUMERO DE LETRAS EN UNA PALABRA

nchar("oxigeno")

# NUMERO DE LETRAS DE CADA ELEMENTO EN EL VECTOR
nchar(x)

# ABREVIAR 
abbreviate(x)
abbreviate(x, 3)
df <- data.frame(`Hidrocarburos Totales` = as.numeric(), `Oxigeno Disuelto` = as.numeric())
abbreviate(colnames(df), 6)
colnames(df) <- abbreviate(colnames(df), 6)

# REPLICAR ELEMENTOS DEL VECTOR. PARAMETROS rep(vector, numero de veces que se quiere replicar)

rep(x, 3)

# REEMPLAZAR POR POSICION
x
replace(x, 1, "amonio")

# REEMPLAZAR INDICANDO LA PALABRA
replace(x, which(x == "nitrogeno"), "amonio")
replace(x, which(x == "amonio"), "oxigeno")

# REEMPLAZA TODAS LAS LETRAS O por U EN EL VECTOR, EVALUA ELEMENTO POR ELEMENTO
gsub("o", "u", x)

# REEMPLAZA SOLO LA PRIMERA O DE CADA ELEMENTO por UNA U, EVALUA ELEMENTO POR ELEMENTO
sub("o", "u", x)

# CONCATENA VECTORES ELEMENTO POR ELEMENTO UTILIZANDO UN ESPACIO EN BLANCO COMO SEPARADOR 

y <- c("Carbono")
paste(x,y, sep = "-")

# CONCATENA VECTORES ELEMENTO POR ELEMENTO SIN UTILIZAR SEPARADOR
paste0(x,y)

# ACCEDER A UN ELEMENTO ESPECIFICO DEL RESULTADO 
paste(x, "y", y)[2]


# MANIPULACION DE ESTRUCTURAS DE DATOS

x <- c("a", "b", "c", "d")

# AÑADIR UN ELEMENTO A UN VECTOR EN LA ULTIMA POSICION

x <- append(x, "a")

# IDENTIFICA LOS ELEMENTOS DUPLICADOS. DEVUELVE UN VECTOR DE OPERADORES LOGICOS
duplicated(x)

# DEVUELVE LA POSICION DE LOS ELEMENTOS DUPLICADOS
which(duplicated(x))

# DEVUELVE ESPECIFICAMENTE LOS ELEMENTOS DUPLICADOS
x[which(duplicated(x))]

d1 <- data.frame(num1 = )
seq(1,10, 0.1)

# CARGAR DATOS DESDE UN ARCHIVO CSV

file.choose()
datos_practica <- read.csv("C:\\Users\\camil\\datos_practica.csv")

# CREA UN SUBCONJUNTO A PARTIR DE UN SET DE DATOS Y LAS COLUMNAS SELECCIONADAS
datos_practica <- subset(datos_practica, select = c("DEPTO","CODEST","NOMEST","ICAMPFF"))
datos_practica <- subset(datos_practica, select = -c(X))
datos_practica <- datos_practica[,-c(1,2)]

# CREA UNA NUEVA COLUMNA EN EL DATAFRAME A PARTIR DE UNA SECUENCIA
datos_practica$id <- seq(1, nrow(datos_practica))

# CREA UNA NUEVA COLUMNA EN EL DATAFRAME QUE UTILIZANDO SOLO LOS PRIMEROS 4 CARACTERES DE OTRA COLUMNA
datos_practica$ID.COD <- substr(datos_practica$CODEST, start = 1, stop = 4)

# SEGMENTA UN CONJUNTO DE DATOS A PARTIR DE UNA VARIABLE 
datos_segmentados <- split(datos_practica, datos_practica$DEPTO)

# CREA EL VECTOR 
meses <- c("febrero","marzo","enero","abril","mayo","junio")

# CREA UNA COLUMNA MES ASIGNANDO VALORES ALEATORIOS QUE PERTENEZCAN AL VECTOR ANTERIOR
datos_practica$MES <- sample(meses, size = nrow(datos_practica), replace = TRUE)
datos_practica <- datos_practica[with(datos_practica, order(MES)),]

# CONVIERTE LA COLUMNA EN UN FACTOR
datos_practica$MES <- factor(datos_practica$MES, levels = c("enero","febrero","marzo","abril","mayo","junio"))

# VISUALIZA LOS NIVELES DEL FACTOR
levels(datos_practica$MES)

# ORDENA LOS ELEMENTOS DEL FACTOR
datos_practica$MES <- ordered(datos_practica$MES, c("enero","febrero","marzo","abril","mayo","junio"))
datos_practica$MES <- factor(datos_practica$MES, levels = c("enero","febrero","marzo","abril","mayo","junio"))

# ORDENA LOS ELEMENTOS DEL DATAFRAME A PARTIR DE LAS COLUMNAS SELECCIONADAS
datos_practica <- datos_practica[with(datos_practica, order(DEPTO,CODEST,MES)),]

# APPLY, SAPPLY, TAPPLY, LAPPLY

# MULTIPLICA LOS VALORES DE DOS COLUMNAS FILA POR FILA. APPLY EJECUTA UNA FUNCION/OPERACION TENIENDO COMO CRITERIO EL SEGUNDO PARAMETRO (MARGIN). SI ES 1 OPERA POR FILAS,
# SI ES 2 OPERA POR COLUMNAS
datos_practica$concatenar <- apply(datos_practica, 1, function(x) paste0(as.character(x[c("DEPTO")]),"-",as.character(x[c("CODEST")])))

# LAPPLY Y SAPPLY EJECUTAN FUNCIONES SOBRE VECTORES. LAPPLY DEVUELVE EL RESULTADO EN UNA LISTA, SAPPLY EN UN VECTOR
lapply(datos_practica$concatenar, function(x) gsub("-", "*", x))
sapply(datos_practica$concatenar, function(x) gsub("-", "*", x))

# APLICA FUNCIONES COMO MEAN,MEDIA,MAX,MIN A PARTIR DE UN CRITERIO DE AGRUPACION, EN ESTE CASO EL ID
tapply(datos_practica$ICAMPFF, datos_practica$DEPTO, mean)

# MANIPULACION DE ARCHIVOS

# CREA UN DIRECTORIO
dir.create("C:\\Users\\camil\\Documents\\OTGA\\ejercicio1")

# LISTA LOS ELEMENTOS EL DIRECTORIO
dir("C:\\Users\\camil\\Documents\\OTGA")
list.files("C:\\Users\\camil\\Documents\\OTGA")

# CREA UN ARCHIVO EN LA DIRECCION C:\\Users\\camil\\Documents\\, DE NOMBRE practica CON EXTENSION .txt
file.create("C:\\Users\\camil\\Documents\\OTGA\\practica.txt")

# VERIFICA QUE EL ARCHIVO EXISTE
file.exists("C:\\Users\\camil\\Documents\\OTGA\\practica.txt")

# ELIMINA EL ARCHIVO
file.remove("C:\\Users\\camil\\Documents\\OTGA\\practica.txt")

# DEVUELVE EL DIRECTORIO DEL ARCHIVO SELECCIONADO
file.choose()

# DEVUELVE EL DIRECTORIO DE TRABAJO
getwd()

# DEFINE UN DIRECTORIO DE TRABAJO
setwd("C:\\Users\\camil")
