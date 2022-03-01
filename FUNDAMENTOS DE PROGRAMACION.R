
# FUNDAMENTOS DE PROGRAMACION EN R - FUNCIONES IMPORTANTES DEL RBase

# IMPRIMIR UN MENSAJE EN LA CONSOLA
print("Hola Mundo")

# ASIGNACION
x <- 1
x = 2
x <<- 3

# ESTRUCTURAS DE DATOS

# Vector
x <- c("Salinidad", "Temperatura", "Conductividad")

# Matriz
xmatrix <- matrix(1:9, nrow = 3, ncol = 3)
vector1 <- c("Oxigeno", "Hidrogeno")
vector2 <- c("Nitrogeno", "Carbono")
charmatrix <- rbind(vector1,vector2)
class(charmatrix)
vector1 <- matrix(1:3, nrow = 3, ncol = 1)
vector2 <- matrix(4:6, nrow = 3, ncol = 1)
charmatrix <- cbind(vector1,vector2)

# Lista

lista <- list(obj1 = "", obj2 = "", obj3 = "")
lista <- list(obj1 = as.character(), obj2 = as.numeric())
lista[[1]] <- "Temperatura";lista[[2]] <- 23.4

# Dataframes

df <- data.frame(Numeros = c(1:3), Elementos = c("Oxigeno", "Carbono", "Hidrogeno"), Booleanos = c(1,0,1))
df$Booleanos
df[["Booleanos"]]

# OPERADORES

"Extraer Informacion - Vectores"
x[1]

"Extraer Informacion - Matrices"
xmatrix[1,2]

"Extraer Informacion - Listas"
lista$obj1[1]
lista[[2]]

"Extraer Informacion - Dataframes"
df[1,2]
df[c(FALSE,TRUE,FALSE),]
df[,3]
df$Booleanos

"Diferencia"
x[1]!=x[2]

"Igualdad"
x[1]==x[2]

"Coincidencia"
"Salinidad" %in% x
"Salinidad" %in% xmatrix[1]
"Carbono" %in% df$Elementos

# Condicionales

a = 1
b = 2

ifelse(a==b, "Es correcto", "Es incorrecto")

if(a == b){
  
  print("Son iguales")
  
}else if(a < b){
  
  print("a es menor que b")
  
}else{print("a es mayor que b")}


# CICLO WHILE

i <- 0

while(i < 10){
  
  if(i != 6 ){
    
    print(i)
    i <- i +1
    
  }else{
    i <- i +1
    next}
}

# CICLO FOR

for(i in 1:nrow(df)){print(i)}

# FUNCIONES


function.addition <- function(x,y){
  
  a <- 1
  a <<- "a"
  return(x+y)
  
}

function.addition(3,2)

