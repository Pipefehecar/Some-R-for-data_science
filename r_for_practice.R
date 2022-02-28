datos <- read.csv("estaciones.csv")
sum(datos$ID_ESTACION)
table(datos$PAIS_LOC)
table(datos$PROYECTO_LOC)
estructura_manglar<-read.csv("estructura-manglar.csv")
table(estructura_manglar$ESPECIE)
summary(estructura_manglar)
estructura_manglar$nueva <-estructura_manglar$ALTURA > 20
estructura_manglar$nueva
?summary
write.table(estructura_manglar,"estructura_nueva.csv", quote=FALSE,sep=",")
class(estructura_manglar$ALTURA)
sapply(estructura_manglar$ALTURA, class)
