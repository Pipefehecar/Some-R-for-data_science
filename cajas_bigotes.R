
#ipak(c("tidyverse"))

install.packages("tidyverse")
install.packages("ggplot2")

library(tidyverse)
library(ggplot2)
options(scipen = 999)

"Lectura del dataframe"
datos <- read.csv2("C:\\Users\\usrlabsis31\\Desktop\\just R\\Some R\\dataframes\\data.csv", stringsAsFactors = FALSE)
# View(datos)
datosf <- subset(datos, select = c("ID_MUESTRA","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES","SUSTRATO_MS_DES","SUS_ESTACION_DES",
                                   "CODIGO_VARIABLE_EV_DES","UNIDADES_EV_DES","CONCENTRACION_EV"))
datosf <- datosf[datosf$TEMPORADA_MS_DES == "Seca" & datosf$REGION_DES == "Caribe" & datosf$SUS_ESTACION_DES == "Agua Marina",]
datosf <- datosf[datosf$CODIGO_VARIABLE_EV_DES %in% c("Temperatura","Salinidad"),]

datosf <- datosf[datosf$COD_ESTACION %in% c("C47003006","C47003015","C47003016","C47003022","C47003020","C47003008"),]




"DIBUJAMOS EL DIAGRAMA DE CAJAS"

ggplot(datosf, aes(x=0, y=CONCENTRACION_EV, fill = COD_ESTACION)) + 
  geom_boxplot() + facet_wrap(~CODIGO_VARIABLE_EV_DES) + theme_dark()




"METODO IQR"

iqrMethod <- function(x){
  
  estaciones <<- unique(x$COD_ESTACION) #extraemos las estaciones
  
  iqrDf <<- data.frame(matrix(NA, nrow = 6, ncol = 6))  
  colnames(iqrDf) <<- c("CODEST", "Q1", "Q3", "IQR", "limitInf", "limitSup")
  iqrDf$CODEST <<- estaciones
  
  dfValids <- x[0,]
  dfOutliers <- x[0,]
  
  for(codEst in estaciones){
    
    iqrDatos <- x[x$COD_ESTACION == codEst,] #seleccionamos las fila correspondiente a la estacion iterada
    Q <<- quantile(iqrDatos$CONCENTRACION_EV, probs=c(.25, .75), na.rm = FALSE) #matriz con los cuartiles
    iqrV <<- IQR(iqrDatos$CONCENTRACION_EV)
    
    iqrDf$Q1[iqrDf$CODEST == codEst] <<- Q[1]
    iqrDf$Q3[iqrDf$CODEST == codEst] <<- Q[2]
    iqrDf$IQR[iqrDf$CODEST == codEst] <<- iqrV
    iqrDf$limitInf[iqrDf$CODEST == codEst] <<- Q[2] - 1.5*iqrV
    iqrDf$limitSup[iqrDf$CODEST == codEst] <<- Q[1] + 1.5*iqrV
    
    limitInf <- iqrDf$limitInf[iqrDf$CODEST == codEst]
    limitSup <- iqrDf$limitSup[iqrDf$CODEST == codEst]
    
    searchValids <- iqrDatos[iqrDatos$CONCENTRACION_EV >= limitInf & iqrDatos$CONCENTRACION_EV <= limitSup,]
    searchOutliers <- iqrDatos[iqrDatos$CONCENTRACION_EV < limitInf | iqrDatos$CONCENTRACION_EV > limitSup,]
    
    dfValids <- rbind(dfValids, searchValids)
    dfOutliers <- rbind(dfOutliers, searchOutliers)
  }
  
  return(list(Valids = dfValids, Outliers = dfOutliers))
}


filas_salinidad = datosf$CODIGO_VARIABLE_EV_DES == "Salinidad" #devuelve verdadero o falso para cada posicion
parametro_salinidad = datosf[filas_salinidad,] #filas seleccionasdas, todas las columnas


IQR.salinidad <- iqrMethod(parametro_salinidad)
IQR.temperatura <- iqrMethod(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",])


View(IQR.salinidad$Valids)
View(IQR.salinidad$Outliers)
