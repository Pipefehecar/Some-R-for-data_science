ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]#esta en los paquetes instalados?
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# ISOLATION FOREST

ipak(c("solitude","rgl", "car"))

new_data <- subset(datos, select = c("ID_MUESTREO","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES","SUSTRATO_MS_DES","SUS_ESTACION_DES",
                                     "CODIGO_VARIABLE_EV_DES","CONCENTRACION_EV"))
new_data <- new_data[new_data$TEMPORADA_MS_DES == "Seca" & new_data$REGION_DES == "Caribe" & new_data$SUS_ESTACION_DES == "Agua Marina",]
new_data <- new_data[new_data$CODIGO_VARIABLE_EV_DES %in% c("Temperatura","Salinidad","pH"),]
new_data <- reshape(new_data, idvar = c("ID_MUESTREO","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES",
                                        "SUSTRATO_MS_DES","SUS_ESTACION_DES"), timevar = "CODIGO_VARIABLE_EV_DES", direction = "wide")
new_data <- new_data[complete.cases(new_data),]

detect_outliers_isoforest <- function(x){   #   DETECCION A TRAVES DEL ALGORITMO DE ISOLATION FOREST
  
  iso <<- isolationForest$new(sample_size = length(x), respect_unordered_factors = "ignore")
  iso$fit(x)
  x$predict <- iso$predict(x)
  x$etiqueta <- ifelse(x$predict$anomaly_score >= 0.60, "outlier", "normal")
  x <- subset(x, select = -c(predict))
  valids <- x[x$etiqueta == "normal",]
  outliers <- x[x$etiqueta == "outlier",]
  return(x)
}

ISO.result <- detect_outliers_isoforest(new_data)
ISO.result$etiqueta <- as.factor(ISO.result$etiqueta)

scatter3d(x = ISO.result$CONCENTRACION_EV.Temperatura,
          y = ISO.result$CONCENTRACION_EV.pH,
          z = ISO.result$CONCENTRACION_EV.Salinidad,
          groups = ISO.result$etiqueta)