
ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

options(scipen = 999)

# ALGORITMOS DE DETECCION DE DATOS ATIPICOS

ipak(c("tidyverse"))

datos <- read.csv2("C:\\Users\\usrlabsis28\\Downloads\\data.csv", stringsAsFactors = FALSE)
datosf <- subset(datos, select = c("ID_MUESTRA","COD_ESTACION","REGION_DES","TEMPORADA_MS_DES","SUSTRATO_MS_DES","SUS_ESTACION_DES",
                                   "CODIGO_VARIABLE_EV_DES","UNIDADES_EV_DES","CONCENTRACION_EV"))
datosf <- datosf[datosf$TEMPORADA_MS_DES == "Seca" & datosf$REGION_DES == "Caribe" & datosf$SUS_ESTACION_DES == "Agua Marina",]
datosf <- datosf[datosf$CODIGO_VARIABLE_EV_DES %in% c("Temperatura","Salinidad"),]

datosf <- datosf[datosf$COD_ESTACION %in% c("C47003006","C47003015","C47003016","C47003022","C47003020","C47003008"),]

ggplot(datosf, aes(x=0, y=CONCENTRACION_EV, fill = COD_ESTACION)) + 
  geom_boxplot() + facet_wrap(~CODIGO_VARIABLE_EV_DES) + theme_bw()

"METODO IQR"

iqrMethod <- function(x){
  
  estaciones <<- unique(x$COD_ESTACION)
  iqrDf <<- data.frame(matrix(NA, nrow = 6, ncol = 6))  
  colnames(iqrDf) <<- c("CODEST", "Q1", "Q3", "IQR", "limitInf", "limitSup")
  iqrDf$CODEST <<- estaciones
  
  dfValids <- x[0,]
  dfOutliers <- x[0,]
  
  for(codEst in estaciones){
    
    iqrDatos <- x[x$COD_ESTACION == codEst,]
    Q <<- quantile(iqrDatos$CONCENTRACION_EV, probs=c(.25, .75), na.rm = FALSE)
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

IQR.salinidad <- iqrMethod(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Salinidad",])
IQR.temperatura <- iqrMethod(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",])

#  DETECCION POR LA MEDIA

detect_outliers_mean <- function(x){          
  
  processed_data <<- x %>% group_by(COD_ESTACION) %>% 
    summarise(FREQ = n(), STD = sd(CONCENTRACION_EV), MEAN = mean(CONCENTRACION_EV), .groups = 'drop')
  
  processed_data <<- mutate(processed_data, LIM_INF = MEAN - 3*STD, LIM_SUP = MEAN + 3*STD) 
  processed_data$LIM_INF[processed_data$LIM_INF < 0] <- 0 
  
  datos_redcam <<- merge(x, y = subset(processed_data, select = -c(FREQ,MEAN,STD)), 
                         by = "COD_ESTACION", all.datos_redcam = TRUE)
  
  valids <<- datos_redcam[(datos_redcam$LIM_INF <= datos_redcam$CONCENTRACION_EV & datos_redcam$CONCENTRACION_EV <= datos_redcam$LIM_SUP),]
  outliers <<- datos_redcam[(datos_redcam$CONCENTRACION_EV < datos_redcam$LIM_INF | datos_redcam$CONCENTRACION_EV > datos_redcam$LIM_SUP),]
  return(list(Valids = valids, Outliers = outliers))
}

MEAN.salinidad <- detect_outliers_mean(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Salinidad",])
MEAN.temperatura <- detect_outliers_mean(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",])

# DETECCION POR LA MEDIANA

detect_outliers_median <- function(x){          
  
  processed_data <<- x %>% group_by(COD_ESTACION) %>% 
    summarise(FREQ = n(), MAD = mad(CONCENTRACION_EV), MEDIAN = median(CONCENTRACION_EV), .groups = 'drop')
  
  processed_data <<- mutate(processed_data, LIM_INF = MEDIAN - 3*MAD, LIM_SUP = MEDIAN + 3*MAD) 
  processed_data$LIM_INF[processed_data$LIM_INF < 0] <- 0 
  
  datos_redcam <<- merge(x, y = subset(processed_data, select = -c(FREQ,MEDIAN,MAD)), 
                         by = "COD_ESTACION", all.datos_redcam = TRUE)
  
  valids <<- datos_redcam[(datos_redcam$LIM_INF <= datos_redcam$CONCENTRACION_EV & datos_redcam$CONCENTRACION_EV <= datos_redcam$LIM_SUP),]
  outliers <<- datos_redcam[(datos_redcam$CONCENTRACION_EV < datos_redcam$LIM_INF | datos_redcam$CONCENTRACION_EV > datos_redcam$LIM_SUP),]
  return(list(Valids = valids, Outliers = outliers))
}

MEDIAN.salinidad <- detect_outliers_median(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Salinidad",])
MEDIAN.temperatura <- detect_outliers_median(datosf[datosf$CODIGO_VARIABLE_EV_DES == "Temperatura",])

# METODOS NO PARAMETRICOS

# NO SUPERVISADOS

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

# DBSCAN

ipak(c("DBSCAN"))

dbScanData <- new_data
pivotData <- as.matrix(dbScanData[7:9])
kNNdistplot(pivotData, k = 10)  
dbscan.result <- dbscan(pivotData, eps= 2.5, minPts=10)
hullplot(pivotData, dbscan.result$cluster)
dbScanData$cluster <- dbscan.result$cluster 
dbScanData[dbScanData$cluster == 0,]

"----------------------------------------------------------------------------------------------------------------------"

# VALIDACION DE VARIABLES CATEGORICAS
planilla_data <- read.csv("C:\\Users\\usrlabsis28\\Documents\\OTGA_CienciaDeDatos\\Datos_OTGA\\planilla_especie.csv", 
                          stringsAsFactors = FALSE)

str(planilla_data$REINO)
reinos <- c("ANIMALIA","PLANTAE")
str(planilla_data$REINO[planilla_data$REINO %in% reinos])
str(planilla_data$REINO[!planilla_data$REINO %in% reinos])

"WoRMS"
ipak(c("httr","jsonlite","tidyverse"))

especiesData <- read.delim("C:\\Users\\usrlabsis28\\Documents\\OTGA_CienciaDeDatos\\Datos_OTGA\\occurrence.txt", 
                           header = TRUE, stringsAsFactors = FALSE, encoding = "UTF-8")

especiesData <- subset(especiesData, select = c("id","scientificName","kingdom","phylum","class","order","family",
                                                "genus","specificEpithet","taxonRank","scientificNameAuthorship"))

#especiesData <- especiesData[especiesData$scientificName != "Dictyota guineënsis",]
especies <- unique(especiesData$scientificName)

st_query <- "https://www.marinespecies.org/rest/AphiaRecordsByNames?scientificnames[]="
end_query <- "&like=false&marine_only=true"

for(i in 1:length(especies)){
  
  if(grepl("^\\S+\\s+", especies[i])){
    name <- gsub("[[:space:]]", "%20", especies[i])
  }else{name <- especies[i]}
  
  if ((i == 1)){
    url <- paste(st_query, name, "&scientificnames[]=", sep = "")
  }else if (i < length(especies)){
    url <- paste(url, name, "&scientificnames[]=", sep = "")
  }else{
    url <- paste(url, name, end_query, sep = "")
  }
}

conx <- httr::GET(url)
url_cont <- content(conx, "text")
data_json <- fromJSON(url_cont)
worms_data <- do.call("rbind", lapply(data_json, as.data.frame))
worms_data <- subset(worms_data, select = c("scientificname","kingdom","phylum","class","order","family","genus","rank","authority"))

data_distinct <- especiesData %>% dplyr::select(scientificName,kingdom,phylum,class,order,family,genus,taxonRank,scientificNameAuthorship) %>% distinct

data_distinct$taxonRank <- str_replace_all(data_distinct$taxonRank, c("Reino"="Kingdom","Filo"="Phylum","Clase"="Class",
                                                                      "Orden"="Order","Familia"="Family","Género"="Genus",
                                                                      "Especie"="Species"))

colnames(worms_data)[8:9] <- c("taxonRank","scientificNameAuthorship")
data_distinct[data_distinct == "" | is.na(data_distinct)] <- "--"
worms_data[worms_data == "" | is.na(worms_data)] <- "--"
data_distinct[,c("status","suggest")] <- NA

evaluateWorms <- function(data){
  
  for(i in 1:nrow(data)){
    
    item <- data[i,-c(10,11)]
    compareItem <- worms_data[worms_data$scientificname == item$scientificName,]
    numCompareItems <- nrow(compareItem)
    
    if(numCompareItems == 0){
      
      data$status[i] <- "Nombre científico no encontrado en Worms"
      
    }else{
      
      if(numCompareItems == 1){
        comparer <- item == compareItem
      }else{
        compareItem$occurrences <- NA
        for(elem in 1:numCompareItems){
          compareItem$occurrences[elem] <- sum(item == compareItem[elem,-10])
        }
        compareItem <- compareItem[compareItem$occurrences == max(compareItem$occurrences),-10]
        comparer <- item == compareItem
      }
      
      if(length(comparer[comparer == F]) > 0){
        data$status[i] <- "Registro invalido"
        no_match_cols <- colnames(comparer)[which(comparer == FALSE)]
        no_match_values <- compareItem[colnames(compareItem) %in% no_match_cols]
        data$suggest[i] <- str_glue("Sugerencias|",paste(colnames(no_match_values), no_match_values, sep = ": ", collapse = ", "))  
        
      }else{
        data$status[i] <- "Registro valido"
      }
    }
  }
  
  return(data)
}

registros <- evaluateWorms(data_distinct)

"REGEX"

regex_data <- data.frame(NUMBERS = c("10117856493234","8547936f90685091","89051","544509460990115f"),
                         VIEWS = c("VM_AGM_0000_000","VM_AGM_3410_830","VM_AGM_55555_000","VM_AGM_9999_9999"),
                         OTHERS = c("psj@invemar.com","psj@invemar.gov","Psj15*","psj15*"))

str_detect(regex_data$OTHERS, "^(?=.*[a-z])(?=.*[A-Z])(?=.*[0-9])(?=.*[#$%&*-]).*$")





