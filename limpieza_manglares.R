#install.packages("janitor")
library(janitor)
library(dplyr)

#cargamos el df
data = read.csv2(file.choose())


#paso 1 familiarizarse con el data set
  #vistazo inicial
  str(data)
 
  
  #acortamos nuestro dataframe
  data_acortada <- subset(data, select = c("FECHA","ID_MUESTRA","ID_MUESTREO","ID_PARCELA","PARCELA",
                                           "ID_ARBOL","ESPECIE","ID_ESTADO_ARBOL","ESTADO_ARBOL","ALTURA","CAP","DAP"))
  
  data_acortada <- data_acortada[data_acortada$ID_PARCELA %in% c("45924","45926","45928","45932","45930","45934","45907","45910","45912","45922","47837","47839","45914","45916","45920","50089","50093","50091"),]
  
  #data_acortada$FECHA <- str_replace("",NA,data_acortada$FECHA)
  
  #remplazamos los valores blancos por NA
  data_acortada <- data_acortada %>% mutate_all(na_if,"") #20711
  
  #eliminamos valores no nulos
  nonulls <- data_acortada[complete.cases(data_acortada),] #7240
  
  #detectamos los valores duplicados
  duplicados <- nonulls %>% group_by(FECHA,ID_ARBOL,ID_PARCELA) %>% dplyr::summarise(Freq=n())
  
  #reemplazamos duplicados con la mediana
  noduplicados <- nonulls %>% group_by(ID_MUESTREO,ID_MUESTRA,FECHA,ID_ARBOL,ID_PARCELA,ESPECIE,ESTADO_ARBOL) %>% dplyr::summarise(ALTURA=median(ALTURA), CAP=median(CAP), DAP=median(DAP))
  
  #detectamos los valores duplicados nuevamente
  duplicados <- noduplicados %>% group_by(FECHA,ID_ARBOL,ID_PARCELA) %>% dplyr::summarise(Freq=n())
  duplicados[duplicados$Freq > 1,]
  
  [1] "ALTURA"  "--"      "numeric"
  [1] "CAP"     "--"      "numeric"
  [1] "DAP"     "--"      "numeric"
  
  duplicados[duplicados$Freq > 1,]
  
  noduplicados$FECHA <- as.Date(noduplicados$FECHA, format='%d/%m/%Y')
  noduplicados$FECHA <- format(noduplicados$FECHA, "%d-%m-%Y")
  
  unique(noduplicados$ESPECIE)
  
  unique(noduplicados$ESTADO_ARBOL)
  
  noduplicados[!noduplicados$ESTADO_ARBOL %in% c("Vivo"),]
  
  nonulls[nonulls$FECHA == "02/07/14" & nonulls$ID_ARBOL == 3 & nonulls$ID_PARCELA == 45916,]
  
  for ( columna in colnames(data_acortada) ){
    print(c(columna,"--",class(data_acortada[[columna]])))
  }
  
  str(data_acortada)
  
  #Verificamos la frecuencia de los valores en la columnas categoricas

    
    
    #Verificamos los registros de ID PARCELA
    ESP = data_acortada %>% tabyl(ID_PARCELA)%>% adorn_pct_formatting(digits = 4)
    ESP[order(-ESP$n),]#ordenamos por cantidad de registros
    
    #Verificamos los registros de PARCELA
    ESP = data_acortada %>% tabyl(PARCELA)%>% adorn_pct_formatting(digits = 4)
    ESP[order(ESP$PARCELA),]#ordenamos por cantidad de registros
    
    #Verificamos los registros de especies
    ESP = data_acortada %>% tabyl(ESPECIE)%>% adorn_pct_formatting(digits = 4)
    ESP[order(-ESP$n),]#ordenamos por cantidad de registros
    
    #Verificamos los registros del estado de los arboles
    EST = data_acortada %>% tabyl(ESTADO_ARBOL)%>% adorn_pct_formatting(digits = 4)
    EST[order(-EST$n),]#ordenamos por cantidad de registros

#Un vistazo inicial
str(df)