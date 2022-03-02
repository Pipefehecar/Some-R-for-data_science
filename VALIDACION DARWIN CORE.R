
library(httr)
library(jsonlite)
library(readxl)
library(dplyr)

import_options <- as.vector(c(".xlsx",".csx - sep(,)",".txt"))
select_option <- select.list(import_options, multiple = FALSE, graphics = TRUE, title = "SL. Extensión")

if(select_option == ".xlsx"){data <- read_excel(file.choose(), sheet = 1, col_names = TRUE, col_types = "text", na = "")
}else if (select_option == ".csv"){data <- read.csv(file.choose())
}else{data <- read.delim(file.choose())}

data <- subset(data, select = c("id","scientificName","kingdom","phylum","class","order","family",
                                "genus","taxonRank","scientificNameAuthorship"))

unq_names <- data %>% select(scientificName) %>% distinct
unq_names <- as.vector(t(unq_names))

st_query <- "https://www.marinespecies.org/rest/AphiaRecordsByNames?scientificnames[]="
end_query <- "&like=false&marine_only=true"

for(i in 1:length(unq_names)){
  
  if(grepl("^\\S+\\s+", unq_names[i])){
    
    name <- gsub("[[:space:]]", "%20", unq_names[i])
    
  }else{name <- unq_names[i]}
 
  if ((i == 1)){
    url <- paste(st_query, name, "&scientificnames[]=", sep = "")
  }
  
  else if (i < length(unq_names)){
    url <- paste(url, name, "&scientificnames[]=", sep = "")
  }
  
  else{
    url <- paste(url, name, end_query, sep = "")
  }
}

url <- "https://www.marinespecies.org/rest/AphiaRecordsByNames?scientificnames[]=Lithophyllum&scientificnames[]=Porolithon&like=false&marine_only=true"
conx <- httr::GET(url)
url_cont <- content(conx, "text")
data_json <- fromJSON(url_cont)
query_data <- do.call("rbind", lapply(data_json, as.data.frame))
query_data <- subset(query_data, select = c("AphiaID","scientificname","kingdom","phylum","class","order","family","genus","rank","authority"))

data_distinct <- data %>% select(scientificName,kingdom,phylum,class,order,family,genus,taxonRank,scientificNameAuthorship) %>% distinct

for( col in colnames(data_distinct ) ){
  data_distinct[[col]]  <- as.character(data_distinct[[col]])
}

data_distinct$taxonRank[data_distinct$taxonRank == "Reino"] <- "kingdom"
data_distinct$taxonRank[data_distinct$taxonRank == "Filo"] <- "phylum"
data_distinct$taxonRank[data_distinct$taxonRank == "Clase"] <- "class"
data_distinct$taxonRank[data_distinct$taxonRank == "Orden"] <- "order"
data_distinct$taxonRank[data_distinct$taxonRank == "Familia"] <- "family"
data_distinct$taxonRank[data_distinct$taxonRank == "Género"] <- "genus"

colnames(query_data)[9] <- "taxonRank"
colnames(query_data)[10] <- "scientificNameAuthorship"

data_distinct["status"] <- NA
data_distinct["suggest"] <- NA

for(i in 1:nrow(data_distinct)){
  
  data_compare <- subset(data_distinct[i,], select = -c(status,suggest))
  name <- data_compare$scientificName
  author <- data_compare$scientificNameAuthorship
  query_compare <- subset(query_data, query_data$scientificname == name, select = -c(AphiaID))
  
  if(nrow(query_compare) > 0){
    
    if(nrow(query_compare) > 1){
      
      if(is.na(data_compare$scientificNameAuthorship)){
        
        query_compare <- subset(query_compare, is.na(query_compare$scientificNameAuthorship))
        
        if(nrow(query_compare) > 1){
          
          query_compare$na_count <- apply(query_compare, 1, function(x) sum(is.na(x)))
          query_compare <- query_compare[query_compare$na_count ==  sum(is.na(data_compare)),]
          
        }
        
      }else{
        query_compare <- subset(query_compare, query_compare$scientificNameAuthorship == author)
      }
    }
    
    if(data_compare$taxonRank == "kingdom"){ logic_state <<- data_compare[1:5-8] == query_compare[1:5-8]
    }else if(data_compare$taxonRank == "phylum"){ logic_state <<- data_compare[1:4-8] == query_compare[1:4-8]
    }else if(data_compare$taxonRank == "class"){ logic_state <<- data_compare[1:3-8] == query_compare[1:3-8]
    }else if(data_compare$taxonRank == "order"){ logic_state <<- data_compare[1:2-8] == query_compare[1:2-8]
    }else if(data_compare$taxonRank == "family"){ logic_state <<- data_compare[1:1-8] == query_compare[1:1-8]
    }else if(data_compare$taxonRank == "genus" | data_compare$taxonRank == "Especie"){ logic_state <<- data_compare[1:7] == query_compare[1:7]}
    
    logic_state[is.na(logic_state)] <- FALSE
    
    if(all(logic_state)){
      
      data_distinct$status[i] <- "VALID"
      data_distinct$suggest[i] <- ""
      
    }else{
        no_match_cols <- colnames(logic_state)[which(logic_state == FALSE)]
        no_match_cols <- paste(no_match_cols, collapse = ", ")
        
        colnames(data_compare)[which(is.na(data_compare) && is.na(query_compare))]
        
        if(is.na(data_compare$scientificNameAuthorship) && is.na(query_compare$scientificNameAuthorship)){
          
          if(length(no_match_cols) > 1){
            
            no_match_cols <- no_match_cols[no_match_cols!="scientificNameAuthorship"]
            data_distinct$status[i] <- paste("No match in: ", no_match_cols, collapse = "")
            name <- data_distinct$scientificName[i]
            no_match_values <- sub('.*No match in:\\s*', '', data_distinct$status[i])
            nmv_vector <- unlist(strsplit(no_match_values, ", "))
            
            for(j in 1:length(nmv_vector)){
              
              if(j == 1){
                
                suggested <- paste0(nmv_vector[j], ": ", query_compare[,nmv_vector[j]])
                
              }else{suggested <- paste0(suggested, ", ", nmv_vector[j], ": ", query_compare[,nmv_vector[j]])}
            }
            
            data_distinct$suggest[i] <- suggested
          
          }else{data_distinct$status[i] <- "scientificNameAuthorship not founded";data_distinct$suggest[i] <- ""}
          
        }else{
          
          data_distinct$status[i] <- paste("No match in: ", no_match_cols, collapse = "")
          name <- data_distinct$scientificName[i]
          no_match_values <- sub('.*No match in:\\s*', '', data_distinct$status[i])
          nmv_vector <- unlist(strsplit(no_match_values, ", "))
          
          for(j in 1:length(nmv_vector)){
            
            if(j == 1){
              
              suggested <- paste0(nmv_vector[j], ": ", query_compare[,nmv_vector[j]])
              
            }else{suggested <- paste0(suggested, ", ", nmv_vector[j], ": ", query_compare[,nmv_vector[j]])}
          }
          
          data_distinct$suggest[i] <- suggested
          
         }
     }
    
  }else{data_distinct$status[i] <- "scientificName not found"; data_distinct$suggest[i] <- ""}
}
