#install.packages("rvest")
#install.packages("dplyr")
library(stringr)
library(rvest)
library(dplyr)
#library(tidyverse)

link = "https://wfoplantlist.org/plant-list/taxon/wfo-9949999999-2021-12?page=1&hide_syns=true"
page = read_html(link)


names <- page %>% html_nodes(xpath="//*[@data-th='Name']/a") %>% html_text2()

#names = page %>% html_nodes(".name a") %>% html_text2()

#authors <- page %>% html_nodes(".wfo-list-authors") %>% html_text()


authors = page %>% html_nodes("tbody .wfo-list-authors a") %>% html_text2()

  Protologue = page %>% html_nodes(xpath="//*[@data-th='Protologue']") %>% html_text()
WFO_link = page %>% html_nodes(xpath="/*[@data-th='WFO link']") %>% html_text()

summary(names)
summary(authors)
summary(authors)
summary(Protologue)
summary(WFO_link)

plant_list <- data.frame(names,authors,Protologue,WFO_link)
plant_list

write.table(plant_list,"plant_list-df.csv", quote=FALSE,sep=",",col.names	="Family names", row.names = FALSE)
