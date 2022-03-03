#install.packages("rvest")
#install.packages("dplyr")
pwd_get
library(rvest)
library(dplyr)
library(stringr)

link_home = "http://www.theplantlist.org"
main_page =  read_html(link_home)

#major_groups_scientific_name = html_nodes(main_page,"#nametree .majorgroup") %>% html_text()
major_groups_scientific_name = html_nodes(main_page,xpath="//*[@id='nametree']/li/a/i") %>% html_text()
major_groups_common_name = html_nodes(main_page,xpath="//*[@id='nametree']/li/a/text()") %>% html_text()
major_groups_links = html_nodes(main_page,"#nametree a") %>% html_attr("href")


num_major_groups <- length(major_groups_scientific_name)


plants <- data.frame()


for(i in 1:num_major_groups){
  # print(paste(link_home,major_groups_links[i]))
  url = paste(link_home,major_groups_links[i]) 
  page = read_html(str_replace(url," ",""))
  names = html_nodes(page,".family") %>% html_text()
  length(names)
  for (name in names) {
    new_row = c(name,major_groups_common_name[i],major_groups_scientific_name[i])
    plants <- rbind(plants,new_row)
  }

}
names(plants)<-c("NAME","GROUP_COMMON_NAME","GROUP_SCIENTIFIC_NAME")
plants #imprimimos el dataset
View(plants)
write.table(plants,"plant_list-families.csv", quote=FALSE,sep=";", row.names = FALSE)
