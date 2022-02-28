#install.packages("rvest")
#install.packages("dplyr")

library(rvest)
library(dplyr)

link_home= "http://www.theplantlist.org/1.1/browse/"
main_page =  read_html(link_home)

major_groups_scientific = html_nodes(main_page,"#nametree .majorgroup") %>% html_text()
major_groups_clasification = html_nodes(main_page,"#nametree a") %>% html_text()
major_groups_links = html_nodes(main_page,"#nametree a") %>% html_attr("href")

num_major_groups <- length(major_groups_scientific)
for ( group in major_groups_clasification) {
  ?paste()
}



link = "http://www.theplantlist.org/1.1/browse/A/#B"
page = read_html(link)






name = page %>% html_nodes(".family") %>% html_text()
name
write.table(name,"Angiosperms-families.csv", quote=FALSE,sep=",",col.names	="Family names", row.names = FALSE)
