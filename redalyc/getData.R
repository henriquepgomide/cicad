# Get data from Redalyc
# author: Henrique Gomide
# license: GNU V3

library("rvest")

########################
## Fuente Academica ----
########################

url <- "fuenteAcademica/busqueda_fuenteacademica-p3.html"

fuenteAcademica_index <- url %>%
  read_html() %>%
  html_nodes(".record-index") %>%
  html_text()

fuenteAcademica_title <- url %>%
  read_html() %>%
  html_nodes(".title-link-wrapper") %>%
  html_text()

fuenteAcademica_abstract <- url %>%
  read_html() %>%
  html_nodes(".abstract") %>%
  html_text() 

fuenteAcademica_authors <- url %>%
  read_html() %>%
  html_nodes(".standard-view-style") %>%
  html_text()
authors <- fuenteAcademica_authors[grepl("By:", fuenteAcademica_authors)]
ind <- c(16)
val <- c(authors, rep("Unknow",length(ind)))
id  <- c(seq_along(authors), ind+0.5 )
authors <- val[order(id)]

fuenteAcademica_3 <- data.frame(db = "fuente academica", index = fuenteAcademica_index, title = fuenteAcademica_title, authors = authors, abstract = fuenteAcademica_abstract)
fuenteAcademica_1$abstract <- NA

fuenteAcademica <- rbind(fuenteAcademica_1, fuenteAcademica_2, fuenteAcademica_3)
write.csv(fuenteAcademica, "fuenteAcademica.csv", row.names = FALSE)


########################
## medicLatina      ----
########################

url <- "medicLatina/busqueda_medicLatina-p6.html"
medicLatina_index <- url %>%
  read_html() %>%
  html_nodes(".record-index") %>%
  html_text()

medicLatina_title <- url %>%
  read_html() %>%
  html_nodes(".title-link-wrapper") %>%
  html_text()

# medicLatina_abstract <- url %>%
#   read_html() %>%
#   html_nodes(".abstract") %>%
#   html_text() 

medicLatina_6 <- data.frame(db = "medicLatina", index = medicLatina_index, title = medicLatina_title, authors = NA, abstract = NA)

medicLatina <- rbind(medicLatina_1, medicLatina_2, medicLatina_3, medicLatina_4, medicLatina_5, medicLatina_6)
write.csv(medicLatina, "medicLatina.csv", row.names = FALSE)
