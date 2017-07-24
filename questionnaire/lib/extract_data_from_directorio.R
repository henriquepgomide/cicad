# Extract data from directorio file
library(docxtractr)
doc <- read_docx(system.file("", package="docxtractr"))
docx_tbl_count(doc)
docx_describe_tbls(doc)

df <- docx_extract_tbl(doc, 1)
