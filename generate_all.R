# Libraries
renv::activate()
renv::restore()
library(rmarkdown)
library(officedown)
library(knitr)

# Source the R codes
source("initialisation/00_CORE.R")

# DOCX
render("rmd/00_DOCX.Rmd", output_format = rdocx_document(reference_docx = paste0(here(),"templates/doc_template2.docx")), output_file = paste0(here(),"/outputs/GTA_data_checks.docx"))
