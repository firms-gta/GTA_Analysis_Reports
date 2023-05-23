# Libraries
library(rmarkdown)
library(officedown)
library(knitr)

# Source the R codes
source("initialisation/00_CORE.R")

# DOCX
render(here("rmd/00_DOCX.Rmd"), output_format = rdocx_document(reference_docx = paste0(here("templates/doc_template2.docx"))), output_file = paste0(here("outputs/GTA_data_checks.docx")))


render(here("rmd/NOT_CONFORM_CONVERSION_FACTORS.Rmd"), output_format = "")

render(here("rmd/OVERLAPPING_GENERAL.Rmd"), output_format = rdocx_document(reference_docx = paste0(here("templates/doc_template2.docx"))), output_file = paste0(here("outputs/GTA_overlaps.docx")))



render(here("rmd/Overlapping IOTC _ WCPFC.Rmd"), output_format = rdocx_document(reference_docx = paste0(here("templates/doc_template2.docx"))), output_file = paste0(here("outputs/GTA_overlaps_IOTC_WCPFC.docx")))
render(here("rmd/Overlapping IATTC _ WCPFC.Rmd"), output_format = rdocx_document(reference_docx = paste0(here("templates/doc_template2.docx"))), output_file = paste0(here("outputs/GTA_overlaps_IATTC_WCPFC.docx")))
render(here("rmd/Overlapping_CCSBT_data.Rmd"), output_format = rdocx_document(reference_docx = paste0(here("templates/doc_template2.docx"))), output_file = paste0(here("outputs/GTA_overlaps_SBF.docx")))


