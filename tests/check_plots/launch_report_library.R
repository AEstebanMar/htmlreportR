#! /usr/bin/env Rscript

devtools::load_all("../../")

load("enrichments_mf.Rdata")

container <- list(enrichments_mf = enrichments_mf)

template <- file.path("template_lib.txt")

tmp_folder <- "tmp_lib"
plotter <- htmlReport$new(title_doc = "Testing lib mode report", 
						      container = container, 
		                      tmp_folder = tmp_folder)

plotter$build(template)

plotter$write_report("report_lib.html")

