#! /usr/bin/env Rscript

devtools::load_all("../../")

source_folder <- find.package('htmlreportR')

if( Sys.getenv('HTMLREPORTER_MODE') == 'DEVELOPMENT' )
	source_folder <- file.path(source_folder, "inst")
	
load("enrichments_mf.Rdata")

plot_data <- data.frame(V1= 1:10, V2=c(10:5,1:4))

container <- list(enrichments_mf = enrichments_mf,
		  plot_data = plot_data)


template <- file.path("template_lib.txt")

tmp_folder <- "tmp_lib"
plotter <- htmlReport$new(title_doc = "Testing lib mode report", 
				container = container, 
		                tmp_folder = tmp_folder,
		                src = source_folder)

plotter$build(template)

plotter$write_report("report_lib.html")

