#' @importFrom methods new
#' @importFrom utils read.table
main_htmlreportR <- function(options){
	table_list <- strsplit(options$data_files, ",")[[1]]
	data_files <- lapply(table_list, utils::read.table, header = FALSE)
	names(data_files) <- sapply(table_list, basename)

	if (is.null(options$output_file)){
	  output_file <- file.path(dirname(options$template), "report.html")
	} else {
	  output_file <- options$output_file
	}

	tmp_folder <- file.path(dirname(output_file), "tmp")

	plotter <- htmlReport$new(title_doc = options$title, 
						      container = data_files, 
		                      tmp_folder = tmp_folder)
	
	plotter$build(options$template)
	plotter$write_report(output_file)

}