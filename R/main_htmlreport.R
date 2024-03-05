
main_htmlreportR <- function(options){
	table_list <- strsplit(options$data_files, ",")[[1]]
	data_files <- lapply(table_list, read.table, header = FALSE)
	names(data_files) <- sapply(table_list, basename)

	if (is.null(options$output_file)){
	  output_file <- file.path(dirname(options$template), "report.html")
	} else {
	  output_file <- options$output_file
	}

	tmp_dir <- file.path(dirname(output_file), "tmp")
	dir.create(tmp_dir)

	html_report <- new("htmlReport", title = options$title, hash_vars = data_files, tmp_dir = tmp_dir)

	html_report <- build(html_report, options$template)
	write(html_report, output_file)

}