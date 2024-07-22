#' @importFrom methods new
#' @importFrom utils read.table
#' @return
#' @export
#' @examples
main_htmlreportR <- function(options){
	data_files <- list()
	if (!is.null(options$data_files)){
		table_list <- strsplit(options$data_files, ",")[[1]]
		data_files <- lapply(table_list, utils::read.table, header = FALSE)
		names(data_files) <- sapply(table_list, basename)
	}

	if (is.null(options$output_file)){
	  output_file <- file.path(dirname(options$template), "report.html")
	} else {
	  output_file <- options$output_file
	}

	tmp_folder <- file.path(dirname(output_file), "tmp")

	plotter <- htmlReport$new(title_doc = options$title, 
						      container = data_files, 
		                      tmp_folder = tmp_folder,
		                      src = options$source_folder,
		                      compress_obj = options$uncompressed_data,
		                      files_css = options$css_files,
		                      files_js = options$js_files,
		                      cdn_css = options$css_cdn,
		                      cdn_js = options$js_cdn,
		                      type_index = options$menu)
	
	plotter$build(options$template)
	plotter$write_report(output_file)

}