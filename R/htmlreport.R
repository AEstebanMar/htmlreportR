setClass("htmlReport", 
  slots = c(
    hash_vars = "list",
    all_report = "character", 
    title = "character",
    tmp_dir = "character"
  ),
   prototype = list(
    hash_vars = list(),
    all_report = "", 
    title = "",
    tmp_dir = ""
  )
)


setGeneric("%+%", function(plotter, value) standardGeneric("%+%"))
setMethod("%+%", "htmlReport", function(plotter, value) {
	paste(c(plotter@all_report,	value), collapse = "")
})

setGeneric("build", function(plotter, template) standardGeneric("build"))
setMethod("build", "htmlReport", function(plotter, template) {
	templ <- paste(readLines(template), collapse="\n")
	knitr::opts_chunk$set(echo = FALSE, results="asis", message=FALSE, error = FALSE, warning = FALSE)
	rendered_template <- knitr::knit(text = templ, quiet = TRUE)

	plotter@all_report <- plotter %+% "<HTML>\n"
	plotter <- make_head(plotter)
	plotter <- build_body(plotter, rendered_template)
  plotter@all_report <- plotter %+% "\n</HTML>"
  return(plotter)
})


## value es un nombre generico para poder usar este método como paste_report(plotter) <- c("texto")
setGeneric("paste_report<-", function(plotter, value) standardGeneric("paste_report<-"))
setMethod("paste_report<-", "htmlReport", function(plotter, value) {
	plotter@all_report <- paste(c(plotter@all_report,
					value), collapse = "")
    return(plotter)
})

setGeneric("make_head", function(plotter) standardGeneric("make_head"))
setMethod("make_head", "htmlReport", function(plotter) {
	title <- plotter@title
	plotter@all_report <- plotter %+% c("\t<title>", title, "</title>\n<head>\n")
	plotter@all_report <- plotter %+% "</head>\n"
  return(plotter)
})

setGeneric("build_body", function(plotter, body_text) standardGeneric("build_body"))
setMethod("build_body", "htmlReport", function(plotter, body_text) {
	plotter@all_report <- plotter %+% body_text
    return(plotter)
})

setGeneric("write_report", function(plotter, output_path) standardGeneric("write_report"))
setMethod("write_report", "htmlReport", function(plotter, output_path) {
	writeLines(plotter@all_report, output_path)
	unlink(plotter@tmp_dir, recursive = TRUE)
})


setGeneric("static_plot_main", function(
plotter, 
id, 
header = FALSE, 
row_names = FALSE,
transpose = FALSE,
smp_attr = NULL,
var_attr = NULL,
fields = NULL,
func = NULL,
plotting_function = NULL
) standardGeneric("static_plot_main"))
setMethod("static_plot_main", "htmlReport", function(
plotter, 
id, 
header = FALSE, 
row_names = FALSE,
transpose = FALSE,
smp_attr = NULL,
var_attr = NULL,
fields = NULL,
func = NULL,
plotting_function = NULL) {

	options <- list(id = id,
									header = header,
									row_names = row_names,
									transpose = transpose,
									smp_attr = smp_attr,
									var_attr = var_attr,
									fields = fields,
									func = func)
	data_frame <- get_data_for_plot(plotter, options)$data_frame
	if(is.null(plotting_function)) return(data_frame)
	plot_obj <- plotting_function(data_frame)
	get_plot(plotter, plot_obj)
})

setGeneric("static_ggplot_main", function(
plotter, 
id, 
header = FALSE, 
row_names = FALSE,
transpose = FALSE,
smp_attr = NULL,
var_attr = NULL,
fields = NULL,
func = NULL,
plotting_function = NULL) standardGeneric("static_ggplot_main"))
setMethod("static_ggplot_main", "htmlReport", function(
plotter, 
id, 
header = FALSE, 
row_names = FALSE,
transpose = FALSE,
smp_attr = NULL,
var_attr = NULL,
fields = NULL,
func = NULL,
plotting_function = NULL) {
	ggplot_f <- function(data_frame, plotting_function_gg = plotting_function){
				ggplot_obj <- ggplot2::ggplot(data_frame)
				plotting_function_gg(ggplot_obj)
	}

	static_plot_main(plotter =plotter,
									id = id,
									header = header,
									row_names = row_names,
									transpose = transpose,
									smp_attr = smp_attr,
									var_attr = var_attr,
									fields = fields,
									func = func, 
									plotting_function = ggplot_f)
})

setGeneric("get_plot", function(plotter, plot_obj) standardGeneric("get_plot"))
setMethod("get_plot", "htmlReport", function(plotter, plot_obj) {
	#This code writes the plot to a temporal png, then it load the png in base64 encoding and then 
	
	file_png <- file.path(plotter@tmp_dir, "tmp_fig.png")
  png(file_png, 
		width = knitr::opts_current$get("fig.width"),
		height = knitr::opts_current$get("fig.height"),
		units = "in",
		res = 200)
	  plot(plot_obj)
	dev.off()
	enc_img <- embed_file(file_png)
	cat(paste0("\n<img src=", enc_img, " />"))
	
})


embed_file <- function(img_file) {
    xfun::base64_uri(img_file)
}


setGeneric("get_data_for_plot", function(plotter, options) standardGeneric("get_data_for_plot"))
setMethod("get_data_for_plot", "htmlReport", function(plotter, options) {
		all_data <- get_data(plotter, options)
		all_data <- c(all_data,
								list(samples = colnames(all_data$data_frame),
										variables = rownames(all_data$data_frame)))
		return(all_data)
})


setGeneric("get_data", function(plotter, options) standardGeneric("get_data"))
setMethod("get_data", "htmlReport", function(plotter, options) {
		data_frame <- plotter@hash_vars[[options$id]]
		#add_header_row_names
		data_frame <- add_header_row_names(plotter, data_frame, options)

		#transpose
		if (options$transpose) data_frame <- as.data.frame(t(data_frame))
		#extract data
		all_data <- extract_data(plotter, data_frame, options)
		#modification function
		if (!is.null(options$func)) all_data$data_frame <- options$func(all_data$data_frame)
		return(all_data)				
})

setGeneric("extract_data", function(plotter, data_frame, options) standardGeneric("extract_data"))
setMethod("extract_data", "htmlReport", function(plotter, data_frame, options) {	
		smp_attr <- NULL #length(NULL) ==> 0
    var_attr <- NULL
    
    if (length(options$smp_attr) > 0){
    	smp_attr <- data_frame[,options$smp_attr, drop = FALSE]
    	data_frame <- data_frame[,-options$smp_attr, drop = FALSE]
    } 
    if (length(options$var_attr) > 0){
    	var_attr <- data_frame[options$var_attr,, drop = FALSE]
    	data_frame <- data_frame[-options$var_attr,, drop = FALSE]
    }
    
    if (length(options$smp_attr) > 0 && 
    		length(options$var_attr) > 0){
    	smp_attr <- smp_attr[-options$var_attr,,drop = FALSE]    	
    } 
    if (length(options$fields > 0))
    data_frame <- data_frame[,options$fields, drop = FALSE]
  	numeric_columns <- grepl("^\\d*\\.?\\d+$", data_frame[1,])
		data_frame[,numeric_columns] <- lapply(data_frame[,numeric_columns], as.numeric)

		return(list(data_frame = data_frame,
								smp_attr = smp_attr,
								var_attr = var_attr))
})


setGeneric("add_header_row_names", function(plotter, data_frame, options) standardGeneric("add_header_row_names"))
setMethod("add_header_row_names", "htmlReport", function(plotter, data_frame, options) {	
		if (options$header) {
			colnames(data_frame) <- data_frame[1,]
			data_frame <- data_frame[-1,]
		}
		if (options$row_names) {
			rownames(data_frame) <- data_frame[,1]
			data_frame <- data_frame[,-1]
		}
		return(data_frame)	
})