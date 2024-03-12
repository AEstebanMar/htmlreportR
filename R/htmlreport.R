#' Build HTML report from template
#'
#' This function builds an HTML report from a 
#' given template file for an object of class "htmlReport".
#' 
#' @param plotter An object of class "htmlReport".
#' @param template A character string specifying the file path of the template.
#' 
#' @return An object of class "htmlReport" with HTML t built from the template.
#' 
#' @details
#' This function reads the contents of the template file,
#' sets up Knitr options for rendering, 
#' renders the template using Knitr, and constructs 
#' the HTML report by combining the rendered 
#' template with the existing report content in the "htmlReport" object.
#' 
#' @examples
#' # Create an htmlReport object
#' plotter <- new("htmlReport")
#' 
#' # Build the report from a template
#' plotter <- build(plotter, "template.html")
#' 
#' @export
setGeneric("build", function(plotter, template) standardGeneric("build"))
setMethod("build", "htmlReport", function(plotter, template) {
	templ <- paste(readLines(template), collapse="\n")
	knitr::opts_chunk$set(echo = FALSE, 
												results="asis", 
												message=FALSE, 
												error = FALSE, 
												warning = FALSE)
	rendered_template <- knitr::knit(text = templ, quiet = TRUE)

	plotter <- plotter %+% "<HTML>\n"
	plotter <- make_head(plotter)
	plotter <- build_body(plotter, rendered_template)
  plotter <- plotter %+% "\n</HTML>"
  return(plotter)
})



#' Generate static plot for HTML report
#'
#' This function generates a static plot for inclusion in an 
#' HTML report for an object of class "htmlReport".
#' 
#' @param plotter An object of class "htmlReport".
#' @param id A character string specifying the 
#' identifier for the plot included in hash_vars.
#' @param header Logical, indicating whether the dataset has a header row.
#' @param row_names Logical, indicating whether to include row names.
#' @param transpose Logical, indicating whether to transpose the dataset.
#' @param smp_attr A list of attributes for samples.
#' @param var_attr A list of attributes for variables.
#' @param fields A character vector specifying the fields to include in the plot
#' @param func A function to preprocess data before plotting.
#' @param plotting_function A function used for generating the plot.
#' 
#' @return An object of class "htmlReport" with the static plot included.
#' 
#' @details
#' This function generates a static plot based on 
#' the provided data and plot specifications. It first 
#' retrieves the data frame for plotting using the provided options, 
#' preprocesses the data if a preprocessing 
#' function is specified, generates the plot using the provided 
#' plotting function, and then adds the plot 
#' to the HTML report object.
#' 
#' 
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


#' Generate static ggplot for HTML report
#'
#' This function generates a static ggplot for inclusion in an 
#' HTML report for an object of class "htmlReport".
#' 
#' @param plotter An object of class "htmlReport".
#' @param id A character string specifying the 
#' identifier for the element of hash_vars that is taken.
#' @param header Logical, indicating whether the dataset has a header row.
#' @param row_names Logical, indicating whether to include row names.
#' @param transpose Logical, indicating whether to transpose the dataset.
#' @param smp_attr A list of attributes for samples.
#' @param var_attr A list of attributes for variables.
#' @param fields A character vector specifying  fields to include in the ggplot.
#' @param func A function to preprocess data before plotting.
#' @param plotting_function A function used for generating the ggplot.
#' 
#' @return An object of class "htmlReport" with the static ggplot included.
#' 
#' @details
#' This function generates a static ggplot based on the 
#' provided data and plot specifications. It first 
#' defines a wrapper function for ggplot generation, then 
#' calls the \code{static_plot_main} function 
#' to generate the plot and include it in the HTML report object.
#' 
#' 
#' @export
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


#' Make HTML Report Head
#'
#' This method generates the head section of an 
#' HTML report by adding the title to an \code{htmlReport} object.
#' 
#' @param plotter An object of class \code{htmlReport}.
#'
#' @return An updated \code{htmlReport} object with the 
#' title added to its head section.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' plotter <- make_head(plotter)
#' }
setGeneric("make_head", function(plotter) standardGeneric("make_head"))
setMethod("make_head", "htmlReport", function(plotter) {
	title <- plotter@title
	plotter <- plotter %+% c("\t<title>", title, "</title>\n<head>\n")
	plotter <- plotter %+% "</head>\n"
  return(plotter)
})



#' Build HTML Report Body
#'
#' This method builds the body of an HTML report by 
#' appending the specified body text to an \code{htmlReport} object.
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param body_text A character string containing the body text 
#' to be appended to the HTML report.
#'
#' @return An updated \code{htmlReport} object with the specified 
#' body text appended to its body.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' body_text <- "<p>This is the body text of the report.</p>"
#' plotter <- build_body(plotter, body_text)
#' }
#'
setGeneric("build_body", function(
plotter, 
body_text) standardGeneric("build_body"))
setMethod("build_body", "htmlReport", function(plotter, body_text) {
	plotter <- plotter %+% body_text
    return(plotter)
})

#' Write HTML Report
#'
#' This method writes the HTML report generated by an 
#' \code{htmlReport} object to a specified output file path.
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param output_path A character string specifying 
#' the output file path for the HTML report.
#'
#' @return Writes the HTML report to the specified 
#' output file path and removes temporary files.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' output_path <- "report.html"
#' write_report(plotter, output_path)
#' }
#'
#' @export
setGeneric("write_report", function(
	plotter, 
	output_path) standardGeneric("write_report"))
setMethod("write_report", "htmlReport", function(plotter, output_path) {
	writeLines(plotter@all_report, output_path)
	unlink(plotter@tmp_dir, recursive = TRUE)
})


#' Get Plot from htmlReport Object
#'
#' This method generates and retrieves a plot from an \code{htmlReport} 
#' This code writes the plot to a temporal png, 
#' then it load the png in base64 encoding and then 
#' 
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param plot_obj A plot object like ggplot
#'
#' @return Displays the plot within the HTML report.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' # plot_obj is a valid plot object
#' get_plot(plotter, plot_obj)
#' }
setGeneric("get_plot", function(plotter, plot_obj) standardGeneric("get_plot"))
setMethod("get_plot", "htmlReport", function(plotter, plot_obj) {
	#This code writes the plot to a temporal png, 
	#then it load the png in base64 encoding and then 
	
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


#' Get Data for Plotting from htmlReport Object
#'
#' This method retrieves data suitable for plotting from an 
#' \code{htmlReport} object based on specified options.
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param options A list containing options for data retrieval.
#'
#' @return A list containing the retrieved data, 
#' attributes, samples, and variables.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' plotter <- new("htmlReport", hash_vars = list(data_id = df))
#' options <- list(id = "data_id", transpose = FALSE)
#' data <- get_data_for_plot(plotter, options)
#' }
setGeneric("get_data_for_plot", function(
	plotter, 
	options) standardGeneric("get_data_for_plot"))
setMethod("get_data_for_plot", "htmlReport", function(plotter, options) {
		all_data <- get_data(plotter, options)
		all_data <- c(all_data,
								list(samples = colnames(all_data$data_frame),
										variables = rownames(all_data$data_frame)))
		return(all_data)
})

#' Retrieve Data from htmlReport Object
#'
#' This method retrieves data from an 
#' \code{htmlReport} object based on specified options.
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param options A list containing options for data retrieval.
#'
#' @return A list containing the retrieved data and additional information.
#'   
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' plotter <- new("htmlReport", hash_vars = list(data_id = df))
#' options <- list(id = "data_id", transpose = FALSE)
#' data <- get_data(plotter, options)
#' }
#'
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
		if (!is.null(options$func)) 
			all_data$data_frame <- options$func(all_data$data_frame)
		return(all_data)				
})


#' Extract Data from htmlReport Object dataframe
#'
#' This method extracts data from an \code{htmlReport} 
#' object based on specified options.
#' 
#' @param plotter An object of class \code{htmlReport}.
#' @param data_frame A data frame containing the data to be extracted.
#' @param options A list containing options for data extraction.
#'  #'
#' @return A list containing the extracted data and additional information.
#'   
#' @examples
#' \dontrun{
#' plotter <- new("htmlReport")
#' data_frame <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' options <- list(smp_attr = NULL,
#'                 var_attr = NULL,
#'                 fields = NULL)
#' extracted_data <- extract_data(plotter, data_frame, options)
#' }
#'
setGeneric("extract_data", function(
	plotter, 
	data_frame, 
	options) standardGeneric("extract_data"))
setMethod("extract_data", "htmlReport", function(
	plotter, 
	data_frame, 
	options) {	
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
  	if(length(numeric_columns) > 0)
			data_frame[,numeric_columns] <- lapply(data_frame[,numeric_columns],
																						 as.numeric)

		return(list(data_frame = data_frame,
								smp_attr = smp_attr,
								var_attr = var_attr))
})


#' Add Header and Row Names to Data Frame for HTML Report table
#'
#' This function modifies a data frame to include specific column and row names
#' 
#' @param plotter The plotter object associated with the HTML report.
#' @param data_frame The data frame to be modified.
#' @param options A list of options controlling the 
#' modification of column and row names.
#'
#' @details This function checks the options provided and manipulates the 
#' column and row names of the input data frame accordingly. If the 'header' 
#' option is set to true, it assigns the first row of the data frame as column 
#' names and removes that row from the data frame. If the 'row_names' 
#' option is true, it assigns the first column of the data frame as row names 
#' and removes that column from the data frame. If either option is not true, 
#' it assigns sequential numbers as column or row names, respectively.
#' 
#' @return The modified data frame with updated column and/or row names.
#' 
#' @examples
#' # Create sample data frame
#' \dontrun{
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' 
#' # Define options
#' options <- list(header = TRUE, row_names = TRUE)
#' 
#' # Apply the function
#' modified_df <- add_header_row_names(plotter, df, options)
#'} 
setGeneric("add_header_row_names", function(
	plotter, 
	data_frame, 
	options) standardGeneric("add_header_row_names"))
setMethod("add_header_row_names", "htmlReport", function(
	plotter, 
	data_frame, 
	options) {	
		if (options$header) {
			colnames(data_frame) <- data_frame[1,]
			data_frame <- data_frame[-1,]
		} 
		if (options$row_names) {
			rownames(data_frame) <- data_frame[,1]
			data_frame <- data_frame[,-1]
		}

		if (!options$header)
			colnames(data_frame) <- seq(1,ncol(data_frame))
		if (!options$row_names)
			rownames(data_frame) <- as.character(seq(1,nrow(data_frame)))

		return(data_frame)	
})



#' Custom addition operator for combining htmlReport objects
#'
#' This function defines a custom addition operator for combining two strings. 
#' 
#' @param plotter An object of class "htmlReport".
#' @param value An object of any type that can be coerced to character.
#' 
#' @return An object of class "htmlReport" with an updated @all_report 
#' which includes at the end the "value" string
#' 
#' @examples
#' \dontrun{
#' # Create two htmlReport objects
#' plotter <- new("htmlReport")
#' plotter <- plotter %+% "<h1>First Report</h1>"
#'} 
setGeneric("%+%", function(plotter, value) standardGeneric("%+%"))
setMethod("%+%", "htmlReport", function(plotter, value) {
	plotter@all_report <- paste(c(plotter@all_report,	value), collapse = "")
	return(plotter)
})