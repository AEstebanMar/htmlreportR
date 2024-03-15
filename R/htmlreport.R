
#####################################################################
## Public Methods
#####################################################################


#' Build HTML report from template
#'
#' @name build-htmlReport-method
#' @title Build HTML report from template
#' @description This function builds an HTML report from a given 
#' template file for an object of class "htmlReport".
#' 
#' @param template A character string specifying the file path of the template.
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
#' \dontrun{
#' plotter <- htmlReport$new()
#' 
#' # Build the report from a template
#' plotter$build("template.html")
#' }
#' @importFrom knitr knit opts_chunk
NULL
htmlReport$methods(
	build = function(template) {
		templ <- paste(readLines(template), collapse="\n")
		knitr::opts_chunk$set(echo = FALSE, 
							  results="asis", 
							  message=FALSE, 
							  error = FALSE, 
							  warning = FALSE)
		plotter <- .self
		rendered_template <- knitr::knit(text = templ, quiet = TRUE)
		concat("<HTML>\n")
		make_head()
		build_body(rendered_template)

	 	concat("\n</HTML>")
	}
)




#' Generate static plot for HTML report
#'
#' @name static_plot_main-htmlReport-method
#' @title Generate static plot for HTML report
#' @description This function generates a static plot for inclusion in an HTML 
#' report for an object of class "htmlReport".
#' 
#' @param id A character string specifying the identifier for the plot included in hash_vars.
#' @param header Logical, indicating whether the dataset has a header row.
#' @param row_names Logical, indicating whether to include row names.
#' @param transpose Logical, indicating whether to transpose the dataset.
#' @param smp_attr A list of attributes for samples.
#' @param var_attr A list of attributes for variables.
#' @param fields A character vector specifying the fields to include in the plot.
#' @param func A function to preprocess data before plotting.
#' @param plotting_function A function used for generating the plot.
#' @param text Logical, indicating whether to convert table to text or a vector indicating the numeric fields.
#' 
#' @details
#' This function generates a static plot based on the provided data and plot specifications. 
#' It first retrieves the data frame for plotting using the provided options, preprocesses the 
#' data if a preprocessing function is specified, generates the plot using the provided plotting 
#' function, and then adds the plot to the HTML report object.
#' 
NULL
htmlReport$methods(static_plot_main = function(id, 
											   header = FALSE, 
											   row_names = FALSE,
											   transpose = FALSE,
											   smp_attr = NULL,
											   var_attr = NULL,
											   fields = NULL,
											   func = NULL,
											   plotting_function = NULL,
											   text = FALSE) {

	options <- list(id = id,
					header = header,
					row_names = row_names,
					transpose = transpose,
					smp_attr = smp_attr,
					var_attr = var_attr,
					fields = fields,
					func = func,
					text = text)

	data_frame <- get_data_for_plot(options)$data_frame
	if(is.null(plotting_function)) return(data_frame)
	plot_obj <- plotting_function(data_frame)
	get_plot(plot_obj)
})



#' Generate static ggplot for HTML report
#'
#' @name static_ggplot_main-htmlReport-method
#' @title Generate static ggplot for HTML report
#' @description This function generates a static ggplot for 
#' inclusion in an HTML report for an object of class "htmlReport".
#' 
#' @param id A character string specifying the identifier 
#' for the element of hash_vars that is taken.
#' @param header Logical, indicating whether the dataset has a header row.
#' @param row_names Logical, indicating whether to include row names.
#' @param transpose Logical, indicating whether to transpose the dataset.
#' @param smp_attr A list of attributes for samples.
#' @param var_attr A list of attributes for variables.
#' @param fields A character vector specifying fields to include in the ggplot.
#' @param func A function to preprocess data before plotting.
#' @param plotting_function A function used for generating the ggplot.
#' @param text Logical, indicating whether to convert table to text or a vector indicating the numeric fields.
#' 
#' @details
#' This function generates a static ggplot based on the provided data 
#' and plot specifications. It first defines a wrapper function for ggplot 
#' generation, then calls the \code{static_plot_main} function to generate the 
#' plot and include it in the HTML report object.
#' 
#' @importFrom ggplot2 ggplot
NULL
htmlReport$methods(
	static_ggplot_main = function(id, 
								 header = FALSE, 
								 row_names = FALSE,
								 transpose = FALSE,
								 smp_attr = NULL,
								 var_attr = NULL,
								 fields = NULL,
								 func = NULL,
								 plotting_function = NULL,
								 text = TRUE) {
	ggplot_f <- function(data_frame, plotting_function_gg = plotting_function){
				ggplot_obj <- ggplot2::ggplot(data_frame)
				plotting_function_gg(ggplot_obj)
	}

	static_plot_main(id = id,
					 header = header,
					 row_names = row_names,
					 transpose = transpose,
					 smp_attr = smp_attr,
					 var_attr = var_attr,
					 fields = fields,
					 func = func, 
					 plotting_function = ggplot_f,
					 text = text)
})



#' Write HTML Report
#'
#' @name write_report-htmlReport-method
#' @title Write HTML Report
#' @description This method writes the HTML report generated by an \code{htmlReport} object to a specified output file path.
#' 
#' @param output_path A character string specifying the output file path for the HTML report.
#' 
#' @return Writes the HTML report to the specified output file path and removes temporary files.
#' 
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' plotter <- htmlReport$new()
#' output_path <- "report.html"
#' plotter$write_report(output_path)
#' }
#' 
NULL
htmlReport$methods(write_report = function(output_path) {
	writeLines(all_report, output_path)
	unlink(tmp_dir, recursive = TRUE)
})

#####################################################################
## Private Methods
#####################################################################

#' Make HTML Report Head
#'
#' @name make_head
#' @title Make HTML Report Head
#' @description This method generates the head section of an HTML report by adding the title to an \code{htmlReport} object.
#' 
#' @return An updated \code{htmlReport} object with the title added to its head section.
#' 
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' plotter <- htmlReport$new()
#' plotter$make_head()
#' }
#' 
NULL
htmlReport$methods(make_head = function() {
	concat(c("\t<title>", title, "</title>\n<head>\n"))

	concat("</head>\n")
})



#' Build HTML Report Body
#'
#' @name build_body
#' @title Build HTML Report Body
#' @description This method builds the body of an HTML report 
#' by appending the specified body text to an \code{htmlReport} object.
#' 
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
#' plotter$build_body(body_text)
#' }
#' 
NULL
htmlReport$methods(build_body = function(body_text) {
	concat(body_text)
})




#' Get Plot from htmlReport Object
#'
#' @name get_plot
#' @title Get Plot from htmlReport Object
#' @description This method generates and retrieves a plot from an \code{htmlReport} object. This code writes the plot to a temporal png, then it loads the png in base64 encoding and then displays the plot within the HTML report.
#' 
#' @param plot_obj A plot object like ggplot.
#'
#' @return Displays the plot within the HTML report.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' # plot_obj is a valid plot object
#' plotter$get_plot(plot_obj)
#' }
#' 
#' @importFrom knitr opts_current
#' @importFrom grDevices png dev.off
NULL
htmlReport$methods(get_plot = function(plot_obj) {
	file_png <- file.path(tmp_dir, "tmp_fig.png")
  	grDevices::png(file_png, 
		width = knitr::opts_current$get("fig.width"),
		height = knitr::opts_current$get("fig.height"),
		units = "in",
		res = 200)
		plot(plot_obj)
	grDevices::dev.off()
	enc_img <- embed_file(file_png)
	cat(paste0("\n<img src=", enc_img, " />"))
})


#' Get Data for Plotting from htmlReport Object
#'
#' @name get_data_for_plot
#' @title Get Data for Plotting from htmlReport Object
#' @description This method retrieves data suitable for plotting from an \code{htmlReport} object based on specified options.
#' 
#' @param options A list containing options for data retrieval.
#'
#' @return A list containing the retrieved data, attributes, samples, and variables.
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' options <- list(id = "data_id", transpose = FALSE)
#' data <- plotter$get_data_for_plot(options)
#' }
NULL
htmlReport$methods(get_data_for_plot = function(options) {
		all_data <- get_data(options)
		all_data <- c(all_data,
					 list(samples = colnames(all_data$data_frame),
						  variables = rownames(all_data$data_frame)))
		return(all_data)
})

#' Retrieve Data from htmlReport Object
#'
#' This method retrieves data from an 
#' \code{htmlReport} object based on specified options.
#' @name get_data
#' @param options A list containing options for data retrieval.
#'
#' @return A list containing the retrieved data and additional information.
#'   
#'
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' options <- list(id = "data_id", transpose = FALSE)
#' data <- plotter$get_data(options)
#' }
#'
NULL
htmlReport$methods(get_data = function(options) {
		data_frame <- hash_vars[[options$id]]
		#add_header_row_names
		data_frame <- add_header_row_names(data_frame, options)

		#transpose
		if (options$transpose) data_frame <- as.data.frame(t(data_frame))
		#extract data
		all_data <- extract_data(data_frame, options)
		#modification function
		if (!is.null(options$func)) 
			all_data$data_frame <- options$func(all_data$data_frame)
		return(all_data)				
})


#' Retrieve Data from htmlReport Object
#'
#' @name get_data
#' @title Retrieve Data from htmlReport Object
#' @description This method retrieves data from an \code{htmlReport} object based on specified options.
#' 
#' @param options A list containing options for data retrieval.
#' 
#' @return A list containing the retrieved data and additional information.
#'   
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' options <- list(id = "data_id", transpose = FALSE)
#' data <- plotter$get_data(options)
#' }
#'
NULL
htmlReport$methods(extract_data = function(data_frame, 	options) {	
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

    numeric_fields <- NULL
	if (is.numeric(options$text)) {
		numeric_fields <- options$text
	} else if (options$text == FALSE) {
		numeric_fields <- seq(1,ncol(data_frame))		
	}
	if (!is.null(numeric_fields))
		data_frame[,numeric_fields] <- as.data.frame(lapply(
				data_frame[,numeric_fields], as.numeric))

		return(list(data_frame = data_frame,
								smp_attr = smp_attr,
								var_attr = var_attr))
})


#' Add Header and Row Names to Data Frame for HTML Report table
#'
#' @name add_header_row_names
#' @title Add Header and Row Names to Data Frame for HTML Report table
#' @description This function modifies a data frame to include specific column and row names.
#' 
#' @param data_frame The data frame to be modified.
#' @param options A list of options controlling the modification of column and row names.
#'
#' @details This function checks the options provided and manipulates the column and row names of the input data frame accordingly. If the 'header' option is set to true, it assigns the first row of the data frame as column names and removes that row from the data frame. If the 'row_names' option is true, it assigns the first column of the data frame as row names and removes that column from the data frame. If either option is not true, it assigns sequential numbers as column or row names, respectively.
#' 
#' @return The modified data frame with updated column and/or row names.
#' 
#' @examples
#' # Create sample data frame
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' df <- data.frame(A = c(1, 2, 3), B = c(4, 5, 6), C = c(7, 8, 9))
#' 
#' # Define options
#' options <- list(header = TRUE, row_names = TRUE)
#' 
#' # Apply the function
#' modified_df <- plotter$add_header_row_names(df, options)
#' } 
#' 
NULL
htmlReport$methods(add_header_row_names = function(data_frame, options) {	
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
#' @name concat
#' @title Custom addition operator for combining htmlReport objects
#' @description This function defines a custom addition operator for combining two strings.
#' 
#' @param value An object of any type that can be coerced to character.
#' 
#' @return An object of class "htmlReport" with an updated @all_report which includes at the end the "value" string.
#' 
#' @examples
#' \dontrun{
#' # Assuming plotter is an object of class htmlReport
#' plotter$concat("<h1>First Report</h1>")
#'}
NULL
htmlReport$methods(concat = function(text_vec) {
	all_report <<- paste(c(all_report, text_vec), collapse = "")
})



htmlReport$methods(test_f = function(){
css_files <<- "test"
})