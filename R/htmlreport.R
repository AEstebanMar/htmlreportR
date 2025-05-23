
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
#' @returns A new instance of the htmlreportR class
#' @importFrom knitr knit opts_chunk
#'

NULL
htmlReport$methods(
	build = function(template) {
		templ <- paste(readLines(template, warn = FALSE), collapse="\n")
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
#' @param custom_format Logical, indicating if id correspond to a table or a custom object
#' @param width plot width
#' @param height plot height
#' @param size_unit units in plot size
#' @param img_properties string including html properties of img
#' @param resizable logical indicating if plot must be resizable
#' @param classic_R_plot logical indicating if is using classic plot() R function
#' 
#' @details
#' This function generates a static plot based on the provided data and plot specifications. 
#' It first retrieves the data frame for plotting using the provided options, preprocesses the 
#' data if a preprocessing function is specified, generates the plot using the provided plotting 
#' function, and then adds the plot to the HTML report object.
#' 
NULL
htmlReport$methods(static_plot_main = function(id, header = NULL, 
											   row_names = NULL,
											   transpose = FALSE,
											   smp_attr = NULL, var_attr = NULL,
											   fields = NULL, func = NULL,
											   plotting_function = NULL,
											   text = FALSE,
											   custom_format = FALSE,
											   width = NULL, height = NULL, 
											   size_unit = NULL, 
											   img_properties = "",
											   resizable = FALSE,
											   plot_type = "plot") {
	options <- list(id = id, header = header, row_names = row_names,
					transpose = transpose, smp_attr = smp_attr, text = text,
					var_attr = var_attr, fields = fields, func = func)
	if (custom_format) {
		data_frame <- hash_vars[[id]]
	} else {
		data_frame <- get_data_for_plot(options)$data_frame
	}
	if(is.null(plotting_function)) {
		return(data_frame)
	}else if (plot_type == "autoplot") {
		# This next line allows the "data_frame" object to exist inside
		# the scope of the evaluated function, and plot_obj to be a function
		# instead of the result of calling the plotting function.
		aux_func <- function(data_frame){
			eval(parse(text = paste(deparse(plotting_function),
					   collapse ="\n")))}
		plot_obj <- aux_func(data_frame)
	}else {
		plot_obj <- plotting_function(data_frame)
	}
	get_plot(plot_obj, width = width, height = height, size_unit = size_unit,
			 img_properties = img_properties, resizable = resizable,
			 plot_type = plot_type)
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
#' @param width plot width
#' @param height plot height
#' @param size_unit units in plot size
#' @param img_properties string including html properties of img
#' @param resizable logical indicating if plot must be resizable
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
	static_ggplot_main = function(id, header = NULL, row_names = NULL,
								 transpose = FALSE, smp_attr = NULL,
								 var_attr = NULL, fields = NULL, func = NULL,
								 plotting_function = NULL, text = TRUE,
								 width = NULL, height = NULL, size_unit = NULL,
								 img_properties = "", resizable = FALSE) {
	ggplot_f <- function(data_frame, plotting_function_gg = plotting_function){
				ggplot_obj <- ggplot2::ggplot(data_frame)
				plotting_function_gg(ggplot_obj)
	}
	static_plot_main(id = id, header = header, row_names = row_names,
					 transpose = transpose, smp_attr = smp_attr,
					 var_attr = var_attr, fields = fields, func = func, 
					 plotting_function = ggplot_f, text = text, width = width, 
					 height = height,  size_unit = size_unit,
					 img_properties = img_properties, resizable = resizable)})

#' Write HTML Report
#'
#' @name write_report-htmlReport-method
#' @title Write HTML Report
#' @description This method writes the HTML report generated by an \code{htmlReport} object to a specified output file path.
#' 
#' @param output_path A character string specifying the output file path for the HTML report.
#' 
#' @returns Writes the HTML report to the specified output file path and removes temporary files.
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
#' @returns An updated \code{htmlReport} object with the title added to its head section.
#' 
NULL
htmlReport$methods(make_head = function() {
	concat(c("\t<title>", title, "</title>",
			"\n<head>\n", 
			"<meta charset=\"utf-8\">\n",
			"<meta http-equiv=\"CACHE-CONTROL\" CONTENT=\"NO-CACHE\">\n",
            "<meta http-equiv=\"Content-Type\" content=\"text/html; charset=utf-8\" />\n",
            "<meta http-equiv=\"Content-Language\" content=\"en-us\" />\n",
            "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">\n\n"))
	
	css_cdn <<- c(css_cdn, 
		'https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css')
	
	js_cdn <<- c(js_cdn,
            'https://code.jquery.com/jquery-3.7.1.js')
	
	if (features$dt_tables){ # CDN load, this library is difficult to embed in html file

        css_cdn <<- c(css_cdn, 
            'https://cdn.datatables.net/2.0.0/css/dataTables.dataTables.css',
            'https://cdn.datatables.net/buttons/3.0.0/css/buttons.dataTables.css')
        js_cdn <<- c(js_cdn,
            'https://cdn.datatables.net/2.0.0/js/dataTables.js',
            'https://cdn.datatables.net/buttons/3.0.0/js/dataTables.buttons.js',
            'https://cdn.datatables.net/buttons/3.0.0/js/buttons.dataTables.js',
            'https://cdnjs.cloudflare.com/ajax/libs/jszip/3.10.1/jszip.min.js',
            'https://cdn.datatables.net/buttons/3.0.0/js/buttons.html5.min.js')

        if (features$pdfHtml5){

            js_cdn <<- c(js_cdn,
                'https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/pdfmake.min.js',
                'https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.2.7/vfs_fonts.js')
        }
	} 

	if (features$mermaid) 
		js_cdn <<- c(js_cdn,
		"<script type=\"module\"> import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@10/dist/mermaid.esm.min.mjs'; </script>")

	css_files <<- c(css_files, "htmlReport.css")
	js_files <<- c(js_files, "htmlReport.js")
	
	if (features$pako) js_files <<- c(js_files, 'pako.min.js')

	if (features$canvasXpress){
		js_files <<- c(js_files, 'canvasXpress.min.js.gz')
        css_files <<- c(css_files, 'canvasXpress.css')
	}

	concat(get_css_cdn())
	concat(get_js_cdn())

	load_js()
	load_css()


    add_dynamic_js()

	concat("</head>\n")
})




htmlReport$methods(add_dynamic_js = function(){
	string_chunks <- paste(dynamic_js, collapse = "\n")
	concat(paste(c("<script>", string_chunks, "</script>",""), collapse = "\n"))
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
#' @returns An updated \code{htmlReport} object with the specified 
#' body text appended to its body.
#' 
NULL
htmlReport$methods(build_body = function(body_text) {
	concat("<body>\n")
	if (length(index_items) > 0){
		if (index_type == "menu") {
			add_index_item("top_skip", "Main", min(as.numeric(index_items[,3])), top = TRUE)
			concat("<div id = 'top_skip'></div>")
		}
		create_header_index()
	}
	concat(body_text)
	concat("</body>\n")

})

htmlReport$methods(
	create_header_index = function(){
		last_level <- 0
		index_numbers <- as.numeric(index_items[, 3])
		index_numbers <- index_numbers - (min(index_numbers) - 1)
		top_level <- min(index_numbers)
		if (index_type == "menu") {
			index_items <<- index_items[index_numbers == top_level, ,
										drop = FALSE]
			index_numbers <- NULL
			div_id <- " id=\"floating-menu\""
			index <- NULL
		} else {
			index <- "<h1>Table of contents</h1>"
			div_id <- NULL
		}
		ids <- index_items[, 1]
		content <- index_items[, 2]
		list_content <- paste0("<a href = #", ids, ">", content, "</a>")
		html_list <- make_html_list(list_content = list_content,
									list_levels = index_numbers)
		index <- paste0(index, "\n<div", div_id, ">\n", html_list, "</div>")
		concat(index)
})

htmlReport$methods(add_index_item = function(id, text, hlevel, top = FALSE){
	if(nrow(index_items) == 0) {
 		index_items <<- matrix(c(id, text, hlevel), nrow = 1, ncol = 3)
	} else if (!top){
		index_items <<- rbind(index_items, c(id, text, hlevel))
	} else if (top) {
		index_items <<- rbind(c(id, text, hlevel), index_items)
	}
})

htmlReport$methods(create_title = function(text, id, hlevel = 1, indexable = FALSE, clickable = FALSE, t_id = NULL, clickable_text = "(Click me)"){

	if (indexable) add_index_item(id, text, hlevel)

    header <- paste0("<h", hlevel, " id=\"", id, "\">", text, "</h", hlevel, ">")

    if (clickable && !is.null(t_id))
        header <- paste0("<h", hlevel, " id=\"", id, "\" class=\"py_accordion\" onclick=\"hide_show_element('", t_id, "')\">", text, " ", clickable_text, "</h", hlevel, ">")
    
    return(header)
})



htmlReport$methods(
	create_collapsable_container = function(id, html_code, visibility='hidden'){
		init_height <- ""
		if(visibility == "hidden"){
			init_height	<- "height:1px"
		}
        paste0("<div style=\"visibility:", visibility, "; ", init_height, "\" id=\"", id, "\">\n", html_code, "\n</div>")
})

#' Get Plot from htmlReport Object
#'
#' @name get_plot
#' @title Get Plot from htmlReport Object
#' @description This method generates and retrieves a plot from an \code{htmlReport} object. This code writes the plot to a temporal png, then it loads the png in base64 encoding and then displays the plot within the HTML report.
#' 
#' @param plot_obj A plot object like ggplot.
#' @param width plot width
#' @param height plot height
#' @param size_unit units in plot size
#' @param img_properties string including html properties of img
#' @param resizable logical indicating if plot must be resizable
#' 
#' @returns Displays the plot within the HTML report.
#' 
#' @importFrom knitr opts_current
#' @importFrom grDevices png dev.off
NULL
htmlReport$methods(get_plot = function(plot_obj, width = NULL, height = NULL,
				   size_unit = NULL, img_properties = "", resizable = FALSE,
				   plot_type = "plot") {
	if (is.character(plot_obj)) return(plot_obj)
	if (is.null(width)) width <- knitr::opts_current$get("fig.width")
	if (is.null(height)) height <- knitr::opts_current$get("fig.height")
	if (is.null(size_unit)) size_unit <- "in"
	
	file_png <- file.path(tmp_dir, "tmp_fig.png")
  	grDevices::png(file_png, width = width, height = height, units = size_unit,
  				   res = 200)
  	if(is.function(plot_obj) && plot_type == "autoplot"){
  		plot_obj()
  	} else if (plot_type == "plot"){
		plot(plot_obj)
  	} else if (plot_type == "print") {
  		print(plot_obj)
  	} 
	grDevices::dev.off()
	embed_img(file_png, img_properties, resizable = resizable)
})


htmlReport$methods(
	embed_img = function(file_img, img_properties = "", resizable = FALSE) {
		enc_img <- embed_file(file_img)
	if (resizable) {
		img_properties <- paste0(img_properties, " class='fitting_img' ")
		make_resizable(paste0("\n<img ",  img_properties ," src=", enc_img,
							  " />"))	
	} else {
		paste0("\n<img ",  img_properties ," src=", enc_img, " />")
	
	}
})


#' Get Data for Plotting from htmlReport Object
#'
#' @name get_data_for_plot
#' @title Get Data for Plotting from htmlReport Object
#' @description This method retrieves data suitable for plotting from an \code{htmlReport} object based on specified options.
#' 
#' @param options A list containing options for data retrieval.
#'
#' @returns A list containing the retrieved data, attributes, samples, and variables.
#' 
NULL
htmlReport$methods(get_data_for_plot = function(options) {
		all_data <- get_data(options)
		all_data <- c(all_data, list(samples = colnames(all_data$data_frame),
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
#' @returns A list containing the retrieved data and additional information.
#'
NULL
htmlReport$methods(get_data = function(options) {
	data_frame <- hash_vars[[options$id]]
	if(is.null(options$get_table_meth)) {
		if(!is.null(options$header)){
			colnames(data_frame) <- paste0("var", seq(ncol(data_frame)))
		}
		if(!is.null(options$row_names)) {
			rownames(data_frame) <- paste0("sample", seq(nrow(data_frame)))
		}
	}
	all_data <- extract_data(options)
	all_data$data_frame <- add_header_row_names(all_data$data_frame,
												options)
	if(!is.null(all_data$smp_attr)) {
		smp_attr <- add_header_row_names(all_data$smp_attr,
										options = list(header = options$header))
		all_data$smp_attr <- vector(mode = "list", length = ncol(smp_attr))
		for(i in seq(ncol(smp_attr))) {
			all_data$smp_attr[[i]] <- c(colnames(smp_attr)[i], smp_attr[, i])
		}
	}
	if(!is.null(all_data$var_attr)) {
		var_attr <- add_header_row_names(all_data$var_attr,
								  options = list(row_names = options$row_names))
		all_data$var_attr <- vector(mode = "list", length = nrow(var_attr))
		for(i in seq(nrow(var_attr))) {
			all_data$var_attr[[i]] <- c(rownames(var_attr)[i],
										paste(var_attr[i, ]))
		}
	}
	if(options$transpose) {
		all_data$data_frame <- as.data.frame(t(all_data$data_frame))
		backup <- all_data$smp_attr
		all_data$smp_attr <- all_data$var_attr
		all_data$var_attr <- backup
	}
	if(options$text == "dynamic") {
		numeric_fields <- check_numeric_fields(all_data$data_frame)
	} else if(options$text == FALSE){
		numeric_fields <- seq(ncol(all_data$data_frame))
	} else {
		numeric_fields <- c()
	}
	all_data$data_frame[, numeric_fields] <- as.data.frame(lapply(
				all_data$data_frame[, numeric_fields, drop = FALSE], as.numeric))
	if(!is.null(options$func)) 
		all_data$data_frame <- options$func(all_data$data_frame)
	return(all_data)				
})

#' Merge tables contained in hash_vars
#'
#' @name merge_tables
#' @title Merge tables from multiple hash_vars ids
#' @description Non-exported method that will be called by extract_data if
#' it is provided with multiple ids.
#'
#' @param options A list containing options for data retrieval.
#'
#' @returns A merged data frame, minus specified fields and rows.

htmlReport$methods(merge_tables = function(data_frame, options) {
	fields <- options$fields
	rows <- options$rows
	ids <- strsplit(options$id, split = ",")[[1]]
	data_files <- hash_vars[[ids[n]]]
	if(!is.null(fields)) {
		data_fields <- strsplit(fields, ";")[[1]]
		for(n in seq(data_fields)[[1]]) {
			data_files[[n]] <- data_files[[n]][, data_fields[n], drop = FALSE]
		}
	}
	if(!is.null(rows)) {
		data_rows <- strsplit(rows, ";")[[1]]
		for(n in seq(data_rows)[[1]]) {
			data_files[[n]] <- data_files[[n]][, data_rows[n], drop = FALSE]
		}
	}
	data <- do.call(rbind, data_files)
	return(data)
})


#' Retrieve Data from htmlReport Object
#'
#' @name extract_data
#' @title Retrieve Data from htmlReport Object
#' @description This method retrieves data from an \code{htmlReport} object based on specified options.
#' 
#' @param options A list containing options for data retrieval.
#' 
#' @returns A list containing the retrieved data and additional information.
#'
NULL
htmlReport$methods(extract_data = function(options) {	
	smp_attr <- NULL
    var_attr <- NULL
    ids <- options$id
    if("," %in% ids) ids <- strsplit(ids, split = ",")[[1]]
    fields <- options$fields
    get_table_meth <- options$get_table_meth
    if(length(ids) > 1){
    	data_frame <- merge_tables(options) ## UNTESTED
    } else {
    	data_frame <- hash_vars[[ids]]
    	if(!is.null(get_table_meth)) {
    		data_frame <- get_table_meth(data_frame)
    	}
    }
    if(length(options$var_attr) > 0){
    	var_attr <- data_frame[options$var_attr, , drop = FALSE]
    } 
    if(length(options$smp_attr) > 0){
    	smp_attr <- data_frame[, options$smp_attr, drop = FALSE]
    }
    if(length(options$smp_attr) > 0 && length(options$var_attr) > 0){
    	var_attr <- var_attr[, -options$smp_attr, drop = FALSE]
    	smp_attr <- smp_attr[-options$var_attr, , drop = FALSE]
    }
    if(length(options$fields) > 0) {
    	# Despite this being intended to work on columns, it works on rows.
    	# Remember that data frames are transposed when passed to plotting
    	# functions.
	    data_frame <- data_frame[, options$fields, drop = FALSE]
	    if(length(options$var_attr) > 0) {
	    	cols <- colnames(var_attr) %in% colnames(data_frame)
	    	var_attr <- var_attr[, cols, drop = FALSE]
	    }
	}
	if(length(options$rows) > 0) {
    	# Despite this being intended to work on columns, it works on rows.
    	# Remember that data frames are transposed when passed to plotting
    	# functions.
	    data_frame <- data_frame[options$rows, , drop = FALSE]
	    if(length(options$smp_attr) > 0) {
	    	rows <- rownames(smp_attr) %in% rownames(data_frame)
	    	smp_attr <- smp_attr[rows, , drop = FALSE]
	    }
	}
	if(length(options$var_attr) > 0){
		rows <- rownames(data_frame) %in% rownames(var_attr)
	    data_frame <- data_frame[!rows, , drop = FALSE]
	}
	if(length(options$smp_attr) > 0){
		cols <- colnames(data_frame) %in% colnames(smp_attr)
	    data_frame <- data_frame[, !cols, drop = FALSE]
	}
	return(list(data_frame = data_frame, smp_attr = smp_attr,
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
#' @returns The modified data frame with updated column and/or row names.
#'
NULL
htmlReport$methods(add_header_row_names = function(data_frame, options) {
	if(!is.null(options$header)) {
		if(options$header) {
			data_frame <- .row_to_header(data_frame)
		}
	}
	if(!is.null(options$row_names)) {
		if(options$row_names) {
			data_frame <- .col_to_rownames(data_frame)
		}
	}	
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
#' @returns An object of class "htmlReport" with an updated @all_report which includes at the end the "value" string.
#'
NULL
htmlReport$methods(concat = function(text_vec) {
	all_report <<- paste(c(all_report, text_vec), collapse = "")
})



########################################################
## JAVASCRIPT AND CSS METHODS
########################################################

htmlReport$methods(get_js_cdn= function() {
	parsed_js_cdn <- sapply(js_cdn, function(jc) {
		if (grepl("^http", jc)) {
				return(paste0("<script type=\"text/javascript\" src=\"",jc,"\"></script>"))
		}
		return(jc)
	})
	parsed_js_cdn <- c(parsed_js_cdn, "\n")
	paste(parsed_js_cdn, collapse = "\n")
})

htmlReport$methods(get_css_cdn= function() {
	parsed_css_cdn <- sapply(css_cdn, function(cc) {
		if (grepl("^http", cc)) {
				return(paste0("<link rel=\"stylesheet\" type=\"text/css\" href=\"",cc,"\"/>"))
		}
		return(cc)
	})
	parsed_css_cdn <- c(parsed_css_cdn, "\n")
	paste(parsed_css_cdn, collapse = "\n")
})


htmlReport$methods(mermaid_chart = function(chart_sintaxis){
	features$mermaid <<- TRUE
	paste0("<pre class=\"mermaid\">\n", chart_sintaxis, "\n</pre>")
})



htmlReport$methods(load_css = function(){
	for (css_file_name in css_files){
		if (!file.exists(css_file_name))
			css_file_name <- file.path(source_folder, "js", css_file_name)

		css_file <- paste(readLines(css_file_name, warn = FALSE), collapse="\n")
		concat(c("<style type=\"text/css\">\n",css_file, "\n</style>\n\n"))
	}	
})


htmlReport$methods(load_js = function(){
	for (js_file_name in js_files){
		if (!file.exists(js_file_name))
			js_file_name <- file.path(source_folder, "js", js_file_name)

		js_file <- embed_file(js_file_name)

		concat(c("<script src=\"",js_file, "\" type=\"application/javascript\"></script>\n\n"))
	}	
})

########  HTML TABLES ############################

#' Table method for htmlReport class
#'
#' @name table
#' @title Generate table from plotter object with specified options
#' @description This method takes a list of options to build a table from a
#' htmlReport object.
#' 
#' @param options list with options
#' 
#' @returns A string encoding a html table.
#'

htmlReport$methods(table = function(user_options){
	options <- list(id = NULL, header = FALSE, row_names = FALSE,
					transpose = FALSE, smp_attr = NULL, var_attr = NULL,
					fields = NULL, rows = NULL, func = NULL, text = FALSE,
					border = 1, table_rownames = TRUE, cell_align = c(),
					attrib = list(), styled = "bs", rownames_col = "rownames",
				 	buttons_custom = c('copyHtml5', 'excelHtml5', 'csvHtml5'))
	options <- update_options(options, user_options)
	table_attr <- parse_table_attr(options$attrib)
	## var_attr and smp_attr remove data from the data frame, it is not
	## represented anywhere else. This behaviour needs to be documented
	data_frame <- get_data(options)$data_frame
	## col and rowspan
	table_id <- paste0("table_", count_objects)
	if (options$styled == "dt"){
		if ('pdfHtml5' %in% options$buttons_custom) {
			features['pdfHtml5'] <<- TRUE
		}
		embedded_buttons <- paste(collapse = ",", sapply(options$buttons_custom,
												function(x) paste0("'", x,"'")))
    	features$dt_tables <<- TRUE
    	dynamic_js <<- c(dynamic_js,
                    paste(c("$(document).ready(function () {",
                        paste0("\t$(", table_id,").DataTable({ dom:'Bfrtip', ",
                        	"buttons: [", embedded_buttons, "], order: [] });"),
                    "});"), collapse = "\n"))    
	}
	count_objects <<- count_objects + 1
	parse_data_frame(data_frame = data_frame, options = options,
					 table_id = table_id, table_attr = table_attr)

	}
)

htmlReport$methods(
	parse_table_attr = function(attrib){
		table_attr <- sapply(names(attrib), function(attr_id) {
 		   paste0(attr_id, "= \"", attrib[[attr_id]], "\"")
 	 	})
 	 	return(paste(table_attr, collapse = " "))

	})

#' Parse data frame to HTML
#'
#' @name parse_data_frame-htmlReport-method
#' @title Print data frame in HTML format
#' @description Parses a data frame included in an object of class "htmlReport"
#' and HTML table to include it in htmlreportR
#' @param data_frame Data frame to parse
#' @param options list with options
#' @param table_id An integer. Table id in report
#' @param row_names A boolean.
#'   * `TRUE` (the default): Parse data frame row names as a column of
#'   												 the HTML table.
#'   * `FALSE` (the default): Do not parse data frame row names.
#' @returns A table in html format.
NULL
htmlReport$methods(
	parse_data_frame = function(data_frame, options, table_id, table_attr = ""){
		html_data_frame <- paste0("<table id=", table_id,
								   " border=", options$border, " ",
								   table_attr, " >")
		html_data_frame <- c(html_data_frame, "<thead>", "<tr>")
		if(isTRUE(options$table_rownames)) {
			rownames_col <- paste_tag(options$rownames_col, tag = "th")
			html_data_frame <- c(html_data_frame, rownames_col)
		}
		colnames_vector <- sapply(colnames(data_frame), paste_tag, tag = "th")
		names(colnames_vector) <- NULL
		html_data_frame <- c(html_data_frame, colnames_vector,
										"</tr>", "</thead>", "<tbody>")
		for(row_ind in seq(nrow(data_frame))) {
			html_data_frame <- c(html_data_frame, "<tr>")
			if(isTRUE(options$table_rownames)) {
				rownames_vector <- paste_tag(rownames(data_frame)[row_ind], "td")
				html_data_frame <- c(html_data_frame, rownames_vector)
			}
			col_vector <- sapply(data_frame[row_ind, ], paste_tag, "td")
			html_data_frame <- c(html_data_frame, col_vector, "</tr>")
		}
		html_data_frame <- c(html_data_frame, "</tbody>", "</table>")
		return(paste(html_data_frame, collapse="\n"))
})

htmlReport$methods(
	canvasXpress_main = function(user_options){
	options <- list(id = NULL, func = NULL, config_chart = NULL,
		            fields= c(), smp_attr= c(), var_attr= c(), segregate= c(),
		            show_factors= c(), data_format= "one_axis",
		            responsive= TRUE, height= "600px", width= "600px",
		            header= FALSE, row_names= FALSE, add_header_row_names= TRUE,
		            transpose= TRUE, x_label= "x_axis", title= "Title",
		            config= list(), after_render= c(), treeBy= "v",
		            renamed_samples= c(), renamed_variables= c(), alpha= 1,
		            theme= "cx", color_scheme= "CanvasXpress", tree = NULL)

	options <- update_options(options, user_options)
	config <- list('toolbarType' = 'under',
		           'xAxisTitle' = options$x_label,
		           'title' = options$title,
		           "objectColorTransparency"= options$alpha,
		           "theme"= options$theme,
		           "colorScheme"= options$color_scheme)


## esto va dentro de la clase nueva
	if (!is.null(options$tree)) {
		config <- set_tree(options, config)
	}

	config <- update_options(config, options$config)

	plot_data <- get_data_for_plot(options)
	values <- plot_data$data_frame
	smp_attr <- plot_data$smp_attr
	var_attr <- plot_data$var_attr
	samples <- plot_data$samples
	variables <- plot_data$variables 

    if (is.null(values))
     	return(paste0("<div width=\"",options$width, "\" height=\"",
     		   options$height, "\" > <p>NO DATA<p></div>"))

	object_id <- paste0("obj_", count_objects, "_")
    count_objects <<- count_objects + 1
    
	canvasXpress <- canvasXpress_obj$new(obj_id = object_id, smp = samples, 
										 vars = variables, vals = values, 
										 smp_att = smp_attr, var_att = var_attr, 
										 opt = options, conf = config)
	canvasXpress$run_config_chart(config_chart = options$config_chart,
								  options = options)
	canvasXpress$inject_attributes(options, slot="x")
	canvasXpress$inject_attributes(options, slot="z")
	features[['canvasXpress']] <<- TRUE
	plot_data <- get_plot_data(object_id, canvasXpress)
	   
	dynamic_js <<- c(dynamic_js, 
					paste0("$(document).ready(function () {\n",
						plot_data,
						 "});\n"))
    responsive <- ''
    if (options$responsive) responsive <- "responsive='true'" 

    html <- paste0("<canvas  id=\"", object_id, "\" width=\"", options$width, "\" height=\"", options$height, "\" aspectRatio='1:1' ", responsive, "></canvas>")
    return(html)
        
})




#' get_plot_data
#'
#' @name get_plot_data
#' @title Get js Plot from canvasxpress_obj Object
#' 
#' @param object_id string indicating object id
#' @param cvXpress vanvasXpress_obj object
#' 
#' @returns Displays the js code for plot.
#'
#' 
#' @importFrom jsonlite toJSON
NULL
htmlReport$methods(
	get_plot_data = function(object_id, cvXpress){
		afterRender = cvXpress$afterRender
		if(is.null(afterRender)) afterRender = list()
		paste0("var data = ",   decompress_code(compress_data(cvXpress$data_structure)), ";\n",
		   "var conf = ",   jsonlite::toJSON(cvXpress$config, auto_unbox = TRUE), ";\n",
		   "var events = ", jsonlite::toJSON(cvXpress$events, auto_unbox = TRUE), ";\n",
           "var info = ",   jsonlite::toJSON(cvXpress$info, auto_unbox = TRUE), ";\n",
           "var afterRender = ", jsonlite::toJSON(afterRender, auto_unbox = TRUE), ";\n",
           "var C", object_id, " = new CanvasXpress(\"", object_id, "\", data, conf, events, info, afterRender);\n", cvXpress$extracode)
	}
)

#' compress_data
#'
#' @name compress_data
#' @title encode and compress data json
#' 
#' @param data string indicating object id
#' 
#' @returns encoded and compressed json
#'
#' 
#' @importFrom jsonlite as_gzjson_b64 toJSON
NULL
htmlReport$methods(
	compress_data = function(data){
		if (compress) {
			json <- jsonlite::toJSON(data, auto_unbox = FALSE,
									 dataframe = "values")
			buf <- memCompress(json, "gzip")
			b64 <- xfun::base64_encode(buf)
			# b64 encoding has a character limit. Binary strings larger than
			# that are subdivided with \n. Removing them later is safe,
			# makes the html code more readable. Thus, we remove them here.
		    compressed_data <-  gsub("\n", "", b64)
		} else {
			compressed_data <- jsonlite::toJSON(data, auto_unbox = FALSE,
												dataframe = "values",
												rownames = FALSE)
		}
		return(compressed_data)
	}
)


htmlReport$methods(
	decompress_code = function(data){
		string <- data
		if (compress) {
		   features[["pako"]] <<- TRUE
		   string <- paste0("JSON.parse(pako.inflate(atob(\"", data, "\"), { to: 'string' }))")
		}
		return(string)
	}
)

htmlReport$methods(
	set_tree = function(options, config){
		tree <- options$tree
		if(file.exists(options$tree))
        	tree <- tree_from_file(options$tree)
        
        if (options$treeBy == 's'){
            config[['smpDendrogramNewick']] <- tree
            config[['smpDendrogramUseHeight']] <- TRUE
            config[['smpDendrogramHang']] <- FALSE
        } else if (options$treeBy == 'v'){
            config[['varDendrogramNewick']] <- tree
            config[['varDendrogramUseHeight']] <- TRUE
            config[['varDendrogramHang']] <- FALSE

        }
        config
})

htmlReport$methods(
	tree_from_file = function(file){
        paste(readLines(file, warn = FALSE), collapse ="")
})

htmlReport$methods(
	heatmap = function(opt) {
	config_chart <- function(cvX, options){
        cvX$config[['graphType']] <- 'Heatmap'
        if(!is.null(cvX$options$extra_data)) {
        	extra_opts <- list(id = NULL, func = NULL, fields = NULL,
        					   smp_attr = NULL, var_attr = NULL, header = NULL,
        					   row_names = NULL, show_factors = NULL,
        					   segregate = NULL, transpose = TRUE)
        	extra_opts <- update_options(extra_opts, cvX$options$extra_data)
            values2 <- get_data_for_plot(extra_opts)$data_frame
            cvX$data_structure$y$data2 <- values2
            cvX$config$guidesShow <- TRUE
            cvX$config$heatmapIndicatorPosition <- "top"
            cvX$config$sizeBy <- "Size"
            cvX$config$sizeByData <- "data2"
        }
    }
    default_options <- list('row_names' = TRUE, 'config_chart' = config_chart)
    default_options <- update_options(default_options, opt)
    html_string <- canvasXpress_main(default_options)
    return(html_string)
})

htmlReport$methods(
	boxplot = function(opt) {
		config_chart <- function(cvX, options){
			cvX$config[['graphType']] <- 'Boxplot'
			if(isTRUE(options$showBoxplotOriginalData)) {
				c(cvX$config[['showBoxplotOriginalData']] <- TRUE)
			}
			series <- NULL
			group <- NULL
			segregate <- NULL
			wide <- isTRUE(options$format == "wide")
			long_null_group <- options$format == "long" & is.null(options$group)
			if(wide | long_null_group) {
				reshaped <- cvX$reshape(samples = cvX$samples(), x = cvX$x(),
									    variables = cvX$variables(),
									    values = cvX$values())
				cvX$samples(reshaped$samples)
				cvX$x(reshaped$x)
				cvX$variables(reshaped$variables)
				cvX$values(reshaped$values)
				series <- "Factor"
			}
			if(wide) {
				if(length(options$group) > 0) {
					group <- options$group[1]
				}
				if(length(options$group) > 1) {
					segregate <- options$group[2]
				}
			} else {
				if(length(options$group) > 0) {
					series <- options$group[1]
				}
				if(length(options$group) > 1) {
					group <- options$group[2]
				}
				if(length(options$group) > 2) {
					segregate <- options$group[3]
				}
			}
			if(is.null(cvX$config[['colorBy']])) {
				cvX$config[['colorBy']] <- series
			}
			if(is.null(cvX$config[['groupingFactors']])) {
				cvX$config[['groupingFactors']] <- c(series, group)
			}
			if(!is.null(group) & is.null(cvX$config[['segregateSamplesBy']])) {
				if(!is.null(segregate)) {
					cvX$config[['segregateSamplesBy']] <- segregate
				} else {
					cvX$config[['segregateSamplesBy']] <- group
				}
			}
			if(is.null(options$extracode) & is.null(opt$group)) {
				options[['extracode']] <- paste0("C", cvX$object_id,
										".groupSamples([\"Factor\"]);")
			}
			if(isTRUE(options$add_violin)) {
				cvX$config[['showBoxplotIfViolin']] <- TRUE
				cvX$config[['showBoxplotOriginalData']] <- TRUE
				cvX$config[['showViolinBoxplot']] <- TRUE
				cvX$config[['jitter']] <- TRUE
			}
		}
		default_options <- list('row_names' = TRUE, 'header' = TRUE, 'config_chart' = config_chart)
		default_options <- update_options(default_options, opt)
		html_string <- canvasXpress_main(default_options)
		return(html_string)
})

#' CanvasXpress density plot
#'
#' @name density-htmlReport-method
#' @title Build CanvasXpress density plot from R data frame
#' @description Loads data frame and CanvasXpress options for density plot,
#' then calls canvasXpress_main to build it.
#' @param options list with options.
#' @returns HTML code for CanvasXpress density plot of data.

NULL
htmlReport$methods(
	density = function(opt) {
	config_chart <- function(cvX, options){
        cvX$config[['graphType']] <- "Scatter2D"
        cvX$config[['hideHistogram']] <- TRUE
        cvX$config[['showHistogram']] <- ifelse(is.null(options$group),
        									      TRUE, options$group)
        cvX$config[['showFilledHistogramDensity']] <- options$fillDensity
        cvX$config[['showHistogramDensity']] <- TRUE
        cvX$config[['showHistogramMedian']] <- options$median
	}

    default_options <- list('transpose' = FALSE, 'fillDensity' = FALSE,
    					   'median' = FALSE, 'config_chart' = config_chart)
    default_options <- update_options(default_options, opt)
    html_string <- canvasXpress_main(default_options)
    return(html_string)
})

#' CanvasXpress barplot
#'
#' @name barplot-htmlReport-method
#' @title Build CanvasXpress barplot from R data frame
#' @description Loads data frame and CanvasXpress options for barplot,
#' then calls canvasXpress_main to build it.
#' @param options list with options.
#' @returns HTML code for CanvasXpress bar plot of data.

NULL
htmlReport$methods(
	barplot = function(opt) {
	config_chart <- function(cvX, options) {
		cvX$config[['graphType']] <- 'Bar'
		xmod <- cvX$x()
		if(isTRUE(options$colorScale)) {
			xmod[[options$x_label]] <- unlist(cvX$values()[1,])
			#el unlist es para que el subset de 1 fila devuelva un vector
			cvX$config[['colorBy']] <- options$x_label
		}
		cvX$x(xmod)
	}

	default_options <- list('row_names' = TRUE,
							'config_chart' = config_chart)
	default_options <- update_options(default_options, opt)
	html_string <- canvasXpress_main(default_options)
	return(html_string)
})

#' CanvasXpress scatter2D plot
#'
#' @name scatter2D-htmlReport-method
#' @title Build CanvasXpress scatter2D plot from R data frame
#' @description Loads data frame and CanvasXpress options for scatter2D plot,
#' then calls canvasXpress_main to build it.
#' @param options list with options.
#' @returns HTML code for CanvasXpress scatter2D plot of data.

NULL
htmlReport$methods(
	scatter2D = function(opt) {
	config_chart <- function(cvX, options){
	 #   samples, variables, values, x, z = cvX$get_data_structure_vars()
     cvX$config[['graphType']] <- "Scatter2D"
	 cvX$config[['xAxis']] <- ifelse(is.null(options$xAxis), cvX$samples()[1],
	 														 options$xAxis)
	 if(is.null(options$yAxis)) {
	 	smps <- cvX$samples()
	 	cvX$config[['yAxis']] <- smps[smps != cvX$config$xAxis]
	 }else{
	 	cvX$config[['yAxis']] <- options$yAxis
	 }
	 cvX$config[['yAxisTitle']] <- ifelse(is.null(options$y_label), "y_axis",
	 														 options$y_label)
	 if(!is.null(options$regressionLine)) {
	 	cvX$config[["showRegressionFit"]] = TRUE
        cvX$config[["showRegressionFullRange"]] = TRUE
	 }
 	zmod <- cvX$z()
	 if(!is.null(options$pointSize)) {
	 	cvX$config[['sizeBy']] <- options$pointSize
	 	sampleIndex <- grep(options$pointSize, cvX$samples())
	 	zmod[[options$pointSize]] <- cvX$values()[, sampleIndex]
	 }
	 if(!is.null(options$colorScaleBy)) {
	 	cvX$config[['colorBy']] <- options$colorScaleBy
	 	sampleIndex <- grep(options$colorScaleBy, cvX$samples())
	 	zmod[[options$colorScaleBy]] <- cvX$values()[, sampleIndex]
	 }
	 cvX$z(zmod)
	 if(!is.null(options$add_densities)) {
	 	cvX$config[['hideHistogram']] <- FALSE
	 	cvX$config[['histogramBins']] <- 20
	 	cvX$config[['histogramStat']] <- "count"
	 	cvX$config[['showFilledHistogramDensity']] <- TRUE
	 	cvX$config[['showHistogramDensity']] <- TRUE
	 	cvX$config[['showHistogramMedian']] <- TRUE
	 	cvX$config[['xAxisHistogramHeight']] <- 150
	 	cvX$config[['xAxisHistogramShow']] <- TRUE
	 	cvX$config[['yAxisHistogramHeight']] <- 150
	 	cvX$config[['yAxisHistogramShow']] <- TRUE
	 }
	}
    default_options = list('row_names' = FALSE, 'transpose' = FALSE,
    					   'config_chart' = config_chart)
    default_options <- update_options(default_options, opt)
    html_string <- canvasXpress_main(default_options)
    return(html_string)
})

#' CanvasXpress scatter3D plot
#'
#' @name scatter3D-htmlReport-method
#' @title Build CanvasXpress scatter3D plot from R data frame
#' @description Loads data frame and CanvasXpress options for scatter3D plot,
#' then calls canvasXpress_main to build it.
#' @param options list with options.
#' @returns HTML code for CanvasXpress scatter3D plot of data.

htmlReport$methods(
	scatter3D = function(opt) {
		config_chart <- function(cvX, options) {
			cvX$config$graphType <- "Scatter3D"
			cvX$config$xAxis <- ifelse(is.null(options$xAxis),
											cvX$samples()[1], options$xAxis)
			cvX$config$yAxis <- ifelse(is.null(options$yAxis),
											cvX$samples()[2], options$yAxis)
			cvX$config$zAxis <- ifelse(is.null(options$zAxis),
											cvX$samples()[3], options$zAxis)
			cvX$config$yAxisTitle <- ifelse(is.null(options$yAxis),
											"y_axis", options$y_label)
			cvX$config$zAxisTitle <- ifelse(is.null(options$zAxis),
											"z_axis", options$z_label)
			## RegressionLine option from python removed, as #jesus is not even
			## sure of what it does.
			## pointSize option has been reduced from python method, since it
			## can be handled as a sample attribute (as explained by #jesus).
			## Same for colorScaleBy.
			if(!is.null(options$pointSize)) {
				cvX$config$sizeBy <- options$pointSize
			}
			if(!is.null(options$colorScaleBy)) {
				cvX$config$colorBy <- options$colorScaleBy
			}
			if(!is.null(options$shapeBy)) {
				cvX$config$shapeBy <- options$shapeBy
			}
		}
		default_options <- list(row_names = FALSE, transpose = FALSE,
								config_chart = config_chart)
		default_options <- update_options(default_options, opt)
		html_string <- canvasXpress_main(default_options)
		return(html_string)
	})

#' CanvasXpress line plot
#'
#' @name line-htmlReport-method
#' @title Build CanvasXpress line plot from R data frame
#' @description Loads data frame and CanvasXpress options for line plot,
#' then calls canvasXpress_main to build it.
#' @param options list with options.
#' @returns HTML code for CanvasXpress line plot of data.

NULL
htmlReport$methods(
	line = function(opt) {
	config_chart <- function(cvX, options) {
		cvX$config[['graphType']] <- 'Line'
	}
	default_options <- list('row_names' = TRUE,
							'config_chart' = config_chart)
	default_options <- update_options(default_options, opt)
	html_string <- canvasXpress_main(default_options)
	return(html_string)
})

htmlReport$methods(
	make_resizable = function(img){ 

		paste0("<div class=\"resizable_img\">",
			img,
			"</div>")

})

#' pretiffy_div
#'
#' @name prettify_div-htmlReport-method
#' @title Encapsulates input code in div of specified characteristics
#' 
#' @param data string indicating object id
#' 
#' @returns encoded and compressed json
#'
#'
NULL
htmlReport$methods(
	prettify_div = function(code, overflow = NULL, display = NULL,
							direction = NULL, justify = NULL, preset = ""){
		if(!is.null(overflow)) {
			overflow <- paste0("overflow: ", overflow)
		}
		if(!is.null(direction)) {
			if(is.null(display)) {
				warning("Specified direction with incompatible display argument.
						Setting to \"flex\"")
				display <- "flex"
			} else {
				direction <- paste0(display, "-direction: ", direction)
			}
		}
		if(!is.null(display)) {
			display <- paste0("display: ", display)
		}
		if(!is.null(justify)) {
			justify <- paste0("justify-content: ", justify)
		}
		div_params <- list(overflow = overflow, display = display,
						   direction = direction, justify = justify)
		if(preset == "magic") {
			div_params$overflow <- "overflow: hidden"
			div_params$display <- "display: flex"
			div_params$direction <- "flex-direction: row"
			div_params$justify <- "justify-content: center"
		}
		div_params <- div_params[lengths(div_params) > 0]
		if(any(unlist(lapply(div_params, function(x) !is.null(x))))) {
			params <- paste(div_params, collapse = "; ")
			div <- paste0("<div style =\"", params, "\">")
		} else {
			div <- "<div>"
		}
		return(paste(div, code, "</div>", sep = "\n"))
	}
)

#' merge_hashed_tables
#'
#' @name merge_hashed_tables-htmlReport-method
#' @title binds tables contained in hash_vars by rows
#' 
#' @param ids Vector of hash_vars IDs to bind.
#' @param join_method Method by which tables will be joined, default "rbind".
#' also admits "cbind", in which case from_id_name and alt_ids arguments will
#' be ignored.
#' @param add_colnames A boolean. Controls colnames handling.
#'   * `TRUE`: tables' colnames are treated as such.
#'   * `FALSE` (the default): tables' first row is moved to colnames.
#' @param from_id_name Name of column that contains original hash_vars IDs,
#' to trace their origin. If NULL, this column will not be added.
#' @param alt_ids New names for original IDs column, in case renaming them
#' is needed for clarity, for example. If NULL (the default) they will not be
#' renamed.
#' @param target_id Hash_vars ID where output will be saved. If NULL (the
#' default) it will be returned and NOT saved.
#' 
#' @returns Merged table
#'

NULL

htmlReport$methods(
	merge_hashed_tables = function(ids, join_method = "rbind", alt_ids = NULL,
								   add_colnames = FALSE, from_id_name = NULL, 
								   target_id = NULL){
		bound_table <- NULL
		if(join_method == "rbind") {
			if(!is.null(from_id_name)) {
				tables <- lapply(ids, .add_id_column, hash_vars = hash_vars,
					 			 from_id_name = from_id_name,
					 			 add_colnames = add_colnames)
			} else {
				tables <- hash_vars[ids]
				if(isTRUE(add_colnames)) {
					tables <- lapply(tables, .row_to_header)
				}
			}
			bound_table <- do.call(rbind, tables)
			if(!is.null(alt_ids)) {
				bound_table[, from_id_name] <- alt_ids
			}
			rownames(bound_table) <- NULL
		}
		if(join_method == "cbind") {
			tables <- hash_vars[ids]
			bound_table <- do.call(cbind, tables)
		}
		if(is.null(target_id)) {
			return(bound_table)
		} else {
			hash_vars[[target_id]] <<- bound_table
		}	
	}
)

#' get_col_n_row_span
#'
#' @name get_col_n_row_span-htmlReport-method
#' @title converts span noted table into a span map.
#' 
#' @param table Colspan and rowspan maps.
#' @details What we mean by "span map" is a table with the same dimensions
#' as the input table, but with a number in each cell specifyin its column
#' and row spans, respectively. A colspan of three means that it will merge with
#' the two cells on its right into the same cell in the final table. Same logic
#' for row span map, it will merge with the two cells below it.
#' 
#' @returns A list of two tables: colspans and rowspans.
#'
NULL

htmlReport$methods(
	get_col_n_row_span = function(table){
		rowspan <- colspan <- data.frame(matrix(1, ncol = ncol(table),
										 nrow = nrow(table)))
        last_row <- 1
        for(r in seq(nrow(table))) {
        	last_col <- 1
        	for(c in seq(length(table[r, ]))) {
        		if(table[r, c] == "colspan") {
        			colspan[r, last_col] <- colspan[r, last_col] + 1
        		} else {
        			last_col <- c
        		}
        		if(table[r, c] == "rowspan") {
        			rowspan[last_row, c] <- rowspan[last_row, c] + 1
        		} else {
        			last_row <- r
        		}
        	}
        }
        return(list(rowspan = rowspan, colspan = colspan))
	}
)
