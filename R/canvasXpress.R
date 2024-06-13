


canvasXpress_obj$methods(run_config_chart = function(config_chart, options){
					config_chart(.self, options)
})



canvasXpress_obj$methods(
	inject_attributes = function(options, slot){
      	attributes <- list()
        attributes_options <- list("x" = "inject_smp_attr", "z" = "inject_var_attr")
        chosed_option <- options[[attributes_options[[slot]]]]
        if (!is.null(chosed_option)){
            data_structure[[slot]] <<- update_options(data_structure[[slot]], chosed_option)
        }

 })


canvasXpress_obj$methods(
	initialize_extracode = function(options){
		extracode <<- ""
		if (!is.null(options$extracode))
        	add_ext_code(extcode)
})


canvasXpress_obj$methods(
 		segregate_data = function(obj_id, segregate){
        string  <- ""
        for (data_type in names(segregate)) {
        	data_names <- segregate[[data_type]]
        	data_names <- sapply(data_names, function(name) paste0("'", name, "'"))
        	names_string <- paste(data_names, collapse = ",")
            if (data_type == 'var'){
                add_ext_code(paste0(obj_id, ".segregateVariables([", names_string, "]);\n"))
            } else if (data_type == 'smp') {
                add_ext_code(paste0(obj_id, ".segregateSamples([", names_string, "]);\n"))
            }
        }
 })

canvasXpress_obj$methods(
	add_ext_code = function(ext_code){
		extracode <<- paste0(extracode, ext_code, "\n")
	})




#################### DATA_STRUCTURE ACCESSORS
    # data_structure <<- list(
    #                         'y' = list( 
    #                               'vars' = variables,
    #                               'smps' = samples,
    #                               'data' = values
    #                         ),
    #                         'x' = x,
    #                         'z' = z)

canvasXpress_obj$methods(
    variables = function (content = NULL) {
        if (is.null(content)) {
            return(data_structure$y$vars)
        } else {
            data_structure$y$vars <<- content
        }
        invisible(return(NULL))
})

canvasXpress_obj$methods(
    samples = function (content = NULL) {
        if (is.null(content)) {
            return(data_structure$y$smps)
        } else {
            data_structure$y$smps <<- content
        }
        invisible(return(NULL))
})

canvasXpress_obj$methods(
    values = function (content = NULL) {
        if (is.null(content)) {
            return(data_structure$y$data)
        } else {
            data_structure$y$data <<- content
        } 
        invisible(return(NULL))
})
canvasXpress_obj$methods(
    x = function (content = NULL) {
        if (is.null(content)) {
            return(data_structure$x)
        } else {
            data_structure$x <<- content
        }
        invisible(return(NULL))
})

canvasXpress_obj$methods(
    z = function (content = NULL) {
        if (is.null(content)) {
            return(data_structure$z)
        } else {
            data_structure$z <<- content
        }
        invisible(return(NULL))
})