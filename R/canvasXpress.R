


canvasXpress_obj$methods(run_config_chart = function(config_chart, options){
					config_chart(.self, options)
})



canvasXpress_obj$methods(
	inject_attributes = function(options, slot){
      	attributes <- list()
        attributes_options <- list("x" = "inject_var_attr",
                                   "z" = "inject_smp_attr")
        chosen_option <- options[[attributes_options[[slot]]]]
        if (!is.null(chosen_option)){
            data_structure[[slot]] <<- update_options(data_structure[[slot]],
                                                      chosen_option)
        }
})

canvasXpress_obj$methods(
    add_canvas_attributes = function(attr2add){
        hash_attr <- vector(mode = "list", length = length(attr2add))
        for(i in seq(length(attr2add))) {
            attrs <- attr2add[[i]]
            attr_name <- attrs[1]
            hash_attr[[i]] <- attrs[-1]
            names(hash_attr)[i] <- attr_name
        }
        return(hash_attr)
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

canvasXpress_obj$methods(
    reshape = function(samples, variables, x, values) {
        new_samples <- vector(mode = "list", length = length(variables))
        new_samples[[1]] <- samples
        if(length(variables) > 1) {
            for(i in seq(length(variables) - 1)) {
                new_samples[[i + 1]] <- paste0(samples, "_", i - 1)
            }
        }
        new_samples <- unlist(new_samples)
        if(length(x) > 0) {
            new_x <- vector(mode = "list", length = length(x))
            names(new_x) <- names(x)
            for(factor in seq(length(x))) {
                new_x[[factor]] <- rep(x[[factor]], length(variables))
            } 
        } else {
            new_x <- list()
        }
        series_annot <- rep(variables, each = length(samples))
        new_x[['Factor']] <- series_annot
        new_values <- list(c(t(values)))
        names(new_values[[1]]) <- NULL
        res <- list(samples = new_samples, variables = "vals", x = new_x,
                    values = new_values)
        return(res)
        })
