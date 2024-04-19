


canvasXpress_obj$methods(run_config_chart = function(config_chart){
					config_chart(.self)
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
        	data_names <- sapply(data_names, function(name) Fs("'", name, "'"))
        	names_string <- paste(data_names, collapse = ",")
            if (data_type == 'var'){
                add_ext_code(Fs(obj_id, ".segregateVariables([", names_string, "]);\n"))
            } else if (data_type == 'smp') {
                add_ext_code(Fs(obj_id, ".segregateSamples([", names_string, "]);\n"))
            }
        }
 })

canvasXpress_obj$methods(
	add_ext_code = function(ext_code){
		extracode <<- Fs(extracode, ext_code, "\n")
	})


