canvasXpress_obj <- setRefClass("canvasXpress",
    field = list(   
      smp_attr = "list",
      var_attr = "list",
      data_structure = "list",
      object_id = "character",
      extracode = "character",
      config_chart = "function",
      events = "logical",
      info = "logical",
      afterRender = "vector",
      options = "list",
      config = "list"
      ),
    methods = list(
      initialize = function(obj_id = "", vars =NULL, smps = NULL, vals = data.frame(), smp_att = NULL, var_att = NULL, opt = list(), conf = list()){
        options <<- opt
        data_structure <<- list(
                    'y' = list( 
                          'vars' = c(),
                          'smps' = c(),
                          'data' = data.frame()
                    ),
                    'x' = list(),
                    'z' = list())

        if (!is.null(vars)) variables(vars)
        if (!is.null(smps)) samples(smps)
        values(vals)
        object_id <<- obj_id 
        if (!is.null(var_att)) z(as.list(var_att))
        if (!is.null(smp_att)) x(as.list(as.data.frame(t(smp_att))))
        if (!is.null(options$after_render)) afterRender <<- options$after_render
        events <<- FALSE #Possible future use for events for CanvasXpress, currently not used
        info <<- FALSE #Possible future use for events for CanvasXpress, currently not used
        config <<- conf

        initialize_extracode(options)

        if (length(options$segregate) > 0)
            add_ext_code(segregate_data(paste0("C", object_id), options$segregate))
      
        if (!is.null(options$group_samples)) 
            add_ext_code(paste0("C", object_id, ".groupSamples(", options$group_samples, ")"))
        }
    ) #end methods
) #end class



