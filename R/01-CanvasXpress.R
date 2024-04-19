canvasXpress_obj <- setRefClass("canvasXpress",
    field = list(   
      values = "data.frame",
      smp_attr = "list",
      var_attr = "list",
      samples = "vector",
      variables = "vector",
      data_structure = "list",
      object_id = "character",
      x = "list",
      y = "list", 
      z = "list",
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
        if (!is.null(vars)) variables <<- vars
        if (!is.null(smps)) samples <<- smps
        values <<- vals
        object_id <<- obj_id 
        x <<- list()
        y <<- list()
        if (!is.null(var_att)) z <<- as.list(as.data.frame(t(var_att)))
        if (!is.null(smp_att)) x <<- as.list(smp_att)
        if (!is.null(options$after_render)) afterRender <<- options$after_render
        events <<- FALSE #Possible future use for events for CanvasXpress, currently not used
        info <<- FALSE #Possible future use for events for CanvasXpress, currently not used
        config <<- conf
        data_structure <<- list(
                            'y' = list( 
                                  'vars' = variables,
                                  'smps' = samples,
                                  'data' = values
                            ),
                            'x' = x,
                            'z' = z)
       
        initialize_extracode(options)


        if (length(options$segregate) > 0)
            add_ext_code(segregate_data(Fs("C", object_id), options$segregate))
      
        if (!is.null(options$group_samples)) 
            add_ext_code(Fs("C", object_id, ".groupSamples(", options$group_samples, ")"))
        }
    ) #end methods
) #end class



