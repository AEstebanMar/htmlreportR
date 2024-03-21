
htmlReport <- setRefClass("htmlReport",
    field = list(   
      hash_vars = "list",
      all_report = "character", 
      title = "character",
      tmp_dir = "character",
      js_files = "character",
      css_files = "character",
      js_cdn =  "character",
      css_cdn =  "character",
      mermaid = "logical",
      bs_tables = "character",
      dt_tables = "character",
      count_objects = "numeric",
      custom_buttons = "character",
      dynamic_js = "character"
      ),

    methods = list(
      initialize = function(container = list(), title_doc = "", tmp_folder = tempdir(check = TRUE)){
          hash_vars <<- container
          title <<- title_doc
          tmp_dir <<- tmp_folder
          all_report <<- ""
          js_cdn <<- ""
          css_cdn <<- ""
          mermaid <<- FALSE
          count_objects <<- 0
          if(!file.exists(tmp_folder))
            dir.create(tmp_folder)


      }
    ) #end methods
) #end class