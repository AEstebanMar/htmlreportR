
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
      dynamic_js = "character",
      index_items = "matrix",
      index_type = "character",
      features = "list",
      source_folder = "character",
      compress = "logical"
      ),

    methods = list(
      initialize = function(container = list(), title_doc = "", type_index = "contents_list",tmp_folder = tempdir(check = TRUE), src = find.package('htmlreportR'), compress_obj = TRUE, files_css = NULL, files_js = NULL,cdn_js = NULL, cdn_css = NULL ){
          hash_vars <<- container
          title <<- title_doc
          tmp_dir <<- tmp_folder
          all_report <<- ""
          js_cdn <<- ""
          css_cdn <<- ""
          mermaid <<- FALSE
          count_objects <<- 0
          index_type <<- type_index
          source_folder <<- src
          if (!is.null(files_js)) js_files <<- split_str(files_js, ",")
          if (!is.null(files_css)) css_files <<- split_str(files_css, ",")
          if (!is.null(cdn_js)) js_cdn <<- split_str(cdn_js, ",")
          if (!is.null(cdn_css)) css_cdn <<- split_str(cdn_css, ",")


          compress <<- compress_obj
          features <<- list('mermaid' = FALSE, 'dt_tables' = FALSE, 'pdfHtml5' = FALSE, 'canvasXpress' = FALSE, 'pako' = FALSE,
            'cytoscape'= FALSE, 'pyvis' = FALSE, 'elgrapho' = FALSE, 'sigma' = FALSE)

          if(!file.exists(tmp_folder))
            dir.create(tmp_folder)


      }
    ) #end methods
) #end class