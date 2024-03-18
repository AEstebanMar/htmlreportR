
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
      mermaid = "logical"),

    methods = list(
      initialize = function(container = list(), title_doc = "", tmp_folder = ""){
          hash_vars <<- container
          title <<- title_doc
          tmp_dir <<- tmp_folder
          all_report <<- ""
          js_files <<- ""
          css_files <<- ""
          js_cdn <<- ""
          css_cdn <<- ""
          mermaid <<- FALSE
          dir.create(tmp_folder)


      }
    ) #end methods
) #end class