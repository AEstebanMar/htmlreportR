setClass("htmlReport", 
  slots = c(
    hash_vars = "list",
    all_report = "character", 
    title = "character",
    tmp_dir = "character"
  ),
   prototype = list(
    hash_vars = list(),
    all_report = "", 
    title = "",
    tmp_dir = ""
  )
)



#self.js_libraries = []
#        self.css_files = []
#        self.js_cdn = []
#        self.css_cdn = []
#       self.mermaid = T/F

#bootstrap & mermelada
