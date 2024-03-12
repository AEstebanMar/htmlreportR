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



