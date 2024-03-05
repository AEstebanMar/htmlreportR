setClass("htmlReport", 
  slots = c(
    all_report = "character", 
    title = "character",
    hash_vars = "list",
    tmp_dir = "character"
  ),
   prototype = list(
    all_report = "", 
    title = "",
    hash_vars = list(),
    tmp_dir = ""
  )
)

setGeneric("build", function(htmlReport_obj, template) standardGeneric("build"))
setMethod("build", "htmlReport", function(htmlReport_obj, template) {
	templ <- paste(readLines(template), collapse="\n")
	knitr::opts_chunk$set(echo = FALSE, results="asis", message=FALSE)
	rendered_template <- knitr::knit(text = templ, quiet = TRUE)
	paste_report(htmlReport_obj) <- "<HTML>\n"
	htmlReport_obj <- make_head(htmlReport_obj)
	htmlReport_obj <- build_body(htmlReport_obj, rendered_template)
    paste_report(htmlReport_obj) <- "\n</HTML>"
    return(htmlReport_obj)
})


## value es un nombre generico para poder usar este método como paste_report(htmlReport_obj) <- c("texto")
setGeneric("paste_report<-", function(htmlReport_obj, value) standardGeneric("paste_report<-"))
setMethod("paste_report<-", "htmlReport", function(htmlReport_obj, value) {
	htmlReport_obj@all_report <- paste(c(htmlReport_obj@all_report,
					value), collapse = "")
    return(htmlReport_obj)
})

setGeneric("make_head", function(htmlReport_obj) standardGeneric("make_head"))
setMethod("make_head", "htmlReport", function(htmlReport_obj) {
	title <- htmlReport_obj@title
	paste_report(htmlReport_obj) <- c("\t<title>", title, "</title>\n<head>\n")
	paste_report(htmlReport_obj) <- "</head>\n"
    return(htmlReport_obj)
})

setGeneric("build_body", function(htmlReport_obj, body_text) standardGeneric("build_body"))
setMethod("build_body", "htmlReport", function(htmlReport_obj, body_text) {
	paste_report(htmlReport_obj) <- body_text
    return(htmlReport_obj)
})

setGeneric("write", function(htmlReport_obj, output_path) standardGeneric("write"))
setMethod("write", "htmlReport", function(htmlReport_obj, output_path) {
	writeLines(htmlReport_obj@all_report, output_path)
	unlink(htmlReport_obj@tmp_dir, recursive = TRUE)
})
