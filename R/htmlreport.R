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


`%+%` <- function(a, b) paste0(c(a, b), collapse="")

setGeneric("build", function(plotter, template) standardGeneric("build"))
setMethod("build", "htmlReport", function(plotter, template) {
	templ <- paste(readLines(template), collapse="\n")
	knitr::opts_chunk$set(echo = FALSE, results="asis", message=FALSE, error = FALSE, warning = FALSE)
	rendered_template <- knitr::knit(text = templ, quiet = TRUE)

	paste_report(plotter) <- "<HTML>\n"
	plotter <- make_head(plotter)
	plotter <- build_body(plotter, rendered_template)
  paste_report(plotter) <- "\n</HTML>"
  return(plotter)
})


## value es un nombre generico para poder usar este método como paste_report(plotter) <- c("texto")
setGeneric("paste_report<-", function(plotter, value) standardGeneric("paste_report<-"))
setMethod("paste_report<-", "htmlReport", function(plotter, value) {
	plotter@all_report <- paste(c(plotter@all_report,
					value), collapse = "")
    return(plotter)
})

setGeneric("make_head", function(plotter) standardGeneric("make_head"))
setMethod("make_head", "htmlReport", function(plotter) {
	title <- plotter@title
	paste_report(plotter) <- c("\t<title>", title, "</title>\n<head>\n")
	paste_report(plotter) <- "</head>\n"
  return(plotter)
})

setGeneric("build_body", function(plotter, body_text) standardGeneric("build_body"))
setMethod("build_body", "htmlReport", function(plotter, body_text) {
	paste_report(plotter) <- body_text
    return(plotter)
})

setGeneric("write_report", function(plotter, output_path) standardGeneric("write_report"))
setMethod("write_report", "htmlReport", function(plotter, output_path) {
	writeLines(plotter@all_report, output_path)
	unlink(plotter@tmp_dir, recursive = TRUE)
})


setGeneric("static_ggplot_main", function(plotter, id_plot, header = FALSE, plotting_function = NULL, ...) standardGeneric("static_ggplot_main"))
setMethod("static_ggplot_main", "htmlReport", function(plotter, id_plot, header = FALSE, plotting_function = NULL, ...) {
	data_frame <- plotter@hash_vars[[id_plot]]

	if (header) data_frame <- build_header(data_frame) 

	ggplot_obj <- ggplot2::ggplot(data_frame)
	if (is.null(plotting_function )) return(ggplot_obj)

	ggplot_obj <- plotting_function(ggplot_obj, ...)

get_plot(plotter, ggplot_obj)
	})

setGeneric("get_plot", function(plotter, plot_obj) standardGeneric("get_plot"))
setMethod("get_plot", "htmlReport", function(plotter, plot_obj) {
	#This code writes the plot to a temporal png, then it load the png in base64 encoding and then 
	
	file_png <- file.path(plotter@tmp_dir, "tmp_fig.png")
  message(file_png)

  png(file_png, 
		width = knitr::opts_current$get("fig.width"),
		height = knitr::opts_current$get("fig.height"),
		units = "in",
		res = 200)

	  plot(plot_obj)
	
	dev.off()
	enc_img <- embed_img(plotter, file_png)
	cat(paste0("\n<img src=", enc_img, " />"))
	
})


setGeneric("embed_img", function(plotter, img_file) standardGeneric("embed_img"))
setMethod("embed_img", "htmlReport", function(plotter, img_file) {
    enc_img <- xfun::base64_uri(img_file)
    return(enc_img)
})
