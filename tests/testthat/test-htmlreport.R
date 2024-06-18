

#add_header_row_names 

test_that("testing header and rownames addition", {
		plotter <- htmlReport$new()
		table_orig <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2", "2", "4"), nrow = 3, byrow = TRUE))

		frmt_exp <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2", "2", "4"), nrow = 3, byrow = TRUE,dimnames = list(c(1,2,3),c(1,2,3))))
		frmt_row_names_exp <- as.data.frame(matrix(c("h1","h2","1", "3", "2", "4"), nrow = 3, byrow = TRUE, dimnames = list(c("0","r1","r2"),c(1,2))))
		frmt_header_exp <- as.data.frame(matrix(c("r1","1", "3", "r2","2", "4"), nrow = 2, byrow = TRUE, dimnames = list(c(1,2),c("0","h1","h2"))))
	   	frmt_header_row_names_exp <- as.data.frame(matrix(c("1", "3", "2", "4"), nrow = 2, byrow = TRUE, dimnames = list(c("r1","r2"),c("h1","h2"))))
	   
	    user_options <- list("header" = FALSE, "row_names" = FALSE)
        user_options_with_row_names <- list("header" = FALSE, "row_names" = TRUE) 
        user_options_with_header <- list("header" = TRUE, "row_names" = FALSE)
	    user_options_with_header_row_names <- list("header" = TRUE, "row_names" = TRUE)

	    formatted_table <- plotter$add_header_row_names(table_orig, user_options)
	    formatted_table_row_names <- plotter$add_header_row_names(table_orig, user_options_with_row_names)
	    formatted_table_header <- plotter$add_header_row_names(table_orig, user_options_with_header)
	    formatted_table_header_row_names <- plotter$add_header_row_names(table_orig, user_options_with_header_row_names)

	    expect_equal(frmt_exp, formatted_table)
	    expect_equal(frmt_row_names_exp, formatted_table_row_names)
	    expect_equal(frmt_header_exp, formatted_table_header)
	    expect_equal(frmt_header_row_names_exp, formatted_table_header_row_names)
})


#extract_data

test_that("testing the sample and variable attributes formatting", {
		plotter <- htmlReport$new()

		table_orig <-data.frame( "V1" = c("h0","r1", "r2"), 
							     "V2" = c("h1", 1,2), 
							     "V3" = c("h2", 3,4), 
							     row.names = c(1,2,3))

		frmt_exp <- list(data_frame = data.frame( "V1" = c("h0","r1", "r2"), 
											     "V2" = c("h1", "1","2"), 
											     "V3" = c("h2", "3","4"), 
											     row.names = c(1,2,3)),
						 smp_attr= NULL,
						 var_attr = NULL)

		frmt_var_attr_exp <- list(data_frame = data.frame("V2" = c("h1","1","2"), 
														   "V3" = c("h2","3","4"), 
														   row.names = c(1,2,3)) ,
								  smp_attr = NULL,
								  var_attr = data.frame("V1" = c("h0","r1", "r2"),
								  						row.names = c(1,2,3))
								  						 )
		


		frmt_smp_attr_exp <- list(data_frame = data.frame("V1" = c("r1", "r2"), 
														   "V2" = c("1","2"), 
														   "V3" = c("3","4"), 
														   row.names = c(2,3)),
								  smp_attr = data.frame("V1" = "h0", 
								 					     "V2" = "h1",
								 						 "V3" = "h2",
								 						 row.names = c(1)),
								  var_attr =  NULL)
		frmt_smp_var_attr_exp <- list(data_frame = data.frame( "V2" = c(1,2), 
															   "V3" = c(3,4), 
																   row.names = c(2,3)),
									  smp_attr =  data.frame("V2" = "h1",
								 						     "V3" = "h2",
								 						      row.names = c(1)),
	  								  var_attr = data.frame("V1" = c("r1", "r2"),
								  					        row.names = c(2,3)))


	   	user_options <- list("header" = FALSE, "row_names" = FALSE, smp_attr = NULL, var_attr = NULL, text = TRUE)
	   	user_options_smp_attr <- list("header" = FALSE, "row_names" = FALSE, smp_attr = c(1), var_attr = NULL, text = TRUE)
	   	user_options_var_attr <- list("header" = FALSE, "row_names" = FALSE, smp_attr = NULL, var_attr = c(1), text = "dynamic")
	   	user_options_smp_var_attr <- list("header" = FALSE, "row_names" = FALSE, smp_attr = c(1), var_attr = c(1), text = FALSE)



	   	formatted_table <- plotter$extract_data(table_orig, user_options)
	    formatted_table_smp_attr <- plotter$extract_data(table_orig, user_options_smp_attr)
	    formatted_table_var_attr <- plotter$extract_data(table_orig, user_options_var_attr)
   	    formatted_table_smp_var_attr <- plotter$extract_data(table_orig, user_options_smp_var_attr)


	    expect_equal(formatted_table, frmt_exp)
   	    expect_equal(formatted_table_smp_attr,frmt_smp_attr_exp)
	    expect_equal(formatted_table_var_attr,frmt_var_attr_exp)	
	    expect_equal(formatted_table_smp_var_attr,frmt_smp_var_attr_exp)
})


#get_data

test_that("testing the table formatting in the class htmlReport",{

		options <- list(id = "table_orig",
						transpose = FALSE,
						header = TRUE,
						row_names = TRUE,
						smp_attr = c(1),
						var_attr = c(1), 
						text = FALSE)
		mod_function <- function(data_frame){
			row_names <- rownames(data_frame)
			mod_df <- as.data.frame(lapply(data_frame, as.character))
			rownames(mod_df) <- row_names
			mod_df
		}

		container <- list("table_orig" = data.frame( "V1" = c("h0","r1", "r2", "r3"), 
							     					 "V2" = c("h1", "-","var_attr1", "var_attr2"),
							     					 "V3" = c("h2", "smp_attr1", 1,2), 
							     					 "V4" = c("h3", "smp_attr2",3,4), 
							     					 row.names = c(1,2,3,4)))
		plotter <- htmlReport$new(container = container)



		formatted_data_exp <- list(data_frame = data.frame("h2" = c(1,2),
													   "h3" = c(3,4), 
													   row.names = c("r2", "r3")),
							   smp_attr = data.frame("h2" = c("smp_attr1"),
								    				 "h3" = c("smp_attr2"),
													 row.names = c("r1")),
							   var_attr = data.frame("h1" = c("var_attr1", "var_attr2"), 
												     row.names = c("r2", "r3")))
		
		formatted_data_exp_mod <- list(data_frame = data.frame("h2" = c("1","2"),
															   "h3" = c("3","4"), 
													   			row.names = c("r2", "r3")),
									   smp_attr = data.frame("h2" = c("smp_attr1"),
										    				 "h3" = c("smp_attr2"),
															 row.names = c("r1")),
									   var_attr = data.frame("h1" = c("var_attr1", "var_attr2"), 
												     row.names = c("r2", "r3")))
		

		formatted_data_transposed_exp <- list(data_frame = data.frame("r2" = c(1,3),
															      "r3" = c(2,4), 
													   row.names = c("h2", "h3")),
							   smp_attr = data.frame("r2" = c("var_attr1"),
								    				 "r3" = c("var_attr2"),
													 row.names = c("h1")),
							   var_attr = data.frame( "r1" = c("smp_attr1", "smp_attr2"), 
												     row.names = c("h2", "h3")))
		formatted_data <- plotter$get_data(options)
	    expect_equal(formatted_data, formatted_data_exp)
	    
	    options$func <- mod_function
	    formatted_data_mod <- plotter$get_data(options)
	    expect_equal(formatted_data_mod, formatted_data_exp_mod)
	    
	    options$func <- NULL
	    options$transpose <- TRUE
	    formatted_data_transposed <- plotter$get_data(options)
	    expect_equal(formatted_data_transposed, formatted_data_transposed_exp)
})

#get_data_for_plot


#get_data

test_that("testing the table formatting in the class htmlReport",{

		options <- list(id = "table_orig",
						transpose = FALSE,
						header = TRUE,
						row_names = TRUE,
						smp_attr = c(1),
						var_attr = c(1),
						text = FALSE)
		

		container <- list("table_orig" = data.frame( "V1" = c("h0","r1", "r2", "r3"), 
							     					 "V2" = c("h1", "-","var_attr1", "var_attr2"),
							     					 "V3" = c("h2", "smp_attr1", 1,2), 
							     					 "V4" = c("h3", "smp_attr2",3,4), 
							     					 row.names = c(1,2,3,4)))
		plotter <- htmlReport$new(container = container)


		formatted_data_exp <- list(data_frame = data.frame("h2" = c(1,2),
														   "h3" = c(3,4), 
														   row.names = c("r2", "r3")),
								   smp_attr = data.frame("h2" = c("smp_attr1"),
									    				 "h3" = c("smp_attr2"),
														 row.names = c("r1")),
								   var_attr = data.frame("h1" = c("var_attr1", "var_attr2"), 
													     row.names = c("r2", "r3")),
								   samples = c("h2", "h3"),
								   variables = c("r2", "r3"))
		
		formatted_data <- plotter$get_data_for_plot(options)
	    expect_equal(formatted_data, formatted_data_exp)
})


test_that("testing the full table formatting in the class htmlReport", {

	expected_output <-"<table id=table_0 border=1  >
<thead>
<tr>
<th> rownames </th>
<th> h2 </th>
<th> h3 </th>
</tr>
</thead>
<tbody>
<tr>
<td> r2 </td>
<td> 1 </td>
<td> 3 </td>
</tr>
<tr>
<td> r3 </td>
<td> 2 </td>
<td> 4 </td>
</tr>
</tbody>
</table>"
	container <- list(test_data_frame = data.frame( "V1" = c("h0","r1", "r2", "r3"), 
                                "V2" = c("h1", "-","smp_attr1", "smp_attr2"),
                                "V3" = c("h2", "var_attr1", 1,2), 
                                "V4" = c("h3", "var_attr2",3,4), 
                                row.names = c(1,2,3,4)))
	plotter <- htmlReport$new(container = container)
	actual_output <- plotter$table("test_data_frame", header = TRUE, row_names = TRUE, smp_attr = c(1), var_attr = c(1))

	testthat::expect_equal(actual_output, expected_output)
})

test_that("testing the table formatting in the class htmlReport", {
	expected_output <-"<table id=Sample border=1  >
<thead>
<tr>
<th> rownames </th>
<th> V1 </th>
<th> V2 </th>
<th> V3 </th>
<th> V4 </th>
</tr>
</thead>
<tbody>
<tr>
<td> 1 </td>
<td> h0 </td>
<td> h1 </td>
<td> h2 </td>
<td> h3 </td>
</tr>
<tr>
<td> 2 </td>
<td> r1 </td>
<td> - </td>
<td> var_attr1 </td>
<td> var_attr2 </td>
</tr>
<tr>
<td> 3 </td>
<td> r2 </td>
<td> smp_attr1 </td>
<td> 1 </td>
<td> 3 </td>
</tr>
<tr>
<td> 4 </td>
<td> r3 </td>
<td> smp_attr2 </td>
<td> 2 </td>
<td> 4 </td>
</tr>
</tbody>
</table>"
	test_data_frame <- data.frame( "V1" = c("h0","r1", "r2", "r3"), 
                                "V2" = c("h1", "-","smp_attr1", "smp_attr2"),
                                "V3" = c("h2", "var_attr1", 1,2), 
                                "V4" = c("h3", "var_attr2",3,4), 
                                row.names = c(1,2,3,4))
	plotter <- htmlReport$new()
	options <- list(border = 1,
		table_rownames = TRUE)
	actual_output <- plotter$parse_data_frame(test_data_frame, options, "Sample")

	testthat::expect_equal(actual_output, expected_output)
})
####### HTML REPORTING
#make_head
#build_body


test_that("testing_img_embedding", {
	img <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAMAAABHPGVmAAAAxlBMVEUEBAQFBQUHBwcICAgKCgoMDAwPDw8QEBATExMXFxcYGBgcHBwdHR0eHh4fHx8hISEmJiYpKSkvLy8zMzM8PDxAQEBDQ0NERERGRkZHR0dISEhJSUlKSkpOTk5PT09UVFRXV1dZWVlaWlpbW1tcXFyWlpaYmJiZmZmampqcnJyfn5+hoaGoqKiqqqqrq6usrKytra27u7u8vLy+vr7Ly8vm5ubt7e3u7u7v7+/09PT19fX29vb39/f4+Pj6+vr7+/v8/Pz////jxAlkAAABX0lEQVRoge3a6U6DQBQFYBTc0OJShdZWSle1LS61RStgnfd/KQEzaZwiQzreRs05P0guuelHhgnJzVRjG4gGBAgQIEBKIUdV1ZwdWBWzUohcKD9164mxBysXie/8LKeqRnSZXk9ykfnNdZYdVSRsfo/w7KkizJ0y9lj8TtSRmduoeefECGOvsbh/CBC2skmBAAEChAYJak4WnRLh+fvLBQQIECC5eVuQI4HnOO2AGGknM86kux4S3pabTxZOenVkK/YjyPtaCI90uTqTZMbpybpUX3zHtnvPxEiyYvRbuFyAAAECZCMIhiAgQIAAkSLhnBzZbdWbVzNiRE9mnKlLhPBDmu20aIQ0CD9uMtOiHtMgPNZ9MuN4qoYEOTT3tS3jS4TS0HVD0nBczT+dW2bcL34qNhxKGvpj8TeBAPmMPxBu2EI9GkkaBr4UicVvyotQR5GkIVz5YPzbfxYA+QXIB2tlntFVobcaAAAAAElFTkSuQmCC"
	attr <- "width='100' height='100'"
	exp_img <- paste0("\n<img width='100' height='100' src=", img," />")
	plotter <- htmlReport$new()

	file <- file.path(plotter$source_folder, "exData", "test.png")
	if (!file.exists(file))
		file <- file.path(plotter$source_folder,"inst", "exData", "test.png")

	res_img <- plotter$embed_img(file, attr)
	testthat::expect_equal(res_img, exp_img)
})

test_that("testing density method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" ",
							  "height=\"600px\" aspectRatio='1:1'",
							  " responsive='true'></canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
							" {\"y\":{\"vars\":[\"s2\",\"s3\",\"s4\"],",
							"\"smps\":[\"h1\",\"h2\",\"h3\"],\"data\"",
							":[[2,100,8],[2.5,200,5],",
							"[3,300,2]]},\"x\":[],\"z\":[]};\nvar conf ",
							"= {\"toolbarType\":\"under\",\"xAxisTitle\":\"",
							"x_axis\",\"title\":\"Title\",\"",
							"objectColorTransparency\":1,\"theme\":\"cx\",\"",
							"colorScheme\":\"CanvasXpress\",\"graphType\":\"",
							"Scatter2D\",\"hideHistogram\":true,\"",
							"showHistogram\":true,\"showFilledHistogramDensity",
							"\":true,\"showHistogramDensity\":true,",
							"\"showHistogramMedian\":true};\nvar events = ",
							"false;\nvar info = false;\nvar afterRender = [];",
							"\nvar Cobj_0_ = new CanvasXpress(\"obj_0_\", ",
							"data, conf, events, info, afterRender);\n});\n")
	container <- list(test_data_frame = data.frame(
								"V1" = c("h0","s2", "s3", "s4"), 
                                "V2" = c("h1", 2, 2.5, 3),
                                "V3" = c("h2", 100, 200, 300), 
                                "V4" = c("h3", 8,5,2), 
                      row.names = c(1,2,3,4)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$density(list(id = "test_data_frame", header = TRUE,
										  text = FALSE, row_names = TRUE,
										  fillDensity = TRUE, median = TRUE))
	output_dynamic_js <- plotter$dynamic_js
	testthat::expect_equal(output_string, expected_string)
	testthat::expect_equal(output_dynamic_js, expected_dynamic_js)
	testthat::expect_true(plotter$features$canvasXpress)
})

test_that("testing scatter2D method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"600px\" aspectRatio='1:1' responsive='true'>",
							  "</canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"s2\",\"s3\",\"s4\"],",
								  "\"smps\":[\"v1\",\"v2\",\"v3\"],\"data\":[[",
								  "10,15,12],[5,6,8],[9,10,11]]},\"x\":[],\"z",
								  "\":{\"f1\":[\"high\",\"low\",\"average\"],",
								  "\"f2\":[\"big\",\"small\",\"medium\"]}};\n",
								  "var conf = {\"toolbarType\":\"under\",\"xA",
								  "xisTitle\":\"x_axis\",\"title\":\"A\",\"ob",
								  "jectColorTransparency\":1,\"theme\":\"cx\",",
								  "\"colorScheme\":\"CanvasXpress\",\"colorBy",
								  "\":\"f1\",\"shapeBy\":\"f2\",\"graphType\":",
								  "\"Scatter2D\",\"xAxis\":\"v1\",\"yAxis\":[",
								  "\"v2\",\"v3\"],\"yAxisTitle\":\"y_axis\"};",
								  "\nvar events = false;\nvar info = false;\n",
								  "var afterRender = [];\nvar Cobj_0_ = new C",
								  "anvasXpress(\"obj_0_\", data, conf, events",
								  ", info, afterRender);\n});\n")
	container <- list(test_data_frame = data.frame(
								"V1" = c("h0","s2", "s3", "s4"), 
                                "V2" = c("f1", "high", "low", "average"),
                                "V3" = c("f2", "big", "small", "medium"), 
                                "V4" = c("v1", 10, 5, 9),
                                "V5" = c("v2", 15, 6, 10),
                                "V6" = c("v3", 12, 8, 11), 
                      row.names = c(1, 2, 3, 4)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$scatter2D(list(id = "test_data_frame", title = "A",
										  	header = TRUE, row_names = TRUE,
										  	text = "dynamic",
										  	var_attr = c(1, 2),
										  	config = list(colorBy = "f1",
										  				  shapeBy = "f2")))
	output_dynamic_js <- plotter$dynamic_js
	testthat::expect_equal(output_string, expected_string)
	testthat::expect_equal(output_dynamic_js, expected_dynamic_js)
	testthat::expect_true(plotter$features$canvasXpress)
})

test_that("testing barplot method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"300\" aspectRatio='1:1' responsive='true'></",
							  "canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"h0, s2, s3, s4\",\"h1",
								  "\",\"h2\",\"h3\"],\"smps\":[\"1\",\"2\",\"3",
								  "\"],\"data\":[[\"h0, s2, s3, s4\",\"h0, s2,",
								  " s3, s4\",\"h0, s2, s3, s4\"],[\"1\",\"2\",",
								  "\"3\"],[\"4\",\"5\",\"6\"],[\"7\",\"8\",\"9",
								  "\"]]},\"x\":{\"x_axis\":[\"h0, s2, s3, s4\"",
								  ",\"h0, s2, s3, s4\",\"h0, s2, s3, s4\"]},\"",
								  "z\":[]};\nvar conf = {\"toolbarType\":\"",
								  "under\",\"xAxisTitle\":\"x_axis\",\"title\"",
								  ":\"A\",\"objectColorTransparency\":1,\"",
								  "theme\":\"cx\",\"colorScheme\":\"",
								  "CanvasXpress\",\"graphOrientation\":\"",
								  "vertical\",\"graphType\":\"Bar\",\"colorBy",
								  "\":\"x_axis\"};\nvar events = false;\nvar ",
								  "info = false;\nvar afterRender = [];\nvar ",
								  "Cobj_0_ = new CanvasXpress(\"obj_0_\", ",
								  "data, conf, events, info, afterRender);",
								  "\n});\n")
	container <- list(test_data_frame = data.frame(
					  			"V1" = c("h0, s2, s3, s4"),
					  			"V2" = c("h1", 1, 2, 3),
					  			"V3" = c("h2", 4, 5, 6),
					  			"V4" = c("h3", 7, 8, 9),
					  row.names = c(1, 2, 3, 4)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$barplot(list(id = "test_data_frame", title = "A",
										  row_names = FALSE,	header = TRUE,
										  text = "dynamic", height = 300,
										  colorScale = TRUE,
										  config = list(
										  	'graphOrientation' = 'vertical')
										  ))
	output_dynamic_js <- plotter$dynamic_js
	testthat::expect_equal(output_string, expected_string)
	testthat::expect_equal(output_dynamic_js, expected_dynamic_js)
	testthat::expect_true(plotter$features$canvasXpress)
})

test_that("testing line method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" ",
							  "height=\"600px\" aspectRatio='1:1' ",
							  "responsive='true'></canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data",
								  " = {\"y\":{\"vars\":[\"h0, s2, s3, s4\",",
								  "\"h1\",\"h2\",\"h3\"],\"smps\":[\"1\",\"2",
								  "\",\"3\"],\"data\":[[\"h0, s2, s3, s4\",\"",
								  "h0, s2, s3, s4\",\"h0, s2, s3, s4\"],[\"1\"",
								  ",\"2\",\"3\"],[\"4\",\"5\",\"6\"],[\"7\",\"",
								  "8\",\"9\"]]},\"x\":[],\"z\":[]};\nvar conf ",
								  "= {\"toolbarType\":\"under\",\"xAxisTitle\"",
								  ":\"x_axis\",\"title\":\"A\",\"",
								  "objectColorTransparency\":1,\"theme\":\"cx",
								  "\",\"colorScheme\":\"CanvasXpress\",\"",
								  "graphType\":\"Line\"};\nvar events = false;",
								  "\nvar info = false;\nvar afterRender = [];",
								  "\nvar Cobj_0_ = new CanvasXpress(\"obj_0_\"",
								  ", data, conf, events, info, afterRender);\n",
								  "});\n")
	container <- list(test_data_frame = data.frame(
					  			"V1" = c("h0, s2, s3, s4"),
					  			"V2" = c("h1", 1, 2, 3),
					  			"V3" = c("h2", 4, 5, 6),
					  			"V4" = c("h3", 7, 8, 9),
					  row.names = c(1, 2, 3, 4)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$line(list(id = "test_data_frame", title = "A",
									   header = TRUE, row_names = FALSE,
									   text = "dynamic"))
	output_dynamic_js <- plotter$dynamic_js
	testthat::expect_equal(output_string, expected_string)
	testthat::expect_equal(output_dynamic_js, expected_dynamic_js)
	testthat::expect_true(plotter$features$canvasXpress)
})

# table <- data.frame(            
# 		V1 = c("tissue",     "nerv", "pcr",  "gen1", "gen2", "gen3",  "gen4"),    
#         V2 = c("type",       "-",    "-",    "miRNA","miRNA","mRNA",  "mRNA"),
# 		V3 = c("type2",      "-",    "-",    "tRNA", "tRNA", "ncRNA", "ncRNA"),
# 		V4 = c("liver",      "no",   "true", "20",   "40",   "100",   "85"),
# 		V5 = c("brain",      "yes",  "true", "13",   "60",   "85",    "10"),
# 		V6 = c("cerebellum", "yes",  "false","15",   "30",   "12",    "41"))

test_that("test tree configuration", {
	
	plotter <- htmlReport$new()

	options <- list("id"= "complex_table",
            "var_attr"= c(1,2), #Variable attributes
            "smp_attr"= c(1,2), #Sample attributes
            "header"= TRUE, 
            "row_names"= TRUE, 
            #"transpose"= False, We are not testing this option as most of the functions (table, and the different plot functions) already set the expected behaviour according to the needs 
            "layout"= "forcedir", #Testing graph layout
            "x_label"= "x_axis", #Testing plots layout
            'title'= 'Title',
            'alpha'= 1,
            'theme'= 'cx',
            'color_scheme'= 'CanvasXpress'
            )

	config <- list('toolbarType' = 'under',
		           'xAxisTitle' = options$x_label,
		           'title' = options$title,
		           "objectColorTransparency"= options$alpha,
		           "theme"= options$theme,
		           "colorScheme"= options$color_scheme)

	tree_file <- file.path(plotter$source_folder, "exData", "test_tree.txt")
	if (!file.exists(tree_file))
		tree_file <- file.path(plotter$source_folder, "inst", "exData", "test_tree.txt")

    options<- update_options(options, list("tree" = tree_file,
                             			   "treeBy" = "v"))

    config <- plotter$set_tree(options, config)
   	testthat::expect_true(!is.null(config$varDendrogramNewick))
   	testthat::expect_true(length(config$varDendrogramNewick) > 0)
   	testthat::expect_true(config$varDendrogramUseHeight)
   	testthat::expect_false(config$varDendrogramHang)
})