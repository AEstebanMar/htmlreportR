

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

		frmt_var_attr_exp <- list(data_frame = data.frame( "V1" = c("r1", "r2"), 
														   "V2" = c(1,2), 
														   "V3" = c(3,4), 
														   row.names = c(2,3)),
								  smp_attr = NULL,
								  var_attr =  data.frame("V1" = "h0", 
								 					     "V2" = "h1",
								 						 "V3" = "h2",
								 						 row.names = c(1)))

		frmt_smp_attr_exp <- list(data_frame = data.frame( "V2" = c("h1",1,2), 
														   "V3" = c("h2",3,4), 
														   row.names = c(1,2,3)),
								  smp_attr = data.frame("V1" = c("h0","r1", "r2"),
								  						row.names = c(1,2,3)),
								  var_attr =  NULL)
		frmt_smp_var_attr_exp <- list(data_frame = data.frame( "V2" = c(1,2), 
															   "V3" = c(3,4), 
															   row.names = c(2,3)),
									  smp_attr = data.frame("V1" = c("r1", "r2"),
								  					        row.names = c(2,3)),
									  var_attr =  data.frame("V2" = "h1",
								 						     "V3" = "h2",
								 						      row.names = c(1)))


	   	user_options <- list("header" = FALSE, "row_names" = FALSE, smp_attr = NULL, var_attr = NULL, text = TRUE)
	   	user_options_smp_attr <- list("header" = FALSE, "row_names" = FALSE, smp_attr = c(1), var_attr = NULL, text = TRUE)
	   	user_options_var_attr <- list("header" = FALSE, "row_names" = FALSE, smp_attr = NULL, var_attr = c(1), text = c(1))
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
							     					 "V2" = c("h1", "-","smp_attr1", "smp_attr2"),
							     					 "V3" = c("h2", "var_attr1", 1,2), 
							     					 "V4" = c("h3", "var_attr2",3,4), 
							     					 row.names = c(1,2,3,4)))
		plotter <- htmlReport$new(container = container)



		formatted_data_exp <- list(data_frame = data.frame("h2" = c(1,2),
													   "h3" = c(3,4), 
													   row.names = c("r2", "r3")),
							   smp_attr = data.frame("h1" = c("smp_attr1", "smp_attr2"), 
												     row.names = c("r2", "r3")),
							   var_attr = data.frame("h2" = c("var_attr1"),
								    				 "h3" = c("var_attr2"),
													 row.names = c("r1")))
		
		formatted_data_exp_mod <- list(data_frame = data.frame("h2" = c("1","2"),
															   "h3" = c("3","4"), 
													   			row.names = c("r2", "r3")),
									   smp_attr = data.frame("h1" = c("smp_attr1", "smp_attr2"), 
														     row.names = c("r2", "r3")),
									   var_attr = data.frame("h2" = c("var_attr1"),
										    				 "h3" = c("var_attr2"),
															 row.names = c("r1")))
		

		formatted_data_transposed_exp <- list(data_frame = data.frame("r2" = c(1,3),
															      "r3" = c(2,4), 
													   row.names = c("h2", "h3")),
							   smp_attr = data.frame( "r1" = c("var_attr1", "var_attr2"), 
												     row.names = c("h2", "h3")),
							   var_attr = data.frame("r2" = c("smp_attr1"),
								    				 "r3" = c("smp_attr2"),
													 row.names = c("h1")))
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
							     					 "V2" = c("h1", "-","smp_attr1", "smp_attr2"),
							     					 "V3" = c("h2", "var_attr1", 1,2), 
							     					 "V4" = c("h3", "var_attr2",3,4), 
							     					 row.names = c(1,2,3,4)))
		plotter <- htmlReport$new(container = container)


		formatted_data_exp <- list(data_frame = data.frame("h2" = c(1,2),
														   "h3" = c(3,4), 
														   row.names = c("r2", "r3")),
								   smp_attr = data.frame("h1" = c("smp_attr1", "smp_attr2"), 
													     row.names = c("r2", "r3")),
								   var_attr = data.frame("h2" = c("var_attr1"),
									    				 "h3" = c("var_attr2"),
														 row.names = c("r1")),
								   samples = c("h2", "h3"),
								   variables = c("r2", "r3"))
		
		formatted_data <- plotter$get_data_for_plot(options)
	    expect_equal(formatted_data, formatted_data_exp)
})

test_that("testing the table formatting in the class htmlReport", {
	expected_output <-"<table id=Sample border=1 class=table >
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
<td> h0</td>
<td> h1</td>
<td> h2</td>
<td> h3</td>
</tr>
<tr>
<td> 2 </td>
<td> r1</td>
<td> -</td>
<td> var_attr1</td>
<td> var_attr2</td>
</tr>
<tr>
<td> 3 </td>
<td> r2</td>
<td> smp_attr1</td>
<td> 1</td>
<td> 3</td>
</tr>
<tr>
<td> 4 </td>
<td> r3</td>
<td> smp_attr2</td>
<td> 2</td>
<td> 4</td>
</tr>
</tbody>
</table>"
	test_data_frame <- data.frame( "V1" = c("h0","r1", "r2", "r3"), 
                                "V2" = c("h1", "-","smp_attr1", "smp_attr2"),
                                "V3" = c("h2", "var_attr1", 1,2), 
                                "V4" = c("h3", "var_attr2",3,4), 
                                row.names = c(1,2,3,4))
	plotter <- htmlReport$new()
	actual_output <- plotter$parse_data_frame(test_data_frame, "Sample", 1, TRUE)

	testthat::expect_equal(actual_output, expected_output)
})
####### HTML REPORTING
#make_head
#build_body