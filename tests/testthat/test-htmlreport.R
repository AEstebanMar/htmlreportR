
#add_header_row_names

test_that("add_header_row_names, both set to FALSE", {
	plotter <- htmlReport$new()
	input_df <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2",
							  "2", "4"), nrow = 3, byrow = TRUE))
	user_options <- list(header = FALSE, row_names = FALSE)
	expected_df <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3",
								 "r2", "2", "4"), nrow = 3, byrow = TRUE))
	output_df <- plotter$add_header_row_names(input_df, user_options)
	expect_equal(output_df, expected_df)
})

test_that("add_header_row_names, rownames set to TRUE", {
	plotter <- htmlReport$new()
	input_df <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2",
							  "2", "4"), nrow = 3, byrow = TRUE))
	user_options <- list(header = FALSE, row_names = TRUE) 
	expected_df <- as.data.frame(matrix(c("h1", "h2", "1", "3", "2", "4"),
								 nrow = 3, byrow = TRUE))
	colnames(expected_df) <- paste0("V", 2:3)
	rownames(expected_df) <- c("0","r1","r2")
	output_df <- plotter$add_header_row_names(input_df, user_options)
	expect_equal(output_df, expected_df)
})

test_that("add_header_row_names, header set to TRUE", {
	plotter <- htmlReport$new()
	input_df <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2",
							  "2", "4"), nrow = 3, byrow = TRUE))
	user_options <- list(header = TRUE, row_names = FALSE)
	expected_df <- as.data.frame(matrix(c("r1","1", "3", "r2","2", "4"),
								 nrow = 2, byrow = TRUE))
	colnames(expected_df) <- c("0","h1","h2")
	rownames(expected_df) <- 2:3
	output_df <- plotter$add_header_row_names(input_df, user_options)
	expect_equal(output_df, expected_df)
})

test_that("add_header_row_names, both set to TRUE", {
	plotter <- htmlReport$new()
	input_df <- as.data.frame(matrix(c("0", "h1", "h2", "r1", "1", "3", "r2",
							  "2", "4"), nrow = 3, byrow = TRUE))
	user_options <- list(header = TRUE, row_names = TRUE)
	expected_df <- as.data.frame(matrix(c("1", "3", "2", "4"), nrow = 2,
								 byrow = TRUE))
	colnames(expected_df) <- c("h1","h2")
	rownames(expected_df) <- c("r1","r2")
	output_df <- plotter$add_header_row_names(input_df, user_options)
	expect_equal(output_df, expected_df)
})

test_that("extract_data properly handles a redundant \"fields\" argument", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 fields = 1:2)
	input_df <- data.frame(values = 1:4, order = c("First", "Second",
						   "Third", "Fourth"))
	plotter$hash_vars$test_df <- input_df
	rownames(input_df) <- 1:4
	output_df <- plotter$extract_data(user_options)$data_frame
	expect_equal(output_df, input_df)
})

test_that("fields can reverse order of data frame in extract_data", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 fields = 2:1)
	input_df <- data.frame(values = 1:4,
							  order = c("First", "Second", "Third", "Fourth"))
	plotter$hash_vars$test_df <- input_df
	rownames(input_df) <- 1:4
	output_df <- plotter$extract_data(user_options)$data_frame
	expect_equal(output_df, input_df[, 2:1])
	})

test_that("Complex case of extract_data with \"fields\" argument", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 fields = c(3, 1, 2))
	input_df <- data.frame(c("row", "tissue", toupper(letters[1:3])),
						   c("values", "connective", 1:3),
						   c("order", "muscle", "First", "Second", "Third"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output_df <- plotter$extract_data(user_options)$data_frame
	expected_df <- input_df[, user_options$fields]
	expect_equal(output_df, expected_df)
})

test_that("extract_data \"fields\" argument works with var_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
					 	 smp_attr = NULL, var_attr = 1, text = "dynamic",
					 	 fields = 2)
	input_df <- data.frame(1:5, c("First", "Second", "Third",
								  "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	expected_df <- input_df[-1, 2, drop = FALSE]
	expected_var_attr <- input_df[1, -1, drop = FALSE]
	output <- plotter$extract_data(user_options)
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$var_attr, expected_var_attr)
	expect_null(output$smp_attr)
})

test_that("extract_data \"fields\" argument works with smp_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = NULL, text = "dynamic",
						 fields = 2)
	input_df <- data.frame(1:5, c("First", "Second", "Third",
								  "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[, -1, drop = FALSE]
	expected_smp_attr <- input_df[, 1, drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$smp_attr, expected_smp_attr)
	expect_null(output$var_attr)
})

test_that("extract_data \"fields\" argument works with var_attr and smp_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", eader = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = 3, text = "dynamic",
						 fields = 2)
	input_df <- data.frame(toupper(letters[1:5]), 1:5, letters[1:5],
					       c("First", "Second", "Third", "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[-3, 2, drop = FALSE]
	expected_var_attr <- input_df[3, 2, drop = FALSE]
	expected_smp_attr <- input_df[-3, 1, drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$var_attr, expected_var_attr)
	expect_equal(output$smp_attr, expected_smp_attr)
})

test_that("extract_data properly handles a redundant \"rows\" argument", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 rows = 1:4)
	input_df <- data.frame(values = 1:4, order = c("First", "Second",
						   "Third", "Fourth"))
	rownames(input_df) <- 1:4
	plotter$hash_vars$test_df <- input_df
	output_df <- plotter$extract_data(user_options)$data_frame
	expect_equal(output_df, input_df)
})

test_that("rows can reverse order of data frame in extract_data", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 rows = 4:1)
	input_df <- data.frame(values = 1:4,
							  order = c("First", "Second", "Third", "Fourth"))
	rownames(input_df) <- 1:4
	plotter$hash_vars$test_df <- input_df
	output_df <- plotter$extract_data(user_options)$data_frame
	expect_equal(output_df, input_df[4:1, ])
	})

test_that("Complex case of extract_data with \"rows\" argument", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = NULL, text = "dynamic",
						 rows = c(3, 4, 2, 1))
	input_df <- data.frame(c("row", "tissue", toupper(letters[1:3])),
						   c("values", "connective", 1:3),
						   c("order", "muscle", "First", "Second", "Third"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output_df <- plotter$extract_data(user_options)$data_frame
	expected_df <- input_df[user_options$rows, ]
	expect_equal(output_df, expected_df)
})

test_that("extract_data \"rows\" argument works with var_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = NULL, var_attr = 1, text = "dynamic",
						 rows = 2)
	input_df <- data.frame(1:5, c("First", "Second", "Third",
								  "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[2, , drop = FALSE]
	expected_var_attr <- input_df[1, , drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$var_attr, expected_var_attr)
	expect_null(output$smp_attr)
})

test_that("extract_data \"rows\" argument works with smp_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE, smp_attr = 1,
						 var_attr = NULL, text = "dynamic", rows = 2)
	input_df <- data.frame(1:5, c("First", "Second", "Third",
								  "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[2, 2, drop = FALSE]
	expected_smp_attr <- input_df[2, 1, drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$smp_attr, expected_smp_attr)
	expect_null(output$var_attr)
})

test_that("extract_data \"rows\" argument works with var_attr and smp_attr", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = 3, text = "dynamic",
						 fields = 2)
	input_df <- data.frame(toupper(letters[1:5]), 1:5, letters[1:5],
					       c("First", "Second", "Third", "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[-3, 2, drop = FALSE]
	expected_smp_attr <- input_df[-3, 1, drop = FALSE]
	expected_var_attr <- input_df[3, 2, drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$var_attr, expected_var_attr)
	expect_equal(output$smp_attr, expected_smp_attr)
})


test_that("extract_data works with var_attr and smp_attr with no \"rows\"
		   nor \"fields\" argument", {
	plotter <- htmlReport$new()
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = 3, text = "dynamic")
	input_df <- data.frame(toupper(letters[1:5]), 1:5, letters[1:5],
						   c("First", "Second", "Third", "Fourth", "Fifth"))
	rownames(input_df) <- paste0("sample", seq(nrow(input_df)))
	colnames(input_df) <- paste0("var", seq(ncol(input_df)))
	plotter$hash_vars$test_df <- input_df
	output <- plotter$extract_data(user_options)
	expected_df <- input_df[-3, -1]
	expected_var_attr <- input_df[3, -1, drop = FALSE]
	expected_smp_attr <- input_df[-3, 1, drop = FALSE]
	expect_equal(output$data_frame, expected_df)
	expect_equal(output$var_attr, expected_var_attr)
	expect_equal(output$smp_attr, expected_smp_attr)
})

#extract_data

test_that("Simple case for extract_data", {
	plotter <- htmlReport$new()
	input_df <- data.frame(V1 = c("h0","r1", "r2"), V2 = c("h1", 1,2), 
						   V3 = c("h2", 3,4), row.names = 1:3)
	plotter$hash_vars$test_df <- input_df
	expected_df <- data.frame(V1 = c("h0","r1", "r2"), 
							  V2 = c("h1", "1", "2"),
							  V3 = c("h2", "3", "4"))
   	user_options <- list(id = "test_df", header = NULL, row_names = NULL,
   						 smp_attr = NULL, var_attr = NULL, text = TRUE)
    output <- plotter$extract_data(user_options)
    expect_equal(output$data_frame, expected_df)
    expect_null(output$smp_attr)
    expect_null(output$var_attr)
})

test_that("extract_data with var_attr, text set to dynamic", {
	plotter <- htmlReport$new()
	input_df <- data.frame(V1 = c("h0", "r1", "r2"), V2 = c("h1", 1:2), 
						   V3 = c("h2", 3:4))
	plotter$hash_vars$test_df <- input_df
	expected_df <- data.frame(V1 = c("r1", "r2"), V2 = c("1", "2"),
							  V3 = c("3", "4"), row.names = 2:3)
	expected_var_attr <- data.frame(V1 = "h0", V2 = "h1", V3 = "h2")
   	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE, smp_attr = NULL,
   						 var_attr = 1, text = "dynamic")
    output <- plotter$extract_data(user_options)
	expect_equal(output$data_frame, expected_df)
    expect_equal(output$var_attr, expected_var_attr)	
    expect_null(output$smp_attr)
})

test_that("extract_data with smp_attr, text set to TRUE", {
	plotter <- htmlReport$new()
	input_df <- data.frame(V1 = c("h0", "r1", "r2"), V2 = c("h1", 1:2), 
						   V3 = c("h2", 3:4))
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = NULL, text = TRUE)
	plotter$hash_vars$test_df <- input_df
	expected_df <- data.frame(V2 = c("h1", "1", "2"), 
							  V3 = c("h2", "3", "4"))
	expected_smp_attr <- data.frame(V1 = c("h0", "r1", "r2"))
   	output <- plotter$extract_data(user_options)
    expect_equal(output$data_frame, expected_df)
	expect_equal(output$smp_attr, expected_smp_attr)
    expect_null(output$var_attr)
})

test_that("extract_data with smp_attr and var_attr", {
	plotter <- htmlReport$new()
	input_df <- data.frame(V1 = c("h0","r1", "r2"), V2 = c("h1", 1, 2), 
						   V3 = c("h2", 3, 4))
	plotter$hash_vars$test_df <- input_df
	user_options <- list(id = "test_df", header = FALSE, row_names = FALSE,
						 smp_attr = 1, var_attr = 1, text = FALSE)
	expected_df <- data.frame(V2 = as.character(1:2), V3 = as.character(3:4),
							  row.names = 2:3)
	expected_var_attr <- data.frame(V2 = "h1", V3 = "h2")
	expected_smp_attr <- data.frame(V1 = c("r1", "r2"), row.names = 2:3)
    output <- plotter$extract_data(user_options)
    expect_equal(output$data_frame, expected_df)
    expect_equal(output$smp_attr, expected_smp_attr)
    expect_equal(output$var_attr, expected_var_attr)
})

#get_data

test_that("get_data with row_names and header set to TRUE", {
	test_data_frame = data.frame(c("h0","s2", "s3", "s4"), 
                                 c("h1", 2, 2.5, 3),
                                 c("h2", 100, 200, 300), 
                                 c("h3", 8,5,2))
	plotter <- htmlReport$new(container = list(test = test_data_frame))
	options <- list(id = "test", header = TRUE, text = FALSE, row_names = TRUE,
				    transpose = FALSE)
	expected_df <- data.frame(h1 = c(2.0, 2.5, 3), h2 = c(100, 200, 300),
							  h3 = c(8, 5, 2))
	rownames(expected_df) <- c("s2", "s3", "s4")
	output <- plotter$get_data(options)
	expect_equal(output$data_frame, expected_df)
	expect_null(output$smp_attr)
	expect_null(output$var_attr)
})

test_that("get_data with smp_attr and var_attr", {
	options <- list(id = "input_df", transpose = FALSE, header = TRUE,
					row_names = TRUE, smp_attr = 2, var_attr = 2,
					text = FALSE)
	input_df = data.frame(V1 = c("h0","r1", "r2", "r3"),
							V2 = c("h1", "-","smp_attr1", "smp_attr2"),
							V3 = c("h2", "var_attr1", 1, 2), 
							V4 = c("h3", "var_attr2", 3, 4), 
							row.names = 1:4)
	plotter <- htmlReport$new(container = list(input_df = input_df))
	expected_df <- data.frame(h2 = 1:2, h3 = 3:4,
							  row.names = c("r2", "r3"))
	expected_var_attr <- list(c("r1", "var_attr1", "var_attr2"))
	expected_smp_attr <- list(c("h1", "smp_attr1", "smp_attr2"))
	output <- plotter$get_data(options)
    expect_equal(output$data_frame, expected_df)
    expect_equal(output$smp_attr, expected_smp_attr)
    expect_equal(output$var_attr, expected_var_attr)
})

test_that("get_data with smp_attr, var_attr and mod func", {
	mod_function <- function(data_frame){
		row_names <- rownames(data_frame)
		mod_df <- as.data.frame(lapply(data_frame, as.character))
		rownames(mod_df) <- row_names
		mod_df
	}
	options <- list(id = "input_df", transpose = FALSE, header = TRUE,
					row_names = TRUE, smp_attr = 2, var_attr = 2,
					text = FALSE, func = mod_function)
	input_df <- data.frame(V1 = c("h0","r1", "r2", "r3"),
							 V2 = c("h1", "-","var_attr1", "var_attr2"),
							 V3 = c("h2", "smp_attr1", 1, 2), 
							 V4 = c("h3", "smp_attr2", 3, 4), 
							 row.names = 1:4)
	expected_df <- data.frame(h2 = c("1","2"), h3 = c("3","4"),
							  row.names = c("r2", "r3"))
	plotter <- htmlReport$new(container = list(input_df = input_df))
	output <- plotter$get_data(options)
    expect_equal(output$data_frame, expected_df)
})

test_that("get_data with smp_attr and var_attr. Transpose set to TRUE", {
	options <- list(id = "input_df", header = TRUE, row_names = TRUE,
					smp_attr = 2, var_attr = 2, text = FALSE, transpose = TRUE)
	input_df = data.frame(V1 = c("h0","r1", "r2", "r3"),
						  V2 = c("h1", "-","var_attr1", "var_attr2"),
						  V3 = c("h2", "smp_attr1", 1, 2),
						  V4 = c("h3", "smp_attr2", 3, 4),
						  row.names = 1:4)
	expected_df <- data.frame(r2 = c(1, 3), r3 = c(2, 4), 
							  row.names = c("h2", "h3"))
    expected_var_attr <- list(c("h1", "var_attr1", "var_attr2"))
    expected_smp_attr <- list(c("r1", "smp_attr1", "smp_attr2"))
    plotter <- htmlReport$new(container = list(input_df = input_df))
    output <- plotter$get_data(options)
    expect_equal(output$data_frame, expected_df)
    expect_equal(output$smp_attr, expected_smp_attr)
    expect_equal(output$var_attr, expected_var_attr)
})

test_that("testing the table formatting in the class htmlReport",{
	options <- list(id = "input_df",
					transpose = FALSE,
					header = TRUE,
					row_names = TRUE,
					smp_attr = 2,
					var_attr = 2,
					text = FALSE)
	input_df <- data.frame(V1 = c("h0","r1", "r2", "r3"), 
						   V2 = c("h1", "-","smp_attr1", "smp_attr2"),
						   V3 = c("h2", "var_attr1", 1, 2), 
						   V4 = c("h3", "var_attr2", 3, 4), 
						   row.names = c(1,2,3,4))
	plotter <- htmlReport$new(container = list(input_df = input_df))
	output <- plotter$get_data_for_plot(options)
	expected_df <- data.frame(h2 = 1:2, h3 = 3:4, 
							  row.names = c("r2", "r3"))
	expected_smp_attr <- list(c("h1", "smp_attr1", "smp_attr2"))
	expected_var_attr <- list(c("r1", "var_attr1", "var_attr2"))
    expect_equal(output$data_frame, expected_df)
    expect_equal(output$smp_attr, expected_smp_attr)
    expect_equal(output$var_attr, expected_var_attr)
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
	input_df <- data.frame(V1 = c("h0","r1", "r2", "r3"), 
                      	   V2 = c("h1", "-","smp_attr1", "smp_attr2"),
                      	   V3 = c("h2", "var_attr1", 1, 2), 
                      	   V4 = c("h3", "var_attr2", 3, 4), 
                      	   row.names = 1:4)
	plotter <- htmlReport$new()
	plotter$hash_vars$test_df <- input_df
	actual_output <- plotter$table(list(id = "test_df", header = TRUE,
										row_names = TRUE, smp_attr = 2,
										var_attr = 2))
	expect_equal(actual_output, expected_output)
})

test_that("testing the full table formatting in the class htmlReport, renaming
		   table rownames", {
	expected_output <-"<table id=table_0 border=1  >
<thead>
<tr>
<th> custom </th>
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
	input_df <- data.frame(V1 = c("h0","r1", "r2", "r3"), 
                      	   V2 = c("h1", "-","smp_attr1", "smp_attr2"),
                      	   V3 = c("h2", "var_attr1", 1, 2), 
                      	   V4 = c("h3", "var_attr2", 3, 4), 
                      	   row.names = 1:4)
	plotter <- htmlReport$new()
	plotter$hash_vars$test_df <- input_df
	actual_output <- plotter$table(list(id = "test_df", header = TRUE,
										row_names = TRUE, smp_attr = 2, table_rownames = TRUE,
										var_attr = 2, rownames_col = "custom"))
	expect_equal(actual_output, expected_output)
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
                                "V3" = c("h2", "var_attr1", 1, 2), 
                                "V4" = c("h3", "var_attr2", 3, 4), 
                                row.names = 1:4)
	plotter <- htmlReport$new()
	options <- list(border = 1, table_rownames = TRUE,
		   			rownames_col = "rownames")
	actual_output <- plotter$parse_data_frame(test_data_frame, options,
											  "Sample")
	expect_equal(actual_output, expected_output)
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
	expect_equal(res_img, exp_img)
})

test_that("testing density method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" ",
							  "height=\"600px\" aspectRatio='1:1'",
							  " responsive='true'></canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"s2\",\"s3\",\"s4\"],\"",
								  "smps\":[\"h1\",\"h2\",\"h3\"],\"data\":[[2,",
								  "100,8],[2.5,200,5],[3,300,2]]},\"x\":[],\"z",
								  "\":[]};\nvar conf = {\"toolbarType\":\"",
								  "under\",\"xAxisTitle\":\"x_axis\",\"title\"",
								  ":\"Title\",\"objectColorTransparency\":1,\"",
								  "theme\":\"cx\",\"colorScheme\":\"",
								  "CanvasXpress\",\"graphType\":\"Scatter2D\",",
								  "\"hideHistogram\":true,\"showHistogram\":",
								  "true,\"showFilledHistogramDensity\":true,\"",
								  "showHistogramDensity\":true,\"",
								  "showHistogramMedian\":true};\nvar events = ",
								  "false;\nvar info = false;\nvar afterRender ",
								  "= [];\nvar Cobj_0_ = new CanvasXpress(\"",
								  "obj_0_\", data, conf, events, info, ",
								  "afterRender);\n});\n")
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
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("testing scatter2D method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"600px\" aspectRatio='1:1' responsive='true'>",
							  "</canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"s4\"],\"smps\":[\"f1\"",
								  ",\"f2\",\"v1\",\"v2\",\"v3\"],\"data\":[[\"",
								  "average\",\"medium\",9,10,11]]},\"x\":{\"s2",
								  "\":[\"high\",\"big\",\"10\",\"15\",\"12\"],",
								  "\"s3\":[\"low\",\"small\",\"5\",\"6\",\"8\"",
								  "]},\"z\":[]};\nvar conf = {\"toolbarType\":",
								  "\"under\",\"xAxisTitle\":\"x_axis\",\"title",
								  "\":\"A\",\"objectColorTransparency\":1,\"",
								  "theme\":\"cx\",\"colorScheme\":\"",
								  "CanvasXpress\",\"colorBy\":\"f1\",\"shapeBy",
								  "\":\"f2\",\"graphType\":\"Scatter2D\",\"",
								  "xAxis\":\"f1\",\"yAxis\":[\"f2\",\"v1\",\"",
								  "v2\",\"v3\"],\"yAxisTitle\":\"y_axis\"};\n",
								  "var events = false;\nvar info = false;\nvar",
								  " afterRender = [];\nvar Cobj_0_ = new ",
								  "CanvasXpress(\"obj_0_\", data, conf, events,",
								  " info, afterRender);\n});\n")
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
										  	var_attr = 2:3,
										  	config = list(colorBy = "f1",
										  				  shapeBy = "f2")))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("testing barplot method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"300\" aspectRatio='1:1' responsive='true'></",
							  "canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"h0\",\"h1\",\"h2\",",
								  "\"h3\"],\"smps\":[\"2\",\"3\",",
								  "\"4\"],\"data\":[[\"s2\",\"s3\",\"s4",
								  "\"],[\"1\",\"2\",\"3\"],[\"4\",\"5\",\"6\"]",
								  ",[\"7\",\"8\",\"9\"]]},\"x\":{\"x_axis\":[",
								  "\"s2\",\"s3\",\"s4\"]},\"",
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
	input_df <- data.frame(V1 = c("h0", "s2", "s3", "s4"),
						   V2 = c("h1", 1, 2, 3),
					  	   V3 = c("h2", 4, 5, 6), V4 = c("h3", 7, 8, 9),
					  	   row.names = c(1, 2, 3, 4))
	container <- list(test_df = input_df)
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$barplot(list(id = "test_df", title = "A",
										  row_names = FALSE, header = TRUE,
										  text = "dynamic", height = 300,
										  colorScale = TRUE,
										  config = list(
										  	'graphOrientation' = 'vertical')
										  ))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("testing barplot method of htmlReport class, colorBy", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"300\" aspectRatio='1:1' responsive='true'></",
							  "canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
		" {\"y\":{\"vars\":[\"h1\",\"h2\",\"h3\"],\"smps\":[\"2\",\"3\",\"4\"]",
		",\"data\":[[1,2,3],[4,5,6],[7,8,9]]},\"x\":{\"h0\":[\"s2\",\"s3\",\"s",
		"4\"]},\"z\":[]};\nvar conf = {\"toolbarType\":\"under\",\"xAxisTitle",
		"\":\"x_axis\",\"title\":\"A\",\"objectColorTransparency\":1,\"theme\"",
		":\"cx\",\"colorScheme\":\"CanvasXpress\",\"graphOrientation\":\"verti",
		"cal\",\"graphType\":\"Bar\",\"colorBy\":\"V1\"};\nvar events = false;",
		"\nvar info = false;\nvar afterRender = [];\nvar Cobj_0_ = new CanvasX",
		"press(\"obj_0_\", data, conf, events, info, afterRender);\n});\n")
	input_df <- data.frame(V1 = c("h0", "s2", "s3", "s4"),
						   V2 = c("h1", 1, 2, 3),
					  	   V3 = c("h2", 4, 5, 6), V4 = c("h3", 7, 8, 9),
					  	   row.names = c(1, 2, 3, 4))
	container <- list(test_df = input_df)
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$barplot(list(id = "test_df", title = "A",
										  row_names = FALSE, header = TRUE,
										  text = "dynamic", height = 300,
										  colorBy = "V1", smp_attr = 1,
										  config = list(
										  	'graphOrientation' = 'vertical')
										  ))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("testing line method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" ",
							  "height=\"600px\" aspectRatio='1:1' ",
							  "responsive='true'></canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data",
								  " = {\"y\":{\"vars\":[\"h0\",\"h1\",\"h2\",",
								  "\"h3\"],\"smps\":[\"2\",\"3\",",
								  "\"4\"],\"data\":[[\"s2\",\"s3\",\"s4",
								  "\"],[\"1\",\"2\",\"3\"],[\"4\",\"5\",\"6\"]",
								  ",[\"7\",\"8\",\"9\"]]},\"x\":[],\"z\":[]};",
								  "\nvar conf = {\"toolbarType\":\"under\",\"",
								  "xAxisTitle\":\"x_axis\",\"title\":\"A\",\"",
								  "objectColorTransparency\":1,\"theme\":\"cx",
								  "\",\"colorScheme\":\"CanvasXpress\",\"",
								  "graphType\":\"Line\"};\nvar events = false;",
								  "\nvar info = false;\nvar afterRender = [];",
								  "\nvar Cobj_0_ = new CanvasXpress(\"obj_0_\"",
								  ", data, conf, events, info, afterRender);\n",
								  "});\n")
	container <- list(test_data_frame = data.frame(
					  			"V1" = c("h0", "s2", "s3", "s4"),
					  			"V2" = c("h1", 1, 2, 3),
					  			"V3" = c("h2", 4, 5, 6),
					  			"V4" = c("h3", 7, 8, 9),
					  row.names = c(1, 2, 3, 4)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$line(list(id = "test_data_frame", title = "A",
									   header = TRUE, row_names = FALSE,
									   text = "dynamic"))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("testing boxplot method of htmlReport class", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
						      "\"600px\" aspectRatio='1:1' responsive='true'>",
						      "</canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"lung\"],\"smps\":[\"",
								  "gene1\",\"gene2\",\"gene3\"],\"data\":[[50,",
								  "60,70]]},\"x\":{\"pathway\":[\"a\",\"b\",",
								  "\"a\"],\"dataset\":[\"DO\",\"DO\",\"EH\"],",
								  "\"type\":[\"paper\",\"paper\",\"abstract\"]",
								  ",\"top\":[\"top3\",\"top4\",\"top4\"]},\"z",
								  "\":[]};\nvar conf = {\"toolbarType\":\"",
								  "under\",\"xAxisTitle\":\"x_axis\",\"title\"",
								  ":\"Title\",\"objectColorTransparency\":1,\"",
								  "theme\":\"cx\",\"colorScheme\":\"",
								  "CanvasXpress\",\"graphOrientation\":\"",
								  "vertical\",\"colorBy\":\"top\",\"graphType",
								  "\":\"Boxplot\",\"groupingFactors\":[\"",
								  "pathway\",\"dataset\"],\"segregateSamplesBy",
								  "\":\"type\"};\nvar events = false;\nvar ",
								  "info = false;\nvar afterRender = [];\nvar",
								  " Cobj_0_ = new CanvasXpress(\"obj_0_\", ",
								  "data, conf, events, info, afterRender);\n})",
								  ";\n")
	df <- data.frame(c("tissue", paste0("gene", 1:3)),
					 c("lung", seq(50, 70, by = 10)),
					 c("pathway", "a", "b", "a"),
					 c("dataset", "DO", "DO", "EH"),
					 c("type", "paper", "paper", "abstract"),
					 c("top", "top3", "top4", "top4"))
	container <- list(test_data_frame = df)
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$boxplot(list(id = 'test_data_frame', header = TRUE,
									 row_names = TRUE, text = "dynamic",
									 format = "long", smp_attr = 3:6,
									 group = c("pathway", "dataset", "type"),
									 config=list(graphOrientation = "vertical",
									 colorBy = "top")))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

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
   	expect_true(!is.null(config$varDendrogramNewick))
   	expect_true(length(config$varDendrogramNewick) > 0)
   	expect_true(config$varDendrogramUseHeight)
   	expect_false(config$varDendrogramHang)
})

test_that("Prettify_div works with default parameters", {
	plotter <- htmlReport$new()
	expected_string <- "<div>\nI am simple\n</div>"
	output_string <- plotter$prettify_div("I am simple")
	expect_equal(output_string, expected_string)
})

test_that("Prettify_div works with custom parameters", {
	plotter <- htmlReport$new()
	expected_string <- paste0("<div style =\"overflow: show; display: ",
							  "contract; contract-direction: column; ",
							  "justify-content: left\">\nI am complex\n</div>")
	output_string <- plotter$prettify_div("I am complex", overflow = "show",
										  display = "contract",
										  direction = "column",
										  justify = "left")
	expect_equal(output_string, expected_string)
})

test_that("Prettify_div works with magic preset", {
	plotter <- htmlReport$new()
	expected_string <- paste0("<div style =\"overflow: hidden; display: ",
							  "flex; flex-direction: row; ",
							  "justify-content: center\">\nI am magic\n</div>")
	output_string <- plotter$prettify_div("I am magic", preset = "magic")
	expect_equal(output_string, expected_string)
})

# Create index list for index tests

ids <- seq(5)
names <- letters[seq(5)]
hlevels <- c(1, 2, 3, 1, 3)
index_items <- as.matrix(data.frame(ids, names, hlevels))
dimnames(index_items) <- NULL

test_that("Testing create_header_index method", {
	plotter <- htmlReport$new()
	plotter$index_items <- index_items
	plotter$index_type <- "none"
	output <- plotter$create_header_index()
	expected <- paste0("<h1>Table of contents</h1>\n<div>\n<ul>\n<li>",
					   "<a href = #1>a</a></li>\n<ul>\n<li><a href = #2>",
					   "b</a></li>\n<ul>\n<li><a href = #3>c</a></li>\n</ul>",
					   "\n</ul>\n<li><a href = #4>d</a></li>\n<ul>\n<ul>\n<li>",
					   "<a href = #5>e</a></li>\n</ul>\n</ul>\n</div>")
	expect_equal(output, expected)
})

test_that("Testing create_header_index method, menu mode", {
	plotter <- htmlReport$new()
	plotter$index_items <- index_items
	plotter$index_type <- "menu"
	output <- plotter$create_header_index()
	expected <- paste0("\n<div id=\"floating-menu\">\n<ul>\n<li><a href = #1>",
					   "a</a></li>\n<li><a href = #4>d</a></li>\n</ul>",
					   "\n</div>")
	expect_equal(output, expected)
})

test_that("Testing simple heatmap", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"600px\" aspectRatio='1:1' responsive='true'><",
							  "/canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
		" {\"y\":{\"vars\":[\"V1\",\"V2\",\"V3\"],\"smps\":[\"V1\",\"V2\",\"V3",
		"\",\"V4\",\"V5\",\"V6\"],\"data\":[[0,1,2,3,4,5],[10,11,12,13,14,15],",
		"[20,21,22,23,24,25]]},\"x\":[],\"z\":[]};\nvar conf = {\"toolbarType",
		"\":\"under\",\"xAxisTitle\":\"x_axis\",\"title\":\"Title\",\"",
		"objectColorTransparency\":1,\"theme\":\"cx\",\"colorScheme\":\"",
		"CanvasXpress\",\"graphType\":\"Heatmap\"};\nvar events = false;\nvar",
		" info = false;\nvar afterRender = [];\nvar Cobj_0_ = new CanvasXpress",
		"(\"obj_0_\", data, conf, events, info, afterRender);\n});\n")
	df <- data.frame(V1 = 0:5, V2 = 10:15, V3 = 20:25)
	container <- list(test_data_frame = df)
	options <- list(id = "test_data_frame", header = FALSE, text = FALSE,
                         row_names = FALSE)
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$heatmap(list(id = "test_data_frame", text = FALSE,
									header = FALSE, row_names = FALSE))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("Testing double heatmap", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\" height=",
							  "\"600px\" aspectRatio='1:1' responsive='true'><",
							  "/canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
		" {\"y\":{\"vars\":[\"V1\",\"V2\",\"V3\"],\"smps\":[\"V1\",\"V2\",\"V3",
		"\",\"V4\",\"V5\",\"V6\"],\"data\":[[0,1,2,3,4,5],[10,11,12,13,14,15],",
		"[20,21,22,23,24,25]],\"data2\":[[0,1,1.4142,1.7321,2,2.2361],[3.1623,",
		"3.3166,3.4641,3.6056,3.7417,3.873],[4.4721,4.5826,4.6904,4.7958,4.899",
		",5]]},\"x\":[],\"z\":[]};\nvar conf = {\"toolbarType\":\"under\",\"",
		"xAxisTitle\":\"x_axis\",\"title\":\"Title\",\"objectColorTransparency",
		"\":1,\"theme\":\"cx\",\"colorScheme\":\"CanvasXpress\",\"graphType\":",
		"\"Heatmap\",\"guidesShow\":true,\"heatmapIndicatorPosition\":\"top\",",
		"\"sizeBy\":\"Size\",\"sizeByData\":\"data2\"};\nvar events = false;\n",
		"var info = false;\nvar afterRender = [];\nvar Cobj_0_ = new ",
		"CanvasXpress(\"obj_0_\", data, conf, events, info, afterRender);\n});",
		"\n")
	df <- data.frame(V1 = 0:5, V2 = 10:15, V3 = 20:25)
	df2 <- sqrt(df)
	container <- list(test_data_frame = df, second_data_frame = df2)
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output_string <- plotter$heatmap(list(id = "test_data_frame", text = FALSE,
									header = FALSE, row_names = FALSE,
									extra_data = list(id ="second_data_frame",
										header = FALSE, row_names = FALSE,
										text = "dynamic")))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("Test for scatter3D method", {
	expected_string <- paste0("<canvas  id=\"obj_0_\" width=\"600px\"",
							  " height=\"600px\" aspectRatio='1:1' ",
							  "responsive='true'></canvas>")
	expected_dynamic_js <- paste0("$(document).ready(function () {\nvar data =",
								  " {\"y\":{\"vars\":[\"30\",\"40\"],\"smps\":",
								  "[\"2\",\"25\",\"40\",\"30\"],\"data\":[[50,",
								  "35,20,10],[15,50,10,5]]},\"x\":[],\"z\":{\"",
								  "A\":[\"B\",\"A\"]}};\nvar conf = {\"toolbar",
								  "Type\":\"under\",\"xAxisTitle\":\"Num genes",
								  "\",\"title\":\"test_scatter3d_plot\",\"obje",
								  "ctColorTransparency\":1,\"theme\":\"cx\",\"",
								  "colorScheme\":\"CanvasXpress\",\"graphType",
								  "\":\"Scatter3D\",\"xAxis\":\"liver\",\"yAx",
								  "is\":\"brain\",\"zAxis\":\"lung\",\"yAxisT",
								  "itle\":\"FPKM\",\"zAxisTitle\":\"3rdMetric",
								  "\",\"sizeBy\":\"spleen\",\"colorBy\":\"kidn",
								  "ey\",\"shapeBy\":\"pathway\"};\nvar events ",
								  "= false;\nvar info = false;\nvar afterRende",
								  "r = [];\nvar Cobj_0_ = new CanvasXpress(\"o",
								  "bj_0_\", data, conf, events, info, afterRen",
								  "der);\n});\n")
	df <- data.frame(liver = c(20, 30, 40),
					 brain = c(2, 50, 15),
					 lung = c(25, 35, 50),
					 kidney = c(40, 20, 10),
					 spleen = c(30, 10, 5),
					 pathway = c("A", "B", "A"))
	rownames(df) <- c("gen1", "gen2", "gen3")
	plotter <- htmlReport$new(container = list(test_data_frame = df),
							  compress = FALSE)
	output_string <- plotter$scatter3D(list(id = 'test_data_frame',
		header = TRUE, row_names = TRUE, x_label = 'Num genes',
		title = "test_scatter3d_plot", y_label = 'FPKM', z_label= "3rdMetric",
		smp_attr = 6, xAxis = "liver", yAxis = "brain", zAxis = "lung",
		pointSize = "spleen", colorScaleBy = "kidney", shapeBy = "pathway",
		text = "dynamic"))
	output_dynamic_js <- plotter$dynamic_js
	expect_equal(output_string, expected_string)
	expect_equal(output_dynamic_js, expected_dynamic_js)
	expect_true(plotter$features$canvasXpress)
})

test_that("merge_hashed_tables, rbind join, returned table", {
	container <- list(thisone = data.frame(val = 1),
					  thisonetoo = data.frame(val = 0))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output <- plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"))
	expected <- data.frame(val = 1:0)
	expect_equal(output, expected)
})

test_that("merge_hashed_tables, rbind join, add origin names", {
	container <- list(thisone = data.frame(val = 1),
					  thisonetoo = data.frame(val = 0))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output <- plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"),
									      from_id_name = "origin") 
	expected <- data.frame(val = 1:0, origin = c("thisone", "thisonetoo"))
	expect_equal(output, expected)
})

test_that("merge_hashed_tables, rbind join, rename origin names", {
	container <- list(thisone = data.frame(val = 1),
					  thisonetoo = data.frame(val = 0))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output <- plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"),
									      from_id_name = "renamed_origin",
									      alt_ids = c("hello", "world")) 
	expected <- data.frame(val = 1:0, renamed_origin = c("hello", "world"))
	expect_equal(output, expected)
})

test_that("merge_hashed_tables, rbind join, output to hash_vars", {
	container <- list(thisone = data.frame(val = 1),
					  thisonetoo = data.frame(val = 0))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"),
							    target_id = "target")
	expected <- data.frame(val = 1:0)
	expect_equal(plotter$hash_vars$target, expected)
})

test_that("merge_hashed_tables, cbind join, ignores rbind arguments", {
	container <- list(thisone = data.frame(val = 1),
					  thisonetoo = data.frame(type = "A"))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"),
								join_method = "cbind", target_id = "target",
								from_id_name = "this_should_not_exist",
								alt_ids = "this neither")
	expected <- data.frame(val = 1, type = "A")
	expect_equal(plotter$hash_vars$target, expected)
})

test_that("merge_hashed_tables, rbind join, returned table, add_colnames TRUE", {
	container <- list(thisone = data.frame(V1 = c("val", 1)),
					  thisonetoo = data.frame(V1 = c("val", 0)))
	plotter <- htmlReport$new(container = container, compress = FALSE)
	output <- plotter$merge_hashed_tables(ids = c("thisone", "thisonetoo"),
										  add_colnames = TRUE)
	expected <- data.frame(val = as.character(1:0))
	expect_equal(output, expected)
})

test_that("test get_col_n_row_span, no span case", {
	no_span_table <- data.frame(c(3, 9, 3),
								c(4, 5, 6))
	exp_colspan <- exp_rowspan <- data.frame(matrix(1, ncol = 2, nrow = 3))
	expected_list <- list(rowspan = exp_rowspan, colspan = exp_colspan)
	plotter <- htmlReport$new()
	output <- plotter$get_col_n_row_span(no_span_table)
	expect_equal(output, expected_list)
})

test_that("test get_col_n_row_span, table and row spans", {
	span_table <- data.frame(c("title", "sample", "nerv", "rowspan"),
							 c("colspan", "colspan", "brain", "cerebellum"),
							 c("colspan", "expression", 3, 4))
	colnames(span_table) <- paste0("X", seq(ncol(span_table)))
	exp_colspan <- exp_rowspan <- data.frame(matrix(1, ncol = 3, nrow = 4))
	exp_rowspan[c(1:2, 4), ] <- 1
	exp_rowspan[3, ] <- c(2, 1, 1)
	exp_colspan[1, ] <- c(3, 1, 1)
	exp_colspan[2, ] <- c(2, 1, 1)
	exp_colspan[3:4, ] <- 1
	expected_list <- list(rowspan = exp_rowspan, colspan = exp_colspan)
	plotter <- htmlReport$new()
	output <- plotter$get_col_n_row_span(span_table)
	expect_equal(output, expected_list)
})
