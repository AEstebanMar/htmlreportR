test_that("Embed file works",{
	embed_file_test <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAMAAABHPGVmAAAAxlBMVEUEBAQFBQUHBwcICAgKCgoMDAwPDw8QEBATExMXFxcYGBgcHBwdHR0eHh4fHx8hISEmJiYpKSkvLy8zMzM8PDxAQEBDQ0NERERGRkZHR0dISEhJSUlKSkpOTk5PT09UVFRXV1dZWVlaWlpbW1tcXFyWlpaYmJiZmZmampqcnJyfn5+hoaGoqKiqqqqrq6usrKytra27u7u8vLy+vr7Ly8vm5ubt7e3u7u7v7+/09PT19fX29vb39/f4+Pj6+vr7+/v8/Pz////jxAlkAAABX0lEQVRoge3a6U6DQBQFYBTc0OJShdZWSle1LS61RStgnfd/KQEzaZwiQzreRs05P0guuelHhgnJzVRjG4gGBAgQIEBKIUdV1ZwdWBWzUohcKD9164mxBysXie/8LKeqRnSZXk9ykfnNdZYdVSRsfo/w7KkizJ0y9lj8TtSRmduoeefECGOvsbh/CBC2skmBAAEChAYJak4WnRLh+fvLBQQIECC5eVuQI4HnOO2AGGknM86kux4S3pabTxZOenVkK/YjyPtaCI90uTqTZMbpybpUX3zHtnvPxEiyYvRbuFyAAAECZCMIhiAgQIAAkSLhnBzZbdWbVzNiRE9mnKlLhPBDmu20aIQ0CD9uMtOiHtMgPNZ9MuN4qoYEOTT3tS3jS4TS0HVD0nBczT+dW2bcL34qNhxKGvpj8TeBAPmMPxBu2EI9GkkaBr4UicVvyotQR5GkIVz5YPzbfxYA+QXIB2tlntFVobcaAAAAAElFTkSuQmCC"
	plotter <- htmlReport$new()
	file <- file.path(plotter$source_folder, "exData", "test.png")
	if (!file.exists(file))
		file <- file.path(plotter$source_folder, "inst", "exData", "test.png")
	embed_file <- embed_file(file)
	expect_equal(embed_file_test, embed_file)
})

test_that("Test replace_paired_mark, simple case", {
	string <- "** This should be in bold **"
	pattern <- "(\\*\\*+?)([- \\w]+)(\\*\\*+?)"
	expected_text <- "<strong> This should be in bold </strong>"
	output_text <- replace_paired_mark(string, pattern,
									   c("<strong>", "</strong>"))
	expect_equal(output_text, expected_text)
	})

test_that("Test replace_paired_mark, worst case scenario", {
	string <- paste0("### ***This is a worst-case scenario*** with some text in",
				   " between ***And this another worst case scenario***")
	expected_text <- paste0("### <strong><em>This is a worst-case scenario",
							"</em></strong> with some text in between",
							" <strong><em>And this another worst case",
							" scenario</em></strong>")
	pattern <- "(\\*\\*\\*+?)([- \\w]+)(\\*\\*\\*+?)"
	replace <- c("<strong><em>", "</em></strong>")
	output_text <- replace_paired_mark(string, pattern, replace)
	expect_equal(output_text, expected_text)
	})

test_that("Test replace_paired_mark, title inside cat call", {
	string <- 'cat("### Title me this")'
	detected <- grepl("cat\\(\"\\n*#+", string)
	expect_true(detected)
	expected_text <- paste0('cat("<h3> Title me this</h3>")')
	text <- stringr::str_match(string, "(#+)(.*)")
	level <- nchar(text[2])
	replace_1 <- paste0("<h", level, ">")
	replace_2 <- paste0("</h", level, ">\")")
	output_text <- replace_paired_mark(string = string,
									   pattern = "(\\n*#+)(.*)(\\n*\"\\))",
									   replace = c(replace_1, replace_2))
	expect_equal(output_text, expected_text)
	})

test_that("Test replace_paired_mark, title inside cat call with newlines", {
	string <- 'cat("\\n### Title me this\\n")'
	detected <- grepl("cat\\(\"\\\\n*#+", string)
	expect_true(detected)
	expected_text <- paste0('cat("<h3> Title me this</h3>")')
	text <- stringr::str_match(string, "(#+)(.*)")
	level <- nchar(text[2])
	replace_1 <- paste0("<h", level, ">")
	replace_2 <- paste0("</h", level, ">\")")
	output_text <- replace_paired_mark(string = string,
									   pattern = "(\\\\n*#+)(.*)(\\\\n*\"\\))",
									   replace = c(replace_1, replace_2))
	expect_equal(output_text, expected_text)
	})

test_that("Check_numeric_fields simple case", {
	input_df <- data.frame(1:4, letters[1:4], toupper(letters[1:4]))
	output <- check_numeric_fields(input_df)
	expect_equal(output, c(TRUE, FALSE, FALSE))
})

test_that("Check_numeric_fields with mixed vectors", {
	input_df <- data.frame(letters[1:4], toupper(letters[1:4]), 1:4)
	input_df[1, ] <- 1:3
	output <- check_numeric_fields(input_df)
	expect_equal(output, c(FALSE, FALSE, TRUE))
})
