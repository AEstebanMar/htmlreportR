test_that("Embed file works",{
	embed_file_test <- "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAGQAAABkCAMAAABHPGVmAAAAxlBMVEUEBAQFBQUHBwcICAgKCgoMDAwPDw8QEBATExMXFxcYGBgcHBwdHR0eHh4fHx8hISEmJiYpKSkvLy8zMzM8PDxAQEBDQ0NERERGRkZHR0dISEhJSUlKSkpOTk5PT09UVFRXV1dZWVlaWlpbW1tcXFyWlpaYmJiZmZmampqcnJyfn5+hoaGoqKiqqqqrq6usrKytra27u7u8vLy+vr7Ly8vm5ubt7e3u7u7v7+/09PT19fX29vb39/f4+Pj6+vr7+/v8/Pz////jxAlkAAABX0lEQVRoge3a6U6DQBQFYBTc0OJShdZWSle1LS61RStgnfd/KQEzaZwiQzreRs05P0guuelHhgnJzVRjG4gGBAgQIEBKIUdV1ZwdWBWzUohcKD9164mxBysXie/8LKeqRnSZXk9ykfnNdZYdVSRsfo/w7KkizJ0y9lj8TtSRmduoeefECGOvsbh/CBC2skmBAAEChAYJak4WnRLh+fvLBQQIECC5eVuQI4HnOO2AGGknM86kux4S3pabTxZOenVkK/YjyPtaCI90uTqTZMbpybpUX3zHtnvPxEiyYvRbuFyAAAECZCMIhiAgQIAAkSLhnBzZbdWbVzNiRE9mnKlLhPBDmu20aIQ0CD9uMtOiHtMgPNZ9MuN4qoYEOTT3tS3jS4TS0HVD0nBczT+dW2bcL34qNhxKGvpj8TeBAPmMPxBu2EI9GkkaBr4UicVvyotQR5GkIVz5YPzbfxYA+QXIB2tlntFVobcaAAAAAElFTkSuQmCC"
	plotter <- htmlReport$new()
	file <- file.path(plotter$source_folder, "exData", "test.png")
	if (!file.exists(file))
		file <- file.path(plotter$source_folder, "inst", "exData", "test.png")
	embed_file <- embed_file(file)
	expect_equal(embed_file_test, embed_file)
})


