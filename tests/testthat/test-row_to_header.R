test_that("Works in typical cases", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  expected_res_1 <- data.frame(Animal="Fish", Item="Table")
  expected_res_2 <- data.frame(Fish="Animal", Table="Item")
  expect_equal(row_to_header(df), expected_res_1)
  expect_equal(row_to_header(df, 2), expected_res_2)
})

test_that("Keeps rownames when update_rows is FALSE", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  rownames(df) <- c("Header", "Data")
  expected_res <- data.frame(Animal="Fish", Item="Table")
  rownames(expected_res) <- "Data"
  expect_equal(row_to_header(df, 1, FALSE), expected_res)
})

test_that("Updates rownames when update_rows is TRUE (default)", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  rownames(df) <- c("Header", "Data")
  expected_res <- data.frame(Animal="Fish", Item="Table")
  expect_equal(row_to_header(df), expected_res)
})

test_that("Out of bounds row returns error", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  expect_error(row_to_header(df, 3), "row is out of bounds")
  expect_error(row_to_header(df, 0), "row is out of bounds")
  })