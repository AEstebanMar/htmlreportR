test_that("Works in most basic case", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  expected_res <- data.frame(Animal="Fish", Item="Table")
  expect_equal(row_to_header(df), expected_res)
})

test_that("Also works with second row", {
  df <- data.frame(c("Animal", "Fish"), c("Item", "Table"))
  expected_res <- data.frame(Fish="Animal", Table="Item")
  expect_equal(row_to_header(df, 2), expected_res)
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