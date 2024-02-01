test_that("no error when specified columns exist", {
  test <- data.frame(sample=c("P1", "C1", "P2", "C2"),
                     gene=c("MMUT2", "ASS2", "GALE", "MMUT2"),
                     logpval=1:4)
  p <- gg_heatmap(input_table=test, x_axis="sample", y_axis="gene", fill="logpval")
  expect_no_error(print(p))
})
