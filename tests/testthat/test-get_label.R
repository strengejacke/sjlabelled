test_that("get_label.default returns label attribute", {
  x <- 1:5
  attr(x, "label") <- "My Variable"
  expect_equal(get_label(x), "My Variable")
})

test_that("get_label.default returns def.value when no label", {
  x <- 1:5
  expect_null(get_label(x))
  expect_equal(get_label(x, def.value = "fallback"), "fallback")
})

test_that("get_label.data.frame returns labels for all columns", {
  df <- data.frame(a = 1:3, b = 4:6)
  attr(df$a, "label") <- "Variable A"
  attr(df$b, "label") <- "Variable B"
  result <- get_label(df)
  expect_equal(result[["a"]], "Variable A")
  expect_equal(result[["b"]], "Variable B")
})

test_that("get_label.data.frame uses def.value for unlabelled columns", {
  df <- data.frame(a = 1:3, b = 4:6, c = 7:9)
  attr(df$a, "label") <- "Variable A"
  # b and c have no labels
  result <- get_label(df, def.value = c("col_a", "col_b", "col_c"))
  expect_equal(result[["a"]], "Variable A")
  expect_equal(result[["b"]], "col_b")
  expect_equal(result[["c"]], "col_c")
})

test_that("get_label.data.frame uses scalar def.value for all unlabelled", {
  df <- data.frame(a = 1:3, b = 4:6)
  result <- get_label(df, def.value = "none")
  expect_equal(result[["a"]], "none")
  expect_equal(result[["b"]], "none")
})

test_that("get_label.list returns labels for labelled elements", {
  x <- list(1:3, 4:6)
  attr(x[[1]], "label") <- "First"
  attr(x[[2]], "label") <- "Second"
  result <- get_label(x)
  expect_equal(result[[1]], "First")
  expect_equal(result[[2]], "Second")
})

test_that("get_label.list returns empty string for unlabelled elements", {
  x <- list(1:3, 4:6)
  attr(x[[1]], "label") <- "First"
  # second element has no label
  result <- get_label(x)
  expect_equal(result[[1]], "First")
  expect_equal(result[[2]], "")
})

test_that("get_label.list uses def.value for unlabelled elements", {
  x <- list(1:3, 4:6, 7:9)
  attr(x[[1]], "label") <- "First"
  # second and third elements have no label
  result <- get_label(x, def.value = c("one", "two", "three"))
  expect_equal(result[[1]], "First")
  expect_equal(result[[2]], "two")
  expect_equal(result[[3]], "three")
})

test_that("get_label.list uses scalar def.value for all unlabelled", {
  x <- list(1:3, 4:6)
  result <- get_label(x, def.value = "fallback")
  expect_equal(result[[1]], "fallback")
  expect_equal(result[[2]], "fallback")
})

test_that("get_label.list recycles def.value when shorter than list", {
  x <- list(1:3, 4:6, 7:9)
  result <- get_label(x, def.value = "default")
  expect_equal(result[[1]], "default")
  expect_equal(result[[2]], "default")
  expect_equal(result[[3]], "default")
})
