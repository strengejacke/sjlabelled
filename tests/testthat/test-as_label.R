test_that("as_character handles unused labels correctly (issue #65)", {
  # Create labelled vector with unused label (value 0 not present)
  val <- rep(c(1:2), each = 3)
  fct <- set_labels(val, labels = c("Zero" = 0, "One" = 1, "Two" = 2))
  fct <- as_factor(fct)

  # as_character should map values to correct labels
  chr <- as_character(fct)
  expect_equal(as.character(chr), rep(c("One", "Two"), each = 3))

  # as_label should also work correctly
  lab <- as_label(fct)
  expect_equal(as.character(lab), rep(c("One", "Two"), each = 3))
})

test_that("as_character works when all labels are present", {
  val <- rep(c(0:2), each = 2)
  fct <- set_labels(val, labels = c("Zero" = 0, "One" = 1, "Two" = 2))
  fct <- as_factor(fct)

  chr <- as_character(fct)
  expect_equal(as.character(chr), rep(c("Zero", "One", "Two"), each = 2))
})

test_that("as_character works with no unused labels", {
  val <- rep(c(1:2), each = 3)
  fct <- set_labels(val, labels = c("One" = 1, "Two" = 2))
  fct <- as_factor(fct)

  chr <- as_character(fct)
  expect_equal(as.character(chr), rep(c("One", "Two"), each = 3))
})
