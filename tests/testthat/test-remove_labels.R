test_that("remove_labels", {
  skip_if_not_installed("haven")
  z <- factor(LETTERS[3:1], ordered = TRUE)
  z <- sjlabelled::set_labels(z, labels = c("yes", "maybe", "no"))
  x <- sjlabelled::remove_labels(z, labels = 2)

  expect_equal(attributes(x)$labels, c(yes = "A", no = "C"))
})
