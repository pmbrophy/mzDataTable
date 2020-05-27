context(".splitIndex")

test_that(".splitIndex correctly splits a vector of index values without randomization",{
  i <- c(c(1:10), c(31:40))

  splits <- .splitIndex(index = i, nGroups = 2, randomize = FALSE)

  ordered1 <- sapply(X = splits[[1]], FUN = is.unsorted)
  ordered2 <- sapply(X = splits[[1]], FUN = is.unsorted)

  expect_named(splits)
  expect_type(splits, "list")
  expect_equal(length(splits), 2)

  expect_false(any(ordered1))
  expect_false(any(ordered2))
})

test_that(".splitIndex correctly splits a vector of index values with randomization",{
  i <- c(c(1:10), c(31:40))

  splits <- .splitIndex(index = i, nGroups = 2, randomize = TRUE)

  expect_named(splits)
  expect_type(splits, "list")
  expect_equal(length(splits), 2)
})
