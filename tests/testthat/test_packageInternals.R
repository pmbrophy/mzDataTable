context(".dropEmptyCols")

test_that(".dropEmptyCols drops empty columns and keeps columns with data", {

  df <- data.frame(c1 = c(1:10),
                   c2 = rnorm(n = 10, mean = 1, sd = 1),
                   c3 = rep("a", 10),
                   c4 = rep(NA, 10),
                   c5 = rep(NaN, 10),
                   C6 = c(1:10))

  df_out <- .dropEmptyCols(df = df)

  expect_false(identical(df, df_out))
  expect_equal(ncol(df_out), 4)
  expect_true(is.data.frame(df_out))
  expect_true(is.numeric(df_out[,1]))
  expect_true(is.numeric(df_out[,2]))
  expect_true(is.character(df_out[,3]))
  expect_true(is.numeric(df_out[,4]))
})

context(".getNumFromSpectrumId")

test_that(".getNumFromSpectrumId correctly extracts names and values for a single string", {
  vec <- c("name1=10 name2=2 name3=333")
  nums <- .getNumFromSpectrumId(spectrumId = vec)

  expect_equal(names(nums), c("name1", "name2", "name3"))
  expect_equal(ncol(nums), 3)
  expect_equal(nrow(nums), 1)
  expect_equal(nums[1,1], 10)
  expect_equal(nums[1,2], 2)
  expect_equal(nums[1,3], 333)

})

test_that(".getNumFromSpectrumId correctly extracts names and values for a vector of strings", {
  vec <- c("name1=10 name2=2 name3=333", "name1=11 name2=3 name3=444", "name1=10 name2=2 name3=333")
  nums <- .getNumFromSpectrumId(spectrumId = vec)

  expect_equal(names(nums), c("name1", "name2", "name3"))
  expect_equal(ncol(nums), 3)
  expect_equal(nrow(nums), 3)
  expect_equal(nums[1,1], 10)
  expect_equal(nums[1,2], 2)
  expect_equal(nums[1,3], 333)
})

context(".scanChunker")

test_that(".scanChunker correctly indexs a vector of index values", {
  chunks <- .scanChunker(scans = c(1:10), chunkSize = 2)

  expect_type(chunks, "list")
  expect_length(chunks, 5)
})







