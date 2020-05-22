context("mzML2diskFrame")

test_that("mzML2diskFrame converts to disk.frame correctly with default parameters",{
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  testPath <- paste0(getwd(), "/test.df")

  #Test with default values
  diskF <- mzML2diskFrame(path = dataPath, diskFramePath =  testPath)

  #Read CSV
  expect_true(dir.exists(testPath))
  df <- disk.frame::get_chunk(diskF, 1)

  #Remove CSV
  unlink(testPath, recursive = TRUE)

  #Check CSV
  expect_equal(nrow(df), 50)
})

test_that("mzML2diskFrame converts to disk.frame correctly with multiple chunks",{
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  testPath <- paste0(getwd(), "/test.df")

  #Test with default values
  diskF <- mzML2diskFrame(path = dataPath, diskFramePath =  testPath, chunkSize = 2)

  #Read CSV
  expect_true(dir.exists(testPath))

  expect_equal(length(list.files(testPath, pattern = "*.fst")), 3)

  df <- disk.frame::get_chunk(diskF, 3)

  #Remove CSV
  unlink(testPath, recursive = TRUE)

  #Check CSV
  expect_equal(nrow(df), 30)
})
