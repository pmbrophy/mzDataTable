context("mzML2csv")

test_that("mzML2csv converts to csv correctly with default parameters",{
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  csvTestPath <- paste0(getwd(), "/test.csv")

  #Test with default values
  returnValue <- mzML2csv(path = dataPath, csvPath = csvTestPath)

  #Read CSV
  expect_true(file.exists(csvTestPath))
  csv <- data.table::fread(csvTestPath)

  #Remove CSV
  unlink(csvTestPath)

  #Check CSV
  expect_equal(nrow(csv), 50)
})

test_that("mzML2csv converts to csv correctly with specific scans",{
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  csvTestPath <- paste0(getwd(), "/test.csv")

  #Test with default values
  returnValue <- mzML2csv(path = dataPath, csvPath = csvTestPath, scans = c(1,5))

  #Read CSV
  expect_true(file.exists(csvTestPath))
  csv <- data.table::fread(csvTestPath)

  #Remove CSV
  unlink(csvTestPath)

  #Check CSV
  expect_equal(nrow(csv), 20)
  expect_equal(unique(csv$seqNum), c(1,5))
})

test_that("mzML2csv converts to csv correctly with specific scans and chunks",{
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))

  csvTestPath <- paste0(getwd(), "/test.csv")

  #Test with default values
  returnValue <- mzML2csv(path = dataPath, csvPath = csvTestPath, scans = c(1,3,5), chunkSize = 1)

  #Read CSV
  expect_true(file.exists(csvTestPath))
  csv <- data.table::fread(csvTestPath)

  #Remove CSV
  unlink(csvTestPath)

  #Check CSV
  expect_equal(nrow(csv), 30)
  expect_equal(unique(csv$seqNum), c(1,3,5))
})
