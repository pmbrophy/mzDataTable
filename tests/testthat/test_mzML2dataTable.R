context("mzML2dataTable")

test_that("mzML2dataTable loads data from path", {
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(dataPath)

  #All Scans
  dt <- mzML2dataTable(path = dataPath)
  expect_true(data.table::is.data.table(dt))
  expect_equal(nrow(x = dt), 50)

  #All scans with correct header
  header <- mzR::header(mzR_obj)
  expect_identical(dt, mzML2dataTable(path = dataPath, header = header))

  #Specific scans
  dt <- mzML2dataTable(path = dataPath, scans = c(1,5))
  expect_true(data.table::is.data.table(dt))
  expect_equal(nrow(x = dt), 20)

  #Specific scans with incorrect header
  expect_error(mzML2dataTable(path = dataPath, scans = c(1,5), header = header))

  #All scans with incorrect header
  header <- mzR::header(mzR_obj, scans = c(1,3))
  expect_error(mzML2dataTable(path = dataPath, header = header))
})

test_that("mzML2dataTable loads data from mzR object", {
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(filename = dataPath)

  #All Scans
  dt <- mzML2dataTable(path = mzR_obj)
  expect_true(data.table::is.data.table(dt))
  expect_equal(nrow(x = dt), 50)

  #All scans with correct header
  header <- mzR::header(mzR_obj)
  expect_identical(dt, mzML2dataTable(path = mzR_obj, header = header))

  #Specific scans
  dt <- mzML2dataTable(path = mzR_obj, scans = c(1,5))
  expect_true(data.table::is.data.table(dt))
  expect_equal(nrow(x = dt), 20)

  #Specific scans with incorrect header
  expect_error(mzML2dataTable(path = mzR_obj, scans = c(1,5), header = header))

  #All scans with incorrect header
  header <- mzR::header(mzR_obj, scans = c(1,3))
  expect_error(mzML2dataTable(path = mzR_obj, header = header))
  })
