context("getTIC")

test_that("getTIC correctly handles a data.table and disk.frame", {
  #Import data
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(dataPath)
  dt <- mzML2dataTable(path = dataPath)
  dskF <- disk.frame::as.disk.frame(df = dt, nchunks = 1)

  #dt: Not Normalized
  tic <- getTIC(mzObj = dt, normalize = FALSE)
  expect_true(data.table::is.data.table(tic))
  expect_equal(nrow(tic), 5)
  expect_equal(ncol(tic), 3)

  #dskF: Not Normalized
  tic_dskF <- getTIC(mzObj = dskF, normalize = FALSE)
  expect_true(data.table::is.data.table(tic_dskF))
  expect_equal(nrow(tic_dskF), 5)
  expect_equal(ncol(tic_dskF), 3)
  expect_identical(tic, tic_dskF)

  #dt:Normalized
  tic <- getTIC(mzObj = dt, normalize = TRUE)
  expect_true(data.table::is.data.table(tic))
  expect_equal(nrow(tic), 5)
  expect_equal(ncol(tic), 3)
  expect_equal(max(tic$intensity), 1)

  #dskF: Not Normalized
  tic_dskF <- getTIC(mzObj = dskF, normalize = TRUE)
  expect_true(data.table::is.data.table(tic_dskF))
  expect_equal(nrow(tic_dskF), 5)
  expect_equal(ncol(tic_dskF), 3)
  expect_identical(tic, tic_dskF)

  #Cleanup
  disk.frame::delete(dskF)
})

