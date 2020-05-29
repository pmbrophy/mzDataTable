context("getBPI")

test_that("getBPI correctly handles a data.table and disk.frame", {
  #Import data
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(dataPath)
  dt <- mzML2dataTable(path = dataPath)
  dskF <- disk.frame::as.disk.frame(df = dt, nchunks = 1)

  #dt: Not Normalized
  bpi <- getBPI(mzObj = dt, normalize = FALSE)
  expect_true(data.table::is.data.table(bpi))
  expect_equal(nrow(bpi), 5)
  expect_equal(ncol(bpi), 4)

  #dskF: Not Normalized
  bpi_dskF <- getBPI(mzObj = dskF, normalize = FALSE)
  expect_true(data.table::is.data.table(bpi_dskF))
  expect_equal(nrow(bpi_dskF), 5)
  expect_equal(ncol(bpi_dskF), 4)
  expect_identical(bpi, bpi_dskF)

  #dt:Normalized
  bpi <- getBPI(mzObj = dt, normalize = TRUE)
  expect_true(data.table::is.data.table(bpi))
  expect_equal(nrow(bpi), 5)
  expect_equal(ncol(bpi), 4)
  expect_equal(max(bpi$intensity), 1)

  #dskF: Not Normalized
  bpi_dskF <- getBPI(mzObj = dskF, normalize = TRUE)
  expect_true(data.table::is.data.table(bpi_dskF))
  expect_equal(nrow(bpi_dskF), 5)
  expect_equal(ncol(bpi_dskF), 4)
  expect_identical(bpi, bpi_dskF)

  #Cleanup
  disk.frame::delete(dskF)
})

