context("getXIC")

test_that("getXIC correctly handles a data.table and disk.frame", {
  #Import data
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(dataPath)
  dt <- mzML2dataTable(path = dataPath)
  dskF <- disk.frame::as.disk.frame(df = dt, nchunks = 1)

  ######################################################################################### #
  #                                      NO TIME FILTER                                     #
  ######################################################################################### #

  #data.table: mz_delta
  xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1)
  expect_true(data.table::is.data.table(xic))
  expect_equal(nrow(xic), 5)
  expect_equal(ncol(xic), 3)

  #data.table: mz_delta
  xic_diskF <- getXIC(mzObj = dskF, mz = 401, mz_delta = 1)
  expect_true(data.table::is.data.table(xic_diskF))
  expect_equal(nrow(xic_diskF), 5)
  expect_equal(ncol(xic_diskF), 3)
  expect_identical(xic_diskF, xic)

  #data.table: ppmError
  xic <- getXIC(mzObj = dt, mz = 401, ppmTol = 10000)
  expect_true(data.table::is.data.table(xic))
  expect_equal(nrow(xic), 5)
  expect_equal(ncol(xic), 3)

  #data.table: ppmError
  xic_diskF <- getXIC(mzObj = dskF, mz = 401, ppmTol = 10000)
  expect_true(data.table::is.data.table(xic_diskF))
  expect_equal(nrow(xic_diskF), 5)
  expect_equal(ncol(xic_diskF), 3)
  expect_identical(xic_diskF, xic)

  ######################################################################################### #
  #                                      seqNum Filter                                      #
  ######################################################################################### #

  #data.table: Time Filter With seqNum
  xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, iStart = 1, iStop = 3)
  expect_true(data.table::is.data.table(xic))
  expect_equal(nrow(xic), 3)
  expect_equal(ncol(xic), 3)

  #data.table: No Time Filter with seqNum
  xic_diskF <- getXIC(mzObj = dskF, mz = 401, mz_delta = 1, iStart = 1, iStop = 3)
  expect_true(data.table::is.data.table(xic_diskF))
  expect_equal(nrow(xic_diskF), 3)
  expect_equal(ncol(xic_diskF), 3)
  expect_identical(xic_diskF, xic)

  ######################################################################################### #
  #                                    retentionTime Filter                                 #
  ######################################################################################### #

  #data.table: Time Filter With seqNum
  xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, tStop = 1.25)
  expect_true(data.table::is.data.table(xic))
  expect_equal(nrow(xic), 3)
  expect_equal(ncol(xic), 3)

  #data.table: No Time Filter with seqNum
  xic_diskF <- getXIC(mzObj = dskF, mz = 401, mz_delta = 1, tStart = 0.15, tStop = 1.25)
  expect_true(data.table::is.data.table(xic_diskF))
  expect_equal(nrow(xic_diskF), 3)
  expect_equal(ncol(xic_diskF), 3)
  expect_identical(xic_diskF, xic)

  ######################################################################################### #
  #                                        Error Cases                                      #
  ######################################################################################### #
  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, tStop = 1.25, iStart = 1))
  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, tStop = 1.25, iStart = 1, iStop = 10))
  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, tStop = 1.25, iStop = 10))

  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, iStop = 10))
  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStart = 0.15, iStart = 10, iStop = 10))
  expect_error(xic <- getXIC(mzObj = dt, mz = 401, mz_delta = 1, tStop = 0.15, iStart = 10, iStop = 10))

  disk.frame::delete(dskF)
})
