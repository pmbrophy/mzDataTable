context("getSumSpectrum")

test_that("getSumSpectrum works for data.tables and disk.frames",{
  #Import data
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  mzR_obj <- mzR::openMSfile(dataPath)
  dt <- mzML2dataTable(path = dataPath)
  dskF <- disk.frame::as.disk.frame(df = dt, nchunks = 1)

  ######################################################################################### #
  #                                         CENTROID                                        #
  ######################################################################################### #
  #UnNormalized
  sumSpec <- getSumSpectrum(mzObj = dt, ppmTol = 1000, isCentroid = TRUE, normalize = FALSE)
  expect_true(data.table::is.data.table(sumSpec))
  expect_equal(nrow(sumSpec), 2)
  expect_equal(ncol(sumSpec), 3)

  sumSpec_dskF <- getSumSpectrum(mzObj = dskF, ppmTol = 1000, isCentroid = TRUE, normalize = FALSE)
  expect_identical(sumSpec, sumSpec_dskF)

  #Normalize
  sumSpec <- getSumSpectrum(mzObj = dt, ppmTol = 1000, isCentroid = TRUE, normalize = TRUE)
  expect_true(data.table::is.data.table(sumSpec))
  expect_equal(nrow(sumSpec), 2)
  expect_equal(ncol(sumSpec), 3)
  expect_equal(max(sumSpec$intensity), 1)

  sumSpec_dskF <- getSumSpectrum(mzObj = dskF, ppmTol = 1000, isCentroid = TRUE, normalize = TRUE)
  expect_identical(sumSpec, sumSpec_dskF)

  ######################################################################################### #
  #                                         PROFILE                                         #
  ######################################################################################### #
  #UnNormalized
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE)
  expect_true(data.table::is.data.table(sumSpec))
  expect_equal(nrow(sumSpec), 50)
  expect_equal(ncol(sumSpec), 3)

  sumSpec_dskF <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE)
  expect_identical(sumSpec, sumSpec_dskF)

  #Normalize
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = TRUE)
  expect_true(data.table::is.data.table(sumSpec))
  expect_equal(nrow(sumSpec), 50)
  expect_equal(ncol(sumSpec), 3)
  expect_equal(max(sumSpec$intensity), 1)

  sumSpec_dskF <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = TRUE)
  expect_identical(sumSpec, sumSpec_dskF)

  ######################################################################################### #
  #                                CENTROID - TIME FILTER                                   #
  ######################################################################################### #
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStart = 1, iStop = 10)
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStart = 1)
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStop = 10)

  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, tStart = 1, tStop = 10)
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, tStart = 1)
  sumSpec <- getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, tStop = 10)

  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, iStart = 1, iStop = 10)
  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, iStart = 1)
  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, iStop = 10)

  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, tStart = 1, tStop = 10)
  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, tStart = 1)
  sumSpec <- getSumSpectrum(mzObj = dskF, isCentroid = FALSE, normalize = FALSE, tStop = 10)

  ######################################################################################### #
  #                                      THROW ERRORS                                       #
  ######################################################################################### #
  expect_error(getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStart = 1, iStop = 10, tStop = 10))
  expect_error(getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStart = 1, iStop = 10, tStart = 10))
  expect_error(getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStop = 10, tStart = 10))
  expect_error(getSumSpectrum(mzObj = dt, isCentroid = FALSE, normalize = FALSE, iStop = 10, tStart = 10, tStop = 20))

  disk.frame::delete(dskF)
})
