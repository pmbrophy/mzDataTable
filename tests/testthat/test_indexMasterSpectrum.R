context("IndexMasterSpectrum")

test_that("Internal .normCentroid functions work", {
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  dt <- mzML2dataTable(path = dataPath)

  mzGrid_R <- .normCentroidMz(mz = dt$mz, ppm = 10)
  mzGrid_C <- .C_normCentroidMz(mz = dt$mz, ppm = 10)
  expect_true(identical(mzGrid_C, mzGrid_R))

  mzGrid_R <- .normCentroidMz(mz = dt$mz, ppm = 25)
  mzGrid_C <- .C_normCentroidMz(mz = dt$mz, ppm = 25)
  expect_true(identical(mzGrid_C, mzGrid_R))

  mzGrid_R <- .normCentroidMz(mz = dt$mz, ppm = 100)
  mzGrid_C <- .C_normCentroidMz(mz = dt$mz, ppm = 100)
  expect_true(identical(mzGrid_C, mzGrid_R))

  mzGrid_R <- .normCentroidMz(mz = dt$mz, ppm = 1000)
  mzGrid_C <- .C_normCentroidMz(mz = dt$mz, ppm = 1000)
  expect_true(identical(mzGrid_C, mzGrid_R))
})


test_that("Internal .normProfileMz function works", {
  dataPath <- system.file("extdata", "example.mzML", package="mzDataTable", mustWork = TRUE)
  expect_true(file.exists(dataPath))
  dt <- mzML2dataTable(path = dataPath)

  mzGrid <- .normProfileMz(mz = dt$mz)

  expect_equal(nrow(mzGrid), 50)
  expect_equal(ncol(mzGrid), 2)
})
