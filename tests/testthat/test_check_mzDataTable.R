context("check_mzDataTable")

test_that("check_mzDataTable catches missformed data.tables", {
  seqNum <- NULL
  #Not a data.frame
  df <- data.frame()
  expect_error(.check_mzDataTable(df))

  #Missing column
  dt <- data.table::data.table(seqNum = c(1:10),
                               mz = c(1:10),
                               acquisitionNum = c(1:10),
                               retentionTime = c(1:10))
  expect_error(.check_mzDataTable(dt))

  #No Data
  dt <- data.table::data.table(seqNum = c(1:10),
                               mz = c(1:10),
                               intensity = 100,
                               acquisitionNum = c(1:10),
                               retentionTime = c(1:10))
  expect_error(.check_mzDataTable(dt[seqNum > 10]))
})
