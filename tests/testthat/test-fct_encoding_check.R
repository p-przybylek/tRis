test_that("validate encoding", {
  expect_false(encoding_check(NULL))
  df1 <- data.frame(x = c("dolno\x9cl\xb9skie", "Ca\xb3y kraj"), y = 1:2, z = c("boles\xb3awiecki", "Ca\xb3y kraj"))
  expect_false(encoding_check(df1))
  df2 <- data.frame(x = c("dolnośląskie", "Cały kraj"), y = 1:2, z = c("bolesławiecki", "Cały kraj"))
  expect_true(encoding_check(df2))
  df3 <- data.frame(x = c("dolnośląskie", "Ca\xb3y kraj"), y = 1:2, z = c("bolesławiecki", "Cały kraj"))
  expect_false(encoding_check(df3))
})