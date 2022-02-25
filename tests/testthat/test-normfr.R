test_that("normfr() returns the correct ROP for the corresponding P2 fill rate", {
  expect_equal(normfr(50,0.95,100,120), 297.52252)
  expect_equal(normfr(50,0.85,100,120), 200.272491)
})
