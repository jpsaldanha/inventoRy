test_that("CRgamma.x.csl() returns the correct ROP/SS for the corresponding P1 service level"
          ,
{
  expect_equal(CRgamma.x.csl(c(32.53778,26.32383,23.69610,25.12208,24.14786,26.26359,21.21864,
                                29.67055,22.44453,31.73911),0.9), 31.3539755485382)
  expect_equal(CRgamma.x.csl(c(32.53778,26.32383,23.69610,25.12208,24.14786,26.26359,21.21864,
                                29.67055,22.44453,31.73911),0.9,FALSE), 5.03756854853822)
})
