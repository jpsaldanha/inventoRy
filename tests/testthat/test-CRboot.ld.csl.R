test_that("CRboot.ld.csl() returns the correct ROP/SS for the corresponding P1 service level",
{
  expect_equal(CRboot.ld.csl(rnorm(24,5,1),rnorm(24,20,5),0.9,seed = 1971), 117.962370630148)
  expect_equal(CRboot.ld.csl(rnorm(24,5,1),rnorm(24,20,5),0.9,roptru=FALSE,seed = 1971),
               22.2682410762118)
})
