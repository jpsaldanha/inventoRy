test_that("CRboot.x.csl() returns the correct ROP/SS for the corresponding P1 service level",
{
  expect_equal(CRboot.x.csl(rnorm(30,100,20),0.9,seed = 1971), 118.456600066359)
  expect_equal(CRboot.x.csl(rnorm(30,100,20),0.9,roptru=FALSE,seed = 1971),
               20.1144841652193)
})
