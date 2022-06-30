test_that("CRboot.ld.fr() returns the correct ROP/SS for the corresponding P2 fill rate",
{
  expect_equal(CRboot.ld.fr(rnorm(24,5,1),rnorm(24,20,5),100,0.95,seed = 1971), 101.450656210558)
  expect_equal(CRboot.ld.fr(rnorm(24,5,1),rnorm(24,20,5),100,0.95,roptru=FALSE,seed = 1971),
               5.75652665662219)
})
