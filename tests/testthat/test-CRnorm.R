test_that("CRnorm.x.fr() returns the correct ROP/SS for the corresponding P2 fill rate"
          ,
{
  expect_equal(CRboot.x.fr(rnorm(30,100,20),50,.95,500,TRUE,1971), 108.49446191153)
  expect_equal(CRboot.x.fr(rnorm(30,100,20),50,.95,500,FALSE,1971),
               10.1523460103895)
})
