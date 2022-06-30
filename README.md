
<!-- README.md is generated from README.Rmd. Please edit that file -->

# inventoRy (WORK IN PROGRESS)

<!-- badges: start -->
<!-- badges: end -->

The inventoRy package offers the conventional textbook expressions and
nonparametric bootstrap approaches for estimating inventory policy
decisions such as the reorder point and safety stocks for single item
inventories under the continuous review inventory policy using either
the cycle service level or fill rate targets.

<!--The goal of inventoRy is to assist in estimate inventory parameters -- reorder point (ROP) and safety stock (ss) -- for the fill rate and service level customer service criterion under the continuous review inventory policy.
The primary purpose of this package is to provide non-paramteric bootstrap estimation functions for cases when the lead time demand (LTD) distribution takes on non-standard forms. Functions for standard forms are also provided including for uniform, lognormal, truncated-normal, etc. as an accompaniment of the Saldanha (2022) article.
-->

Saldanha, J.P., 2022. Estimating the reorder point for a fill-rate
target under a continuous review policy in the presence of non-standard
lead-time demand distributions. Transportation Research Part E:
Logistics and Transportation Review, doi.org/10.1016/j.tre.2022.102766.

# Installation

<!-- You can install the released version of inventoRy from [CRAN](https://CRAN.R-project.org) with: -->
<!-- ``` r -->
<!-- install.packages("inventoRy") -->
<!-- ``` -->

### install.packages(“devtools”)

devtools::install_github(“jpsaldanha/inventoRy”)

![\\mu](https://latex.codecogs.com/png.image?%5Cdpi%7B110%7D&space;%5Cbg_white&space;%5Cmu "\mu")

<!-- ## Example -->
<!-- This is a basic example which shows you how to solve a common problem: -->
<!-- ```{r example} -->
<!-- library(inventoRy) -->
<!-- ``` -->
<!-- What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so: -->
<!-- ```{r cars} -->
<!-- summary(cars) -->
<!-- ``` -->
<!-- You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/master/examples>. -->
<!-- You can also embed plots, for example: -->
<!-- ```{r pressure, echo = FALSE} -->
<!-- plot(pressure) -->
<!-- ``` -->
<!-- In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN. -->
