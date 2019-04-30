
<!-- README.md is generated from README.Rmd. Please edit that file -->
STAT 585 Final Project: statsfoRstudents package/shiny app
----------------------------------------------------------

[![Coverage status](https://codecov.io/gh/jzemmels/finalProject/branch/master/graph/badge.svg)](https://codecov.io/github/jzemmels/finalProject?branch=master)

``` r
devtools::install_github("https://github.com/jzemmels/statsfoRstudents")
```

The function of this package is mainly in-service to the shiny app that accompanies it. To run this shiny app, run the following code:

``` r
shiny::runApp(appDir = "statsfoRstudents/inst/shiny")
```

Gulzina and Eryn have functions in the R folder that haven't yet been implemented into a Shiny app, so you can give them feedback based on those.

#### TODO:

-   add more information to .Rmd about function of package/app
-   presentation slides
-   vignette
-   (maybe) add "Help" button to each tab of the shiny app that links to the correct section of the vignette (should just be a renderUI)
-   (Joe) add some "questions to think about" to the Normal Plot and Sampling Distribution of Sample Means
-   (Joe) add an outline to the histogram bars in randomSample\_histogram() and sampleMeans\_histogram() functions
