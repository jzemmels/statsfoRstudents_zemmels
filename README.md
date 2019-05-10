
<!-- README.md is generated from README.Rmd. Please edit that file -->
![](README_files/figure-markdown_github/unnamed-chunk-3-1.png)

STAT 585 Final Project: statsfoRstudents package/shiny app
----------------------------------------------------------

[![Coverage status](https://codecov.io/gh/jzemmels/finalProject/branch/master/graph/badge.svg)](https://codecov.io/github/jzemmels/finalProject?branch=master)

This is a package meant to supplement the content of two introductory statistics courses at Iowa State University, STAT 101 and STAT 104. To install this package from github, run the following in your R console:

``` r
devtools::install_github("https://github.com/jzemmels/statsfoRstudents")
```

The function of this package is mainly in-service to the shiny app that accompanies it. To run this shiny app, run the following code:

``` r
statsfoRstudents::runShiny()
```

A summary of the some of the individual applets inside this shiny app is provided here:

-   **Normal Plot:** A visual tool for determining when to reject vs. fail to reject a null hypothesis.

-   **Hypothesis Test Game:** A madlibs-style "game" in which a randomly selected hypothesis test problem is given to the user. The user can practice the hypothesis test procedure while receiving real-time feedback.

-   **Sampling Distribution of Sample Means:** A visual tool to demonstrate the asymptotic distributional properties of the sample mean.

For a more thorough discussion of each applet and how they are implemented behing the scenes, please see the vignette in the vignettes folder of this package or [by following this link](https://jzemmels.github.io/statsfoRstudents/articles/statsforstudents.html)

<!--
#### TODO:
 - add more information to .Rmd about function of package/app
 - presentation slides
 - vignette
 - (maybe) add "Help" button to each tab of the shiny app that links to the correct section of the vignette (should just be a renderUI)
-->
