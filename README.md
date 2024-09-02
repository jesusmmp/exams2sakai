# Intro

Automatic Generation of Exams in R for 'Sakai'
used by universities. Question templates in
the form of the ['`exams`'](https://www.r-exams.org/) package are
transformed into Sakai XML format.

Dependencies:

  - `libxml2`: found on popular Linux distros and in Rtools by default
  - `pandoc`: gets installed by `R` during installation
  - `R`: a recent version
  - popular `R` packages: installed automatically

# Installation

From CRAN

``` r
install.packages( "exams2sakai" )
```

or from github package repository:

``` r
library(devtools)
install_github( "jesusmmp/exams2sakai" )
```

# Using `exams2sakai` function

``` r
library( exams2sakai )

myexam <- list( "boxplots", "regression", "scatterplot", "relfreq" )

ex1 <- exams2sakai( myexam, n = 1,
                    verbose = TRUE,
                    dir = ".", points = c( 1 ) )

```

Results a `qtisakai.zip` file in work directory ...

# Test on Sakai nightly server

1. https://trunk-mysql.nightly.sakaiproject.org/portal/ or https://trunk-oracle.nightly.sakaiproject.org/portal/
2. User Id: instructor. Password: sakai
3. Create a new site
4. Click Menu (View All Sites)
5. Click "Create New Site"
6. Check "course site"
7. Click "Continue"
8. Check "Discussion 1 SMPL101 Discussion" for example
9. Click "Continue"
10. Click "Continue" again
11. Check "Tests & Quizzes"
12. Click "Continue"
13. Click "Continue" again
14. Click "Create Site"
15. Click on your new site: Discussion 1 SMPL101
16. Click Tests & Quizzes
17. Click "Add" tab
18. Click "Import"
19. Upload your zip file
20. Check if your assessment has been imported successfully

# Meta-information

``` r
exname: Swiss Capital
extype: schoice
extype: mchoice
extype: num                   # Only working with one solution
exsingle: TRUE                # mcss
exsingle: FALSE               # mca
extol: 0.1                    # Tolerance for Numeric Response questions
exsolution: 0100001
exshuffle: 5
exextra[randomize]: TRUE      # Randomize answers
exextra[hasRationale]: TRUE   # Require Rationale
```
