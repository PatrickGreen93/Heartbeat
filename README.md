
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Heartbeat

<!-- badges: start -->
<!-- badges: end -->

The goal of the Heartbeat package is to provide users with a fast and
easy method of conducting Heartbeat Analysis on Numeric Likert data with
a single function. Heartbeat Analysis itself is a simple method of
gauging sentiment of respondents at the item level by using within-group
methods to highlight when an individual’s response is significantly
higher or lower than their own typical baseline. The procedure itself
was developed and presented at SIOP 2021 by Christopher Patton and
Justin Purl.

A more in-depth explanation of arguments used within the package as well
as the math behind the analysis and how results can be used is included
in the vignette “introduction.rmd”.

## Installation

Installation of the package can be done by copying the line of code
below.

``` r
devtools::install_github("PatrickGreen93/Heartbeat")
```

## Example

Below is a basic example of how the code works and a reproducible data
set.

``` r
library(Heartbeat)

# Sample data set, set 30 items as it is a good amount to set a theoretical "baseline".
set.seed(43)
n <- 30
y <- 100
example <- data.frame()
for (i in 1:n){
  x <- data.frame(sample(1:5, y, replace = TRUE, prob = c(0.1, 0.2, 0.4, 0.2, 0.1)))
  colnames(x) <- paste0("Q",i)
  example[1:y,i] <- x
}
example[1:5,1:5]
#>   Q1 Q2 Q3 Q4 Q5
#> 1  2  4  3  4  3
#> 2  1  4  2  1  3
#> 3  3  4  2  4  4
#> 4  4  2  2  4  3
#> 5  3  2  3  3  3

example <- heartbeat(example, type = "counts", threshold = 1, props = FALSE, controversial = TRUE)
example[1:5,]
#>   Item Down_Vote Neutral Up_Vote Controversial
#> 1   Q1        14      75      11     0.2933333
#> 2   Q2        18      68      14     0.4117647
#> 3   Q3        22      59      19     0.6440678
#> 4   Q4        16      68      16     0.4705882
#> 5   Q5        18      70      12     0.3428571
```
