# cj_euclid

**Description:**

R package `cj_euclid` estimates and graphs a general linear quadratic model to represent preferences from conjoint responses. 


## Installation Instructions

You can install the most recent development version using the `devtools`
package. First you have to install `devtools` using the following code.
Note that you only have to do this once:

``` r
if(!require(devtools)) install.packages("devtools")
```

Then, load `devtools` and use the function `install_github()` to install
`exr`:

```
remotes::install_github("macartan/cj_euclid")
```

## Get Started


Input is a formula listing outcomes on left-hand side and policy variables on right-hand side as well as a data frame and other arguments.

The assumed model is of the form u(z) = a + b'z - z'Az  where b and z are n-vectors and A is an n*n symmetric matrix. If R is positive semi-definite then implied preferences are "Euclidean" and can be represented via u(z) = k- +(z* - z)'A(z*-z).

The elements of a, b and A are estimated using `fixest` and plotted with `ggplot2`

```
 out <-
   cj_euclid(
    rating ~ universal + stringency,
    data = covid_policy_evaluations)
```

The output is a list containing (1) a model (2) summary information (3) a fitted data frame and (4) a plot. The summary reports the implied weighting 
