Writing funcitons
================

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  2.4405762  1.6228845  1.5986102 -0.2813759 -1.2247505 -0.2682551
    ##  [7] -0.4244413 -0.3633459  1.3825973 -0.6027975 -1.2909590  0.4167107
    ## [13] -0.3809380 -0.5067944 -0.5578999 -0.3722026 -0.6832853  0.0477493
    ## [19]  0.9576812 -1.8452837 -0.7251610  0.0357245  0.1479950  0.7540004
    ## [25]  0.1229608

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  2.4405762  1.6228845  1.5986102 -0.2813759 -1.2247505 -0.2682551
    ##  [7] -0.4244413 -0.3633459  1.3825973 -0.6027975 -1.2909590  0.4167107
    ## [13] -0.3809380 -0.5067944 -0.5578999 -0.3722026 -0.6832853  0.0477493
    ## [19]  0.9576812 -1.8452837 -0.7251610  0.0357245  0.1479950  0.7540004
    ## [25]  0.1229608

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  2.20191343 -0.15019603 -1.23678488  1.32778490  1.30999644 -0.64087643
    ##  [7]  1.35885823  0.67792828 -0.04312048 -0.89060442 -0.16704045 -0.67294122
    ## [13] -0.81701956 -1.48139016 -0.82967866 -1.07006487 -0.50205382  1.03605185
    ## [19]  0.53219148  1.20026362  0.18316942 -0.88892296  0.21661600  0.16744440
    ## [25] -1.43691897 -0.56600127  1.15170042  0.14345855  1.50423549  0.06155322
    ## [31] -0.84977300 -0.94569806 -0.38344289  0.56768901  1.43494574 -0.97236166
    ## [37] -1.26157644  1.18996651  0.83858060 -1.29788136

How great is this?

Only kinda great.

Let’s try again.

``` r
z_scores = function(x){
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(3)
```

    ## Error in z_scores(3): x should have at least 3 numbers

``` r
z_scores(c("my", "name", "is", "Jeff"))
```

    ## Error in z_scores(c("my", "name", "is", "Jeff")): x needs to be numeric

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): x needs to be numeric

``` r
z_scores(y_vec)
```

    ##  [1]  2.20191343 -0.15019603 -1.23678488  1.32778490  1.30999644 -0.64087643
    ##  [7]  1.35885823  0.67792828 -0.04312048 -0.89060442 -0.16704045 -0.67294122
    ## [13] -0.81701956 -1.48139016 -0.82967866 -1.07006487 -0.50205382  1.03605185
    ## [19]  0.53219148  1.20026362  0.18316942 -0.88892296  0.21661600  0.16744440
    ## [25] -1.43691897 -0.56600127  1.15170042  0.14345855  1.50423549  0.06155322
    ## [31] -0.84977300 -0.94569806 -0.38344289  0.56768901  1.43494574 -0.97236166
    ## [37] -1.26157644  1.18996651  0.83858060 -1.29788136

## Multiple outputs

``` r
mean_and_sd = function(x){
  
  if(!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if(length(x) < 3) {
    stop("x should have at least 3 numbers")
  }
  
  mean_x = mean(x)
  
  sd_x = sd(x)
  
  output_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(output_df)
  
}

#rm(x)#清除那些野生的x

mean_and_sd(y_vec)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  12.0 0.292

## Different sample sizes, means, sds

``` r
sim_data = 
  tibble(
    x = rnorm(30, mean = 2, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.62  2.86

Let’s write a function that simulates data, computes the mean and sd.

``` r
sim_mean_sd = function(n, mu = 3, sigma = 4) {
  
  #do checks on inputs
  
  sim_data = 
    tibble(
      x = rnorm(n, mean = mu, sd = sigma)
    )

  sim_data %>% 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
}

sim_mean_sd(30, 40, 3)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.0  3.53

``` r
sim_mean_sd(30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  41.4  2.70

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.72  4.52
