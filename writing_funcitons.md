Writing funcitons
================

## Z scores

``` r
x_vec = rnorm(25, mean = 5, sd = 4)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.422246599 -1.820226614  0.186814636  0.421831543 -0.053302114
    ##  [6]  0.160239260 -0.929265629 -0.378446491 -1.204705239 -1.621852788
    ## [11] -1.660206715  0.144426228  1.202366495  0.454802352  1.103874162
    ## [16]  0.331450707  1.433111387 -0.404407573  0.825324549  0.003573026
    ## [21]  1.783154595  1.179501406 -1.011729060  0.334561325 -0.903136047

``` r
z_scores = function(x){
  
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.422246599 -1.820226614  0.186814636  0.421831543 -0.053302114
    ##  [6]  0.160239260 -0.929265629 -0.378446491 -1.204705239 -1.621852788
    ## [11] -1.660206715  0.144426228  1.202366495  0.454802352  1.103874162
    ## [16]  0.331450707  1.433111387 -0.404407573  0.825324549  0.003573026
    ## [21]  1.783154595  1.179501406 -1.011729060  0.334561325 -0.903136047

``` r
y_vec = rnorm(40, mean = 12, sd = .3)

z_scores(y_vec)
```

    ##  [1]  0.538121018 -0.126098024 -0.091710163 -0.890537133  1.429977506
    ##  [6]  1.016799395  0.378778350  0.154012870 -0.537890986 -0.724225780
    ## [11] -0.204752067  0.616305224 -0.053389545 -0.373180200  1.263043002
    ## [16]  0.903880560 -0.856893694 -0.170930867 -0.744344777 -0.677114143
    ## [21] -0.676072989 -1.392770270 -0.004661117 -2.519269686 -0.705647319
    ## [26]  0.482952882 -0.030829163  2.161486047  0.083557831 -1.391428154
    ## [31] -0.036768651  0.712786781 -1.833695535  0.980982946 -0.176125121
    ## [36]  1.759149521 -0.585060245  0.111277790  2.161824153  0.048459753

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

    ##  [1]  0.538121018 -0.126098024 -0.091710163 -0.890537133  1.429977506
    ##  [6]  1.016799395  0.378778350  0.154012870 -0.537890986 -0.724225780
    ## [11] -0.204752067  0.616305224 -0.053389545 -0.373180200  1.263043002
    ## [16]  0.903880560 -0.856893694 -0.170930867 -0.744344777 -0.677114143
    ## [21] -0.676072989 -1.392770270 -0.004661117 -2.519269686 -0.705647319
    ## [26]  0.482952882 -0.030829163  2.161486047  0.083557831 -1.391428154
    ## [31] -0.036768651  0.712786781 -1.833695535  0.980982946 -0.176125121
    ## [36]  1.759149521 -0.585060245  0.111277790  2.161824153  0.048459753

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
    ## 1  12.0 0.285

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
    ## 1  1.88  3.66

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
    ## 1  40.3  3.21

``` r
sim_mean_sd(30, sigma = 3, mu = 40)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  40.3  2.98

``` r
sim_mean_sd(30)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.18  3.64

## Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_elements(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_elements("#cm_cr-review_list .review-rating") %>%
  html_text()

review_text = 
  dynamite_html %>%
  html_elements(".review-text-content span") %>%
  html_text()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

Write a function that gets reviews based on page url.

``` r
get_page_reviews = function(page_url) {
  
  page_html = read_html(page_url)
  
  review_titles = 
    page_html %>%
    html_elements(".a-text-bold span") %>%
    html_text()
  
  review_stars = 
    page_html %>%
    html_elements("#cm_cr-review_list .review-rating") %>%
    html_text()
  
  review_text = 
    page_html %>%
    html_elements(".review-text-content span") %>%
    html_text()
  
  reviews = tibble(
    title = review_titles,
    stars = review_stars,
    text = review_text
  )

  return(reviews)
}

base_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

urls = str_c(base_url, 1:5)

bind_rows(
  get_page_reviews(urls[1]),
  get_page_reviews(urls[2]),
  get_page_reviews(urls[3]),
  get_page_reviews(urls[4]),
  get_page_reviews(urls[5])
)
```

    ## # A tibble: 50 x 3
    ##    title                                                 stars   text           
    ##    <chr>                                                 <chr>   <chr>          
    ##  1 I Just everyone to know this....                      5.0 ou~ "\n  VOTE FOR ~
    ##  2 the cobweb in his hair during the bike ramp scene lol 5.0 ou~ "\n  5 stars f~
    ##  3 Best quirky movie ever                                5.0 ou~ "\n  You all k~
    ##  4 Classic Film                                          5.0 ou~ "\n  Had to or~
    ##  5 hehehehe                                              5.0 ou~ "\n  goodjobbo~
    ##  6 Painful                                               1.0 ou~ "\n  I think I~
    ##  7 GRAND                                                 5.0 ou~ "\n  GRAND\n"  
    ##  8 Hello, 90s                                            5.0 ou~ "\n  So nostal~
    ##  9 Cult Classic                                          5.0 ou~ "\n  Watched i~
    ## 10 Format was inaccurate                                 4.0 ou~ "\n  There was~
    ## # ... with 40 more rows
