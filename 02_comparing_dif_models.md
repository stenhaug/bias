Comparing DIF models
================

our goal is to take data simulated with DIF and walk through a few
different models that aim to recover that DIF

``` r
library(tidyverse)
library(mirt)
library(lme4)
library(tictoc)
```

01\_sim\_dif\_data.R generated data with two groups, young and old,
where young students have lower ability and the 5th item contains bias.
more specifically:

    ## # A tibble: 6 x 4
    ##   what              old young difference
    ##   <chr>           <dbl> <dbl>      <dbl>
    ## 1 Item 1 Easiness     0     0          0
    ## 2 Item 2 Easiness     0     0          0
    ## 3 Item 3 Easiness     0     0          0
    ## 4 Item 4 Easiness     0     0          0
    ## 5 Item 5 Easiness     0    -1         -1
    ## 6 Group Mean          0    -1         -1

let’s read that data in so we have both the wide and long form

``` r
data_dif_long <- read_csv("data-simmed/data_dif_long.csv")
data_dif_wide <- read_csv("data-simmed/data_dif_wide.csv")
```

now our goal is to figure out the best model that we can use to figure
out what’s going on in this data

### mirt helper function

we’ll be fitting a few mirt models so it’s useful to have a helper
function to quickly summarize them

``` r
summarize_model <- function(model){
    coef_young <- coef(model, simplify = TRUE)$young
    coef_old <- coef(model, simplify = TRUE)$old

    tibble(
        what = c(paste0("Item ", 1:5, " Easiness"), "Group Mean"),
        old = c(coef_old$items[ , "d"], coef_old$means),
        young = c(coef_young$items[ , "d"], coef_young$means)
    ) %>%
        mutate(difference = young - old) %>%
        mutate_if(is.numeric, round, 3)
}
```

### mirt: if we knew the anchors

of course if we knew that the first four items were invariant we could
use those as anchors and then we recover the data generating process
perfectly:

``` r
tic()

multipleGroup(
    data_dif_wide %>% select(-group),
    1,
    itemtype = "Rasch",
    group = data_dif_wide$group,
    # this notation says freely estimate the group means but
    # constrain items 1, 2, 3, and 4 to be invariant
    invariance = c("free_means", "Item_1", "Item_2", "Item_3", "Item_4"),
    verbose = FALSE
) %>%
    summarize_model()
```

    ## # A tibble: 6 x 4
    ##   what               old  young difference
    ##   <chr>            <dbl>  <dbl>      <dbl>
    ## 1 Item 1 Easiness -0.096 -0.096       0   
    ## 2 Item 2 Easiness  0.018  0.018       0   
    ## 3 Item 3 Easiness  0.049  0.049       0   
    ## 4 Item 4 Easiness -0.075 -0.075       0   
    ## 5 Item 5 Easiness  0.096 -1.13       -1.22
    ## 6 Group Mean       0     -0.98       -0.98

``` r
toc()
```

    ## 1.259 sec elapsed

### lme: item random effects by group

thanks to mike i think the right framing is simply can our model pick up
on the difference in performance between groups on each item

mike suggested the following model with item random effects by group.
pay attention to the difference in performance on the last item (by
summing the fixed and random effects). it isn’t quite 2 which I’m
thinking about as the typical “dampening” of parameters when our model
isn’t specified fully (in this case we don’t have student in the model)

``` r
tic()

item_ranef_by_group <- 
    glmer(
        resp ~ -1 + group + (-1 + group | item), 
        data = data_dif_long, 
        family = binomial
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
fixef(item_ranef_by_group) %>% round(2)
```

    ##   groupold groupyoung 
    ##       0.00      -1.01

``` r
ranef(item_ranef_by_group)$item %>% round(2)
```

    ##   groupold groupyoung
    ## 1    -0.01       0.11
    ## 2    -0.01       0.14
    ## 3    -0.03       0.29
    ## 4    -0.01       0.14
    ## 5     0.07      -0.68

``` r
toc()
```

    ## 1.263 sec elapsed

### lme: adding in student random effects

when we add in student random effects as well, our recovery is much
better (e.g. summing the fixed group effects and random item effects we
get a difference of 1 for the first four items and a difference of 2 for
the last item)

``` r
tic()

item_ranef_by_group_with_student_ranef <- 
    glmer(
        resp ~ -1 + group + (-1 + group | item) + (1 | student), 
        data = data_dif_long, 
        family = binomial
    )
```

    ## boundary (singular) fit: see ?isSingular

``` r
fixef(item_ranef_by_group_with_student_ranef) %>% round(2)
```

    ##   groupold groupyoung 
    ##       0.00      -1.19

``` r
ranef(item_ranef_by_group_with_student_ranef)$item %>% round(2)
```

    ##   groupold groupyoung
    ## 1    -0.02       0.22
    ## 2    -0.03       0.25
    ## 3    -0.05       0.41
    ## 4    -0.03       0.25
    ## 5     0.07      -0.64

``` r
toc()
```

    ## 3.302 sec elapsed

### mirt: fix both group means to 0

going back into the mmle with mirt world, a similar model that makes
sense is let’s fix both group means to be 0 and then let all of the
difference in performances show up in the item paramters.

indeed we’re able to recover the right differences this way.

``` r
tic()

multipleGroup(
    data_dif_wide %>% select(-group),
    1,
    itemtype = "Rasch",
    group = data_dif_wide$group,
    # note we haven't freed up means and we haven't specified anchors
    invariance = "",
    verbose = FALSE
) %>%
    summarize_model()
```

    ## # A tibble: 6 x 4
    ##   what               old  young difference
    ##   <chr>            <dbl>  <dbl>      <dbl>
    ## 1 Item 1 Easiness -0.086 -1.09      -1.00 
    ## 2 Item 2 Easiness  0.077 -1.03      -1.11 
    ## 3 Item 3 Easiness -0.019 -0.854     -0.835
    ## 4 Item 4 Easiness -0.076 -1.06      -0.979
    ## 5 Item 5 Easiness  0.096 -2.11      -2.20 
    ## 6 Group Mean       0      0          0

``` r
toc()
```

    ## 0.538 sec elapsed

i like this approach a lot. i think this is what mike and karen were
doing with the fcme data which at first struck me as odd but now
thinking in terms of punting on finding differences in group means and
just focusing on difference for each item it makes a lot of sense.
