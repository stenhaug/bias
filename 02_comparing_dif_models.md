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
    ## 1 Item 1 Easiness -0.027 -0.027      0    
    ## 2 Item 2 Easiness -0.017 -0.017      0    
    ## 3 Item 3 Easiness  0.008  0.008      0    
    ## 4 Item 4 Easiness -0.044 -0.044      0    
    ## 5 Item 5 Easiness  0.01  -0.994     -1.00 
    ## 6 Group Mean       0     -0.996     -0.996

``` r
toc()
```

    ## 1.372 sec elapsed

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

fixef(item_ranef_by_group) %>% round(2)
```

    ##   groupold groupyoung 
    ##      -0.01      -1.03

``` r
ranef(item_ranef_by_group)$item %>% round(2)
```

    ##   groupold groupyoung
    ## 1    -0.01       0.17
    ## 2     0.00       0.14
    ## 3    -0.01       0.23
    ## 4     0.00       0.13
    ## 5     0.02      -0.67

``` r
toc()
```

    ## 8.754 sec elapsed

### lme: adding in student random effects

when we add in student random effects as well, our recovery is better
but if you sum the fixed and random effects you still don’t see the full
difference of 2 on the last item we should have

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
    ##      -0.01      -1.20

``` r
ranef(item_ranef_by_group_with_student_ranef)$item %>% round(2)
```

    ##   groupold groupyoung
    ## 1        0       0.28
    ## 2        0       0.26
    ## 3        0       0.35
    ## 4        0       0.24
    ## 5        0      -0.61

``` r
toc()
```

    ## 30.035 sec elapsed

### lme: item by group fixed effects and student random effects

this works great and recovers the right differences

``` r
tic()

item_fixef_by_group_with_student_ranef <- 
    glmer(
        resp ~ -1 + group * as.factor(item) + (1 | student), 
        data = data_dif_long, 
        family = binomial
    )
```

    ## Warning in checkConv(attr(opt, "derivs"), opt$par, ctrl = control$checkConv, :
    ## Model failed to converge with max|grad| = 0.0794114 (tol = 0.001, component 1)

``` r
fixef(item_fixef_by_group_with_student_ranef) %>% round(2)
```

    ##                    groupold                  groupyoung 
    ##                       -0.03                       -1.01 
    ##            as.factor(item)2            as.factor(item)3 
    ##                        0.05                        0.00 
    ##            as.factor(item)4            as.factor(item)5 
    ##                        0.01                        0.04 
    ## groupyoung:as.factor(item)2 groupyoung:as.factor(item)3 
    ##                       -0.07                        0.07 
    ## groupyoung:as.factor(item)4 groupyoung:as.factor(item)5 
    ##                       -0.05                       -1.01

``` r
toc()
```

    ## 133.565 sec elapsed

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
    ## 1 Item 1 Easiness -0.034 -1.01      -0.981
    ## 2 Item 2 Easiness  0.011 -1.05      -1.06 
    ## 3 Item 3 Easiness -0.032 -0.943     -0.911
    ## 4 Item 4 Easiness -0.026 -1.06      -1.03 
    ## 5 Item 5 Easiness  0.01  -1.99      -2.00 
    ## 6 Group Mean       0      0          0

``` r
toc()
```

    ## 0.552 sec elapsed

i like this approach a lot. i think this is what karen and mike were
doing with the fcme data which at first struck me as odd but now
thinking in terms of punting on finding differences in group means and
just focusing on difference for each item it makes a lot of sense.
