set.seed(1)

library(tidyverse)
library(mirt)

n_students_each_group <- 10000

# sim old students mean 0 and no bias -------------------------------------
theta_old <- rnorm(n_students_each_group, 0, 1)

data_old <-
    simdata(
        a = c(1, 1, 1, 1, 1),
        d = c(0, 0, 0, 0, 0),
        itemtype = "2PL",
        Theta = matrix(theta_old)
    )

# sim young students mean -1 and item 5 bias ------------------------------
theta_young <- rnorm(n_students_each_group, -1, 1)

data_young <-
    simdata(
        a = c(1, 1, 1, 1, 1),
        d = c(0, 0, 0, 0, -1), # item 5 biases against young students!
        itemtype = "2PL",
        Theta = matrix(theta_young)
    )

# combine -----------------------------------------------------------------
data_dif_wide <-
    rbind(data_old, data_young) %>%
    as_tibble() %>%
    mutate(group = rep(c("old", "young"), each = n_students_each_group)) %>%
    select(group, everything())

# check -------------------------------------------------------------------
# generating parameters
tibble(
    what = c(paste0("Item ", 1:5, " Easiness"), "Group Mean"),
    old = c(0, 0, 0, 0, 0, 0),
    young = c(0, 0, 0, 0, -1, -1)
) %>%
    mutate(difference = young - old)

# recovery
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

multipleGroup(
    data_dif_wide %>% select(-group),
    1,
    itemtype = "Rasch",
    group = data_dif_wide$group,
    # this notation says freely estimate the group means but
    # constrain item 1 to be invariant
    invariance = c("free_means", "Item_1", "Item_2", "Item_3", "Item_4"),
    verbose = FALSE
) %>%
    summarize_model()

# output ------------------------------------------------------------------
data_dif_wide %>% write_csv("data-simmed/data_dif_wide.csv")

data_dif_wide %>%
    mutate(student = row_number()) %>%
    gather(item, resp, -group, -student) %>%
    mutate(item = parse_number(item) %>% as.integer()) %>%
    write_csv("data-simmed/data_dif_long.csv")
