Daily report
================
Yinzhou Zhu
7/1/2020

``` r
daily_data <- filter_by_day(params$day_of_week)
set.seed(35)
partition_index <- createDataPartition(daily_data$shares,
              p = .8,
              list = FALSE,
              times = 1)
partition_index <- as.vector(partition_index)
train_data <- daily_data[partition_index, ]
test_data <- daily_data[-partition_index, ]
```

``` r
linear_fit <- lm(shares ~ kw_avg_avg + n_tokens_title + num_keywords + self_reference_avg_sharess,
       data = train_data)
linear_pred <- predict(linear_fit, newdata = test_data)
summary(linear_fit)$adj.r.squared
```

    ## [1] 0.02725227
