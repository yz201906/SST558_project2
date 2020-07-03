Project2
================
Yinzhou Zhu
6/29/2020

``` r
convert_to_factors <- function(column_list) {
  popularity_data[column_list] <- lapply(popularity_data[column_list], factor)
  return(popularity_data)
}
filter_by_day <- function(day){
  day_data <- popularity_data %>% filter(select(popularity_data, contains(day))==1) %>% select(!contains("week") & !url & !timedelta)
  return(day_data)
}
scatter_plots <- function(var_1, var_2) {
  base_plot <-ggplot(daily_data, aes(remove_outliers(.data[[var_1]]), remove_outliers(.data[[var_2]]))) + geom_point()
  base_plot + geom_smooth()
}
box_plots <- function(var_1, var_2){
  base_plot <-ggplot(daily_data, aes(.data[[var_1]], remove_outliers(.data[[var_2]]))) 
  base_plot + geom_boxplot(outlier.shape = NA) + geom_jitter(size = 0.2, width = 0.3, aes(colour = .data[[var_1]]))
}
bar_plots <- function(var_1) {
  base_plot <-ggplot(daily_data, aes(remove_outliers(.data[[var_1]])))
  base_plot + geom_bar()
}
corr_plots <- function(columns){
  corr_plot <- ggpairs(daily_data, columns = columns, axisLabels = "none",
          upper = list(continuous = wrap("cor", size = 1.5)))
  print(corr_plot + theme(strip.placement = "outside", text = element_text(size = 5)))
}
remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(.25, .75), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
```

``` r
popularity_data <- read.csv("OnlineNewsPopularity.csv")
anyNA(popularity_data)
```

    ## [1] FALSE

``` r
popularity_data <- convert_to_factors(c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world"))
str(popularity_data)
```

    ## 'data.frame':    39644 obs. of  61 variables:
    ##  $ url                          : Factor w/ 39644 levels "http://mashable.com/2013/01/07/amazon-instant-video-browser/",..: 1 2 3 4 5 6 7 8 9 10 ...
    ##  $ timedelta                    : num  731 731 731 731 731 731 731 731 731 731 ...
    ##  $ n_tokens_title               : num  12 9 9 9 13 10 8 12 11 10 ...
    ##  $ n_tokens_content             : num  219 255 211 531 1072 ...
    ##  $ n_unique_tokens              : num  0.664 0.605 0.575 0.504 0.416 ...
    ##  $ n_non_stop_words             : num  1 1 1 1 1 ...
    ##  $ n_non_stop_unique_tokens     : num  0.815 0.792 0.664 0.666 0.541 ...
    ##  $ num_hrefs                    : num  4 3 3 9 19 2 21 20 2 4 ...
    ##  $ num_self_hrefs               : num  2 1 1 0 19 2 20 20 0 1 ...
    ##  $ num_imgs                     : num  1 1 1 1 20 0 20 20 0 1 ...
    ##  $ num_videos                   : num  0 0 0 0 0 0 0 0 0 1 ...
    ##  $ average_token_length         : num  4.68 4.91 4.39 4.4 4.68 ...
    ##  $ num_keywords                 : num  5 4 6 7 7 9 10 9 7 5 ...
    ##  $ data_channel_is_lifestyle    : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 2 1 1 1 ...
    ##  $ data_channel_is_entertainment: Factor w/ 2 levels "0","1": 2 1 1 2 1 1 1 1 1 1 ...
    ##  $ data_channel_is_bus          : Factor w/ 2 levels "0","1": 1 2 2 1 1 1 1 1 1 1 ...
    ##  $ data_channel_is_socmed       : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ data_channel_is_tech         : Factor w/ 2 levels "0","1": 1 1 1 1 2 2 1 2 2 1 ...
    ##  $ data_channel_is_world        : Factor w/ 2 levels "0","1": 1 1 1 1 1 1 1 1 1 2 ...
    ##  $ kw_min_min                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_max_min                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_avg_min                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_min_max                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_max_max                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_avg_max                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_min_avg                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_max_avg                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ kw_avg_avg                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ self_reference_min_shares    : num  496 0 918 0 545 8500 545 545 0 0 ...
    ##  $ self_reference_max_shares    : num  496 0 918 0 16000 8500 16000 16000 0 0 ...
    ##  $ self_reference_avg_sharess   : num  496 0 918 0 3151 ...
    ##  $ weekday_is_monday            : num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ weekday_is_tuesday           : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday_is_wednesday         : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday_is_thursday          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday_is_friday            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday_is_saturday          : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ weekday_is_sunday            : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ is_weekend                   : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ LDA_00                       : num  0.5003 0.7998 0.2178 0.0286 0.0286 ...
    ##  $ LDA_01                       : num  0.3783 0.05 0.0333 0.4193 0.0288 ...
    ##  $ LDA_02                       : num  0.04 0.0501 0.0334 0.4947 0.0286 ...
    ##  $ LDA_03                       : num  0.0413 0.0501 0.0333 0.0289 0.0286 ...
    ##  $ LDA_04                       : num  0.0401 0.05 0.6822 0.0286 0.8854 ...
    ##  $ global_subjectivity          : num  0.522 0.341 0.702 0.43 0.514 ...
    ##  $ global_sentiment_polarity    : num  0.0926 0.1489 0.3233 0.1007 0.281 ...
    ##  $ global_rate_positive_words   : num  0.0457 0.0431 0.0569 0.0414 0.0746 ...
    ##  $ global_rate_negative_words   : num  0.0137 0.01569 0.00948 0.02072 0.01213 ...
    ##  $ rate_positive_words          : num  0.769 0.733 0.857 0.667 0.86 ...
    ##  $ rate_negative_words          : num  0.231 0.267 0.143 0.333 0.14 ...
    ##  $ avg_positive_polarity        : num  0.379 0.287 0.496 0.386 0.411 ...
    ##  $ min_positive_polarity        : num  0.1 0.0333 0.1 0.1364 0.0333 ...
    ##  $ max_positive_polarity        : num  0.7 0.7 1 0.8 1 0.6 1 1 0.8 0.5 ...
    ##  $ avg_negative_polarity        : num  -0.35 -0.119 -0.467 -0.37 -0.22 ...
    ##  $ min_negative_polarity        : num  -0.6 -0.125 -0.8 -0.6 -0.5 -0.4 -0.5 -0.5 -0.125 -0.5 ...
    ##  $ max_negative_polarity        : num  -0.2 -0.1 -0.133 -0.167 -0.05 ...
    ##  $ title_subjectivity           : num  0.5 0 0 0 0.455 ...
    ##  $ title_sentiment_polarity     : num  -0.188 0 0 0 0.136 ...
    ##  $ abs_title_subjectivity       : num  0 0.5 0.5 0.5 0.0455 ...
    ##  $ abs_title_sentiment_polarity : num  0.188 0 0 0 0.136 ...
    ##  $ shares                       : int  593 711 1500 1200 505 855 556 891 3600 710 ...

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
scatter_plots("global_rate_positive_words", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-1.png)<!-- -->

``` r
scatter_plots("global_rate_negative_words", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-2.png)<!-- -->

``` r
scatter_plots("global_sentiment_polarity", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-3.png)<!-- -->

``` r
box_plots("data_channel_is_lifestyle", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-4.png)<!-- -->

``` r
box_plots("data_channel_is_entertainment", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-5.png)<!-- -->

``` r
box_plots("data_channel_is_bus", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-6.png)<!-- -->

``` r
box_plots("data_channel_is_socmed", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-7.png)<!-- -->

``` r
box_plots("data_channel_is_tech", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-8.png)<!-- -->

``` r
box_plots("data_channel_is_world", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-9.png)<!-- -->

``` r
scatter_plots("n_unique_tokens", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-10.png)<!-- -->

``` r
corr_plots(c("LDA_00", "LDA_01", "LDA_02", "LDA_03", "LDA_04"))
```

![](README_files/figure-gfm/initial%20data%20exploration-11.png)<!-- -->

``` r
scatter_plots("kw_avg_avg", "shares")
```

![](README_files/figure-gfm/initial%20data%20exploration-12.png)<!-- -->

``` r
linear_fit <- lm(shares ~ kw_avg_avg + n_tokens_title + num_keywords + self_reference_avg_sharess,
       data = train_data)
linear_pred <- predict(linear_fit, newdata = test_data)
summary(linear_fit)$adj.r.squared
```

    ## [1] 0.02725227

``` r
# cores <- 6
# cl <- makePSOCKcluster(cores)
# registerDoParallel(cl)
# training_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = FALSE, allowParallel = TRUE)
# set.seed(333)
# rf_fit <- train(shares ~ ., train_data, method = "rf",
#                 preProcess=c("center", "scale"), trControl = training_control)
# rf_fit
# rf_pred <- predict(rf_fit, newdata = select(test_data, -"shares"))
# 
# set.seed(333)
# tree_bag_fit <- train(shares ~ ., train_data, method = "treebag", 
#                  preProcess=c("center", "scale"), trControl = training_control)
# tree_bag_fit
# bagged_pred <- predict(tree_bag_fit, newdata = select(test_data, -"shares"))
# 
# set.seed(333)
# bt_fit <- train(shares ~ ., train_data, method = "gbm", verbose = 0,
#                 preProcess=c("center", "scale"), trControl = training_control,
#                 tuneGrid = expand.grid(n.trees = c(100, 200, 500), interaction.depth = c(1,4,9),
#                                        shrinkage = 0.1, n.minobsinnode = 10))
# bt_fit
# bt_pred <- predict(bt_fit, newdata = select(test_data, -"shares"))
# 
# 
# stopCluster(cl)
# registerDoSEQ()
```

``` r
days <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

map(.x = days, .f = ~render(input = "template.Rmd", 
                            output_file = paste0(.x,"report.md"), 
                            params = list(day_of_week = .x))
    )
```
