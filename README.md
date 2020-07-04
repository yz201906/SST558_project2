Project2
================
Yinzhou Zhu
6/29/2020

  - [Preparations](#preparations)
  - [Initial data exploration](#initial-data-exploration)
      - [Data parsing and inspection](#data-parsing-and-inspection)
      - [Partitioning data](#partitioning-data)
      - [Visualization](#visualization)
      - [Linear regression model](#linear-regression-model)
      - [Non-linear regression model](#non-linear-regression-model)
  - [Comparing models](#comparing-models)
  - [Automating reports for each day of
    week](#automating-reports-for-each-day-of-week)

# Preparations

Below are some custom functions that I will be using including data
filtering and plotting.

``` r
convert_to_factors <- function(column_list) {
  popularity_data[column_list] <- lapply(popularity_data[column_list], factor)
  return(popularity_data)
}
filter_by_day <- function(day){
  day_data <- popularity_data %>% filter(select(popularity_data, contains(day))==1) %>% select(!contains("week") & !url & !timedelta)
  return(day_data)
}
box_plots <- function(var){
  base_plot <-ggplot(daily_data, aes(y = .data[[var]])) 
  base_plot + geom_boxplot()
}
box_plots_log <- function(var){
  base_plot <-ggplot(daily_data, aes(y = log(.data[[var]]))) 
  base_plot + geom_boxplot()
}
box_plots_2 <- function(var_1, var_2){
  base_plot <-ggplot(daily_data, aes(.data[[var_1]], log(.data[[var_2]]))) 
  base_plot + geom_boxplot(outlier.shape = NA) + geom_jitter(size = 0.2, width = 0.3, aes(colour = .data[[var_1]]))
}
density_plots <- function(var_1) {
  base_plot <-ggplot(daily_data, aes(.data[[var_1]]))
  base_plot + geom_density()
}
density_plots_log <- function(var_1) {
  base_plot <-ggplot(daily_data, aes(log(.data[[var_1]])))
  base_plot + geom_density()
}
get_lr_rmse <- function(fitted_model, test_values) {
  return(RMSE(fitted_model$fitted.values, test_values))
}
```

# Initial data exploration

## Data parsing and inspection

``` r
popularity_data <- read.csv("OnlineNewsPopularity.csv")
anyNA(popularity_data)
```

    ## [1] FALSE

``` r
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
    ##  $ data_channel_is_lifestyle    : num  0 0 0 0 0 0 1 0 0 0 ...
    ##  $ data_channel_is_entertainment: num  1 0 0 1 0 0 0 0 0 0 ...
    ##  $ data_channel_is_bus          : num  0 1 1 0 0 0 0 0 0 0 ...
    ##  $ data_channel_is_socmed       : num  0 0 0 0 0 0 0 0 0 0 ...
    ##  $ data_channel_is_tech         : num  0 0 0 0 1 1 0 1 1 0 ...
    ##  $ data_channel_is_world        : num  0 0 0 0 0 0 0 0 0 1 ...
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
popularity_data <- convert_to_factors(c("data_channel_is_lifestyle", "data_channel_is_entertainment", "data_channel_is_bus", "data_channel_is_socmed", "data_channel_is_tech", "data_channel_is_world"))
```

## Partitioning data

Data filtering based on day of week and training/test data partitioning.

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

## Visualization

  - 5-number summary  
  - Density plot for `shares`  
  - Boxplot for `shares`  
  - Transformed density plot for `shares`  
  - Transformed boxplot for `shares`  
  - Comparison boxplots between different channels and share numbers  
  - Correlation plot for predictors

<!-- end list -->

``` r
summary(daily_data$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##       1     919    1400    3647    2700  690400

``` r
density_plots("shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-1.png)<!-- -->

``` r
box_plots("shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-2.png)<!-- -->

``` r
density_plots_log("shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-3.png)<!-- -->

``` r
box_plots_log("shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-4.png)<!-- -->

``` r
box_plots_2("data_channel_is_lifestyle", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-5.png)<!-- -->

``` r
box_plots_2("data_channel_is_entertainment", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-6.png)<!-- -->

``` r
box_plots_2("data_channel_is_bus", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-7.png)<!-- -->

``` r
box_plots_2("data_channel_is_socmed", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-8.png)<!-- -->

``` r
box_plots_2("data_channel_is_tech", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-9.png)<!-- -->

``` r
box_plots_2("data_channel_is_world", "shares")
```

![](README_files/figure-gfm/initial%20data%20visualization-10.png)<!-- -->

``` r
ggcorr(data = daily_data, size = 1, nbreaks = 5, method = c("everything", "pearson"))
```

![](README_files/figure-gfm/initial%20data%20visualization-11.png)<!-- -->

## Linear regression model

  - Step-wise forward+backward selection based on AIC values  
  - AIC plot  
  - Saved chosen predictors, training RMSE and test RMSE

<!-- end list -->

``` r
linear_model_fit <- lm(shares ~ ., data = train_data)
linear_model <- ols_step_both_aic(linear_model_fit)
linear_model
```

    ## 
    ## 
    ##                                                     Stepwise Summary                                                    
    ## ----------------------------------------------------------------------------------------------------------------------
    ## Variable                          Method        AIC              RSS               Sum Sq          R-Sq      Adj. R-Sq 
    ## ----------------------------------------------------------------------------------------------------------------------
    ## self_reference_min_shares        addition    115976.145    878279271380.956    34002315931.593    0.03727      0.03709 
    ## kw_avg_avg                       addition    115936.540    871450158409.543    40831428903.006    0.04476      0.04440 
    ## kw_max_avg                       addition    115905.029    865988458748.587    46293128563.962    0.05074      0.05021 
    ## kw_min_avg                       addition    115892.308    863599930493.973    48681656818.577    0.05336      0.05265 
    ## avg_negative_polarity            addition    115885.218    862128377849.626    50153209462.923    0.05498      0.05409 
    ## average_token_length             addition    115879.228    860836887499.779    51444699812.770    0.05639      0.05533 
    ## kw_max_min                       addition    115874.750    859791375813.300    52490211499.249    0.05754      0.05630 
    ## global_subjectivity              addition    115871.127    858884729155.358    53396858157.192    0.05853      0.05712 
    ## data_channel_is_entertainment    addition    115867.816    858029328189.066    54252259123.483    0.05947      0.05788 
    ## num_keywords                     addition    115866.373    857475326841.694    54806260470.855    0.06008      0.05831 
    ## min_positive_polarity            addition    115866.162    857119643563.554    55161943748.995    0.06047      0.05852 
    ## ----------------------------------------------------------------------------------------------------------------------

``` r
plot(linear_model$aic)
```

![](README_files/figure-gfm/linear%20model-1.png)<!-- -->

``` r
lr_formula <- paste0("shares ~ ", paste(linear_model$predictors, collapse = "+"))
final_linear_model <- lm(lr_formula, data = train_data)
lr_training_rmse <- get_lr_rmse(final_linear_model, train_data$shares)
lr_test_rmse <- get_lr_rmse(final_linear_model, test_data$shares)
lr_pred <- predict(final_linear_model, newdata = select(test_data, -"shares"))
head(lr_pred, 100)
```

    ##            2           10           15           20           22           26 
    ## -3228.520595 -2990.587305 -2601.566411  -917.499323 -3360.745100 -2335.912243 
    ##           27           39           40           45           53           60 
    ##  -208.417756 -1896.383268 -3076.097720 -1112.016544  -862.676126  -782.844185 
    ##           71           86           89          110          111          115 
    ##  -480.686333   817.497419  4608.610465  -449.103770    -1.603388  2771.517464 
    ##          118          128          146          148          153          160 
    ##  1481.714557   966.086978  1476.977762  1074.191866  3171.502763  1645.497950 
    ##          162          170          171          196          197          204 
    ##  1001.017436  1501.032677  -271.041409  2622.063427   646.632227  2703.146768 
    ##          211          231          243          244          245          251 
    ##  1716.996401  1478.212546   543.177603  2892.981742   549.563978  2121.988422 
    ##          259          264          265          268          275          277 
    ##  3005.146608   196.428975   915.809989   290.916481  3490.300668  1879.778716 
    ##          280          287          297          298          300          308 
    ##  3587.288172  1774.996197  1793.527509  1587.892088  2749.809200  5255.570516 
    ##          312          314          316          318          319          330 
    ##   919.123307  4487.686107  2377.518278  3216.723890  1017.312297  2379.818956 
    ##          331          333          334          338          341          347 
    ##  1210.148434  1964.453652  1910.142468   758.694055  1593.001574  3169.573446 
    ##          358          359          364          367          374          377 
    ##  3327.676394  3833.531142  2583.511962 -1104.626947  3879.627439  2731.911310 
    ##          381          382          385          388          397          398 
    ##   537.615433  1247.862920  2334.488730  4212.509951  2529.630832  2073.576153 
    ##          399          400          404          406          408          424 
    ##  1240.965026  3168.898147  2253.458937  2603.673301   261.321427   845.191523 
    ##          426          428          431          433          446          448 
    ##  -197.316245   321.742228  1941.147246  3509.437430  3837.787110  2828.451000 
    ##          470          473          474          477          479          481 
    ##  2934.587377  2771.859079  4182.799538  4060.463391  3626.879089 15281.742890 
    ##          484          488          498          500          501          506 
    ##  1404.849859   472.627124  2750.523571  1483.196635  3887.551564  5040.890333 
    ##          510          511          521          530 
    ##  2370.975180  3042.555930  3825.482425  1987.187113

## Non-linear regression model

  - Random forests  
  - Bagged tree  
  - Boosting tree  
  - Saved training RMSEs and test RMSEs

<!-- end list -->

``` r
cores <- 8
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
training_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = FALSE, allowParallel = TRUE)
set.seed(333)
rf_fit <- train(shares ~ ., train_data, method = "rf",
                preProcess=c("center", "scale"), trControl = training_control)
rf_fit
```

    ## Random Forest 
    ## 
    ## 5330 samples
    ##   50 predictor
    ## 
    ## Pre-processing: centered (50), scaled (50) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 4798, 4797, 4797, 4796, 4798, 4798, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   mtry  RMSE      Rsquared    MAE     
    ##    2    11350.55  0.02755032  3652.261
    ##   26    11954.23  0.02243909  3883.399
    ##   50    12425.18  0.02368309  3926.143
    ## 
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final value used for the model was mtry = 2.

``` r
set.seed(333)
tree_bag_fit <- train(shares ~ ., train_data, method = "treebag",
                 preProcess=c("center", "scale"), trControl = training_control)
tree_bag_fit
```

    ## Bagged CART 
    ## 
    ## 5330 samples
    ##   50 predictor
    ## 
    ## Pre-processing: centered (50), scaled (50) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 4798, 4797, 4797, 4796, 4798, 4798, ... 
    ## Resampling results:
    ## 
    ##   RMSE      Rsquared    MAE     
    ##   11573.25  0.01756665  3589.524

``` r
set.seed(333)
bt_fit <- train(shares ~ ., train_data, method = "gbm", verbose = 0,
                preProcess=c("center", "scale"), trControl = training_control,
                tuneGrid = expand.grid(n.trees = c(100, 200, 500), interaction.depth = c(1,4,9),
                                       shrinkage = 0.1, n.minobsinnode = 10))
bt_fit
```

    ## Stochastic Gradient Boosting 
    ## 
    ## 5330 samples
    ##   50 predictor
    ## 
    ## Pre-processing: centered (50), scaled (50) 
    ## Resampling: Cross-Validated (10 fold, repeated 3 times) 
    ## Summary of sample sizes: 4798, 4797, 4797, 4796, 4798, 4798, ... 
    ## Resampling results across tuning parameters:
    ## 
    ##   interaction.depth  n.trees  RMSE      Rsquared     MAE     
    ##   1                  100      11732.01  0.008945435  3668.105
    ##   1                  200      11799.59  0.009110739  3683.330
    ##   1                  500      12062.19  0.011473329  3682.240
    ##   4                  100      11640.59  0.024596873  3684.933
    ##   4                  200      11782.88  0.025291249  3793.274
    ##   4                  500      12052.52  0.021584941  4124.037
    ##   9                  100      11917.06  0.016155954  3933.192
    ##   9                  200      12133.55  0.017008999  4166.232
    ##   9                  500      12327.46  0.017275876  4491.501
    ## 
    ## Tuning parameter 'shrinkage' was held constant at a value of 0.1
    ## 
    ## Tuning parameter 'n.minobsinnode' was held constant at a value of 10
    ## RMSE was used to select the optimal model using the smallest value.
    ## The final values used for the model were n.trees = 100, interaction.depth =
    ##  4, shrinkage = 0.1 and n.minobsinnode = 10.

``` r
stopCluster(cl)
registerDoSEQ()

rf_train_pred <- predict(rf_fit, newdata = select(train_data, -"shares"))
rf_pred <- predict(rf_fit, newdata = select(test_data, -"shares"))
rf_train_rmse <- RMSE(rf_train_pred, test_data$shares)
rf_test_rmse <- RMSE(rf_pred, test_data$shares)

tb_train_pred <- predict(tree_bag_fit, newdata = select(train_data, -"shares"))
tb_pred <- predict(tree_bag_fit, newdata = select(test_data, -"shares"))
tb_train_rmse <- RMSE(tb_train_pred, test_data$shares)
tb_test_rmse <- RMSE(tb_pred, test_data$shares)

bt_train_pred <- predict(bt_fit, newdata = select(train_data, -"shares"))
bt_pred <- predict(bt_fit, newdata = select(test_data, -"shares"))
bt_train_rmse <- RMSE(bt_train_pred, test_data$shares)
bt_test_rmse <- RMSE(bt_pred, test_data$shares)
```

# Comparing models

``` r
lr_performance <- c("Linear regression model", lr_training_rmse, lr_test_rmse)
rf_performance <- c("Random forests model", rf_train_rmse, rf_test_rmse)
tree_bag_performance <- c("Bagged tree model", tb_train_rmse, tb_test_rmse)
bt_performance <- c("Random forests model", bt_train_rmse, bt_test_rmse)
metric <- rbind(lr_performance, rf_performance, tree_bag_performance, bt_performance)
row.names(metric) <-NULL
kable(rbind(c("", "Training RMSE", "Test RMSE"), metric))
```

|                         |                  |                  |
| :---------------------- | :--------------- | :--------------- |
|                         | Training RMSE    | Test RMSE        |
| Linear regression model | 12681.1056179252 | 20165.5965689592 |
| Random forests model    | 20836.5562723611 | 19719.0736386239 |
| Bagged tree model       | 20095.0196038929 | 19843.0988926115 |
| Random forests model    | 20287.8599820034 | 19895.4403672363 |

# Automating reports for each day of week

``` r
days <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

map(.x = days, .f = ~render(input = "template.Rmd",
                            output_file = paste0(.x,"_report.md"),
                            params = list(day_of_week = .x))
    )
```

The analysis for [Monday is available here](monday_report.md).  
The analysis for [Tuesday is available here](tuesday_report.md).  
The analysis for [Wednesday is available here](wednesday_report.md).  
The analysis for [Thursday is available here](thursday_report.md).  
The analysis for [Friday is available here](friday_report.md).  
The analysis for [Saturday is available here](saturday_report.md).  
The analysis for [Sunday is available here](sunday_report.md).
