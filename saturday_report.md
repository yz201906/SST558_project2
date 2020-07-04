Daily report
================
Yinzhou Zhu
7/1/2020

  - [Shares summary and
    visualization](#shares-summary-and-visualization)
  - [Linear regression model with first 100
    predictions](#linear-regression-model-with-first-100-predictions)
  - [Random forests model with first 100
    predictions](#random-forests-model-with-first-100-predictions)
  - [Performance comparison](#performance-comparison)

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

# Shares summary and visualization

``` r
summary(daily_data$shares)
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##      43    1300    2000    4078    3600  617900

``` r
density_plots_log("shares")
```

![](saturday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](saturday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

# Linear regression model with first 100 predictions

``` r
linear_model_fit <- lm(shares ~ ., data = train_data)
linear_model <- ols_step_both_aic(linear_model_fit)
lr_formula <- paste0("shares ~ ", paste(linear_model$predictors, collapse = "+"))
final_linear_model <- lm(lr_formula, data = train_data)
lr_training_rmse <- get_lr_rmse(final_linear_model, train_data$shares)
lr_test_rmse <- get_lr_rmse(final_linear_model, test_data$shares)
lr_pred <- predict(final_linear_model, newdata = select(test_data, -"shares"))
head(test_data$shares, 100)
```

    ##   [1]  1400  3800  2400  3500  2200  4900  5600  3900  6300  3400  2700  1400
    ##  [13]  2100  1200  3800  5400  3200  5500  2400 26800  5800  2400  1500  5200
    ##  [25]  3100  2000  4300 26400  5300 14300  5600  1900  1800  2000 22300  1400
    ##  [37]  1300  1500  7600  2700  2200  3200  8100  4600  1400  2600  1200  1600
    ##  [49]  1600  1900  1600  1200  2300 10100  1400  6600  2000  3600  2400  2000
    ##  [61] 14800  2400  5300  8400  1300  1800  2800  3000  4200  2400  4600  2400
    ##  [73]  1300  9000   473  1200  4100  5500  2600  1800  1300  1500  1500  2600
    ##  [85] 12900  3600  1400  1800   943  3800   918  2700  1300   168  2400  2200
    ##  [97]  2100  4400 24500  4500

``` r
head(lr_pred, 100)
```

    ##        9       20       28       31       37       46       50       52 
    ## 2698.590 2477.875 3277.490 2116.248 2594.438 4071.386 2248.567 2267.074 
    ##       56       64       73       79       80       88       90       98 
    ## 3678.283 3565.236 4987.311 2822.367 2389.831 3504.849 3349.136 4650.675 
    ##      102      108      114      118      119      121      127      129 
    ## 2758.573 4140.827 3883.630 3607.978 4199.033 3586.420 2866.473 4560.354 
    ##      140      142      144      151      156      164      166      169 
    ## 4207.884 4140.485 3257.829 3517.956 4400.150 3479.325 3477.449 3617.719 
    ##      177      189      196      199      204      206      207      214 
    ## 3679.266 4349.157 4289.441 4075.642 3140.961 4020.083 3846.944 5135.729 
    ##      220      229      240      243      245      249      257      261 
    ## 5097.152 5057.047 4954.794 2907.186 3817.450 3492.366 3359.603 5150.019 
    ##      268      293      301      302      308      310      312      313 
    ## 3927.078 2993.927 4403.400 4035.445 4462.097 4583.927 2669.398 3013.058 
    ##      317      318      319      327      328      331      352      369 
    ## 3760.962 5351.841 3835.428 4265.204 4501.446 3790.134 5809.018 4756.166 
    ##      380      393      398      405      407      413      416      421 
    ## 4069.815 4726.661 2662.956 4011.533 3663.365 4226.246 3709.601 4689.482 
    ##      436      444      448      456      457      458      467      470 
    ## 4922.765 4791.732 4255.138 3975.366 3180.102 3594.704 4863.458 4133.404 
    ##      471      472      477      483      486      487      493      496 
    ## 4366.955 3196.182 3738.633 3262.165 3882.301 4040.502 3533.774 3293.889 
    ##      507      508      510      516      519      523      530      538 
    ## 4188.860 3766.878 3296.455 5230.688 2572.220 3515.246 4536.888 2677.498 
    ##      539      541      549      553 
    ## 3419.098 2535.313 3724.658 2690.418

# Random forests model with first 100 predictions

``` r
cores <- 8
cl <- makePSOCKcluster(cores)
registerDoParallel(cl)
training_control <- trainControl(method = "repeatedcv", number = 10, repeats = 3, verboseIter = FALSE, allowParallel = TRUE)

set.seed(333)
rf_fit <- train(shares ~ ., train_data, method = "rf",
                preProcess=c("center", "scale"), trControl = training_control)
stopCluster(cl)
registerDoSEQ()

rf_train_pred <- predict(rf_fit, newdata = select(train_data, -"shares"))
rf_pred <- predict(rf_fit, newdata = select(test_data, -"shares"))
rf_train_rmse <- RMSE(rf_train_pred, test_data$shares)
rf_test_rmse <- RMSE(rf_pred, test_data$shares)
head(test_data$shares, 100)
```

    ##   [1]  1400  3800  2400  3500  2200  4900  5600  3900  6300  3400  2700  1400
    ##  [13]  2100  1200  3800  5400  3200  5500  2400 26800  5800  2400  1500  5200
    ##  [25]  3100  2000  4300 26400  5300 14300  5600  1900  1800  2000 22300  1400
    ##  [37]  1300  1500  7600  2700  2200  3200  8100  4600  1400  2600  1200  1600
    ##  [49]  1600  1900  1600  1200  2300 10100  1400  6600  2000  3600  2400  2000
    ##  [61] 14800  2400  5300  8400  1300  1800  2800  3000  4200  2400  4600  2400
    ##  [73]  1300  9000   473  1200  4100  5500  2600  1800  1300  1500  1500  2600
    ##  [85] 12900  3600  1400  1800   943  3800   918  2700  1300   168  2400  2200
    ##  [97]  2100  4400 24500  4500

``` r
head(rf_pred, 100)
```

    ##         9        20        28        31        37        46        50        52 
    ##  8111.643  9133.577  7805.992  8833.164  5257.547  5394.093 15023.925  5176.794 
    ##        56        64        73        79        80        88        90        98 
    ##  4296.934  8184.206  8897.860 13276.159  5317.297  8205.377  7946.680  6970.000 
    ##       102       108       114       118       119       121       127       129 
    ##  5354.929  3141.823  5274.645  7479.117  5608.475 15771.149  3420.602  5773.685 
    ##       140       142       144       151       156       164       166       169 
    ## 11778.238  5071.363  4273.507  4974.484  7121.588  6604.471  3710.057  4631.682 
    ##       177       189       196       199       204       206       207       214 
    ##  5198.005  6019.893  5464.319  3739.927  4687.670  8995.459  3996.085  7600.686 
    ##       220       229       240       243       245       249       257       261 
    ##  9426.010  3907.563  6143.996  7108.270  4455.725  3515.371  3547.948  5334.698 
    ##       268       293       301       302       308       310       312       313 
    ##  2959.801  4174.870  2858.242  2804.018  3123.225  3223.935  4984.803  3818.978 
    ##       317       318       319       327       328       331       352       369 
    ##  2728.729  4946.993  5015.764  3952.022  3819.984  2353.437  4708.543  5338.482 
    ##       380       393       398       405       407       413       416       421 
    ##  4218.037  3485.842  5658.358  3480.759  5018.154  3394.246  3955.941  4648.042 
    ##       436       444       448       456       457       458       467       470 
    ##  4128.850  3206.529  2792.144  7226.566  4095.662  3063.853  3784.423  5205.478 
    ##       471       472       477       483       486       487       493       496 
    ##  2921.948  2721.627  4822.090  4538.200  5245.856  4134.752  2753.960  3556.204 
    ##       507       508       510       516       519       523       530       538 
    ##  1488.853  6283.278  5567.232  3323.949  4141.966  3553.817  3245.938  5918.161 
    ##       539       541       549       553 
    ##  2560.314  4603.802  7505.662  3048.537

# Performance comparison

``` r
lr_performance <- c("Linear regression model", lr_training_rmse, lr_test_rmse)
rf_performance <- c("Random forests model", rf_train_rmse, rf_test_rmse)
metric <- rbind(lr_performance, rf_performance)
row.names(metric) <-NULL
kable(rbind(c("", "Training RMSE", "Test RMSE"), metric))
```

|                         |                  |                  |
| :---------------------- | :--------------- | :--------------- |
|                         | Training RMSE    | Test RMSE        |
| Linear regression model | 15526.946081155  | 6629.1409782302  |
| Random forests model    | 9887.63042885536 | 6558.56431585856 |
