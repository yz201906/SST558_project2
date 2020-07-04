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
    ##      89    1200    1900    3747    3700   83300

``` r
density_plots_log("shares")
```

![](sunday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](sunday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]  3100  1600  1600  2600  1600  2800  1600  3800  1500  1400  2800  6300
    ##  [13]   697  9700   536  1600  2000  2100  1900  5500  2900  1500  3000   933
    ##  [25] 40400  2600  2300 28200  1000  1700  5400 21700  4200  1200  2400  1600
    ##  [37]  1200  4300  3900  2900 10000  4000  1500 34900  2300  1500  1300  2700
    ##  [49] 15700   935  3500  6200  1700  2900   943  7700  1400  1300 65300  1400
    ##  [61]  5200  1400  5400  1100  1500  3000 17500  1200  3200  1200  2800  1400
    ##  [73]  5400  2600  1100  7900  2900  6300  1600  1100  2100  1600  6400  2300
    ##  [85]  1400 17600  4800  2200 10700  6000  1200  1600  2400  7300  1800  1200
    ##  [97]  4600  9800  3000   822

``` r
head(lr_pred, 100)
```

    ##        4       16       29       31       33       38       42       48 
    ## 2148.617 1381.108 2233.054 3054.377 2540.340 2828.477 3287.504 3422.362 
    ##       58       65       66       69       74       91       97      101 
    ## 3531.263 2391.974 3212.257 1166.078 2535.970 2946.332 3244.068 3172.924 
    ##      110      116      125      127      132      135      137      145 
    ## 2565.897 3517.315 2022.481 4185.193 3333.370 2709.671 2759.317 2426.124 
    ##      156      157      159      170      173      176      177      183 
    ## 3899.388 1680.596 3764.008 1844.010 3001.799 1736.796 3027.484 1316.984 
    ##      184      206      212      222      223      224      228      229 
    ## 2885.200 3160.841 2394.194 2285.989 2897.844 4045.807 1737.751 4686.092 
    ##      232      235      237      239      243      244      246      253 
    ## 4851.715 2548.724 5688.594 4163.032 4343.869 2964.907 3023.071 2674.475 
    ##      256      260      269      271      276      281      283      289 
    ## 2815.669 4480.001 3782.077 3985.986 3316.259 5126.401 3136.252 6435.264 
    ##      292      293      305      316      321      322      327      329 
    ## 3193.387 3565.940 4319.377 2679.374 5047.028 4364.724 3551.822 5158.823 
    ##      331      341      345      347      348      356      357      362 
    ## 5071.788 6283.081 6048.784 2724.389 4071.928 4237.373 5276.731 3788.130 
    ##      367      370      374      375      382      383      391      392 
    ## 2557.572 2407.939 2595.955 3336.568 3889.196 4046.519 3374.251 3954.434 
    ##      395      398      405      409      414      415      419      424 
    ## 3546.296 4099.069 3327.300 2748.092 5924.347 3926.080 3245.252 2454.980 
    ##      425      427      432      435      439      442      447      461 
    ## 4414.276 4277.323 4704.577 3649.670 3789.727 3035.641 8292.845 4470.990 
    ##      465      473      475      476 
    ## 3273.802 4728.178 3751.434 4845.394

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

    ##   [1]  3100  1600  1600  2600  1600  2800  1600  3800  1500  1400  2800  6300
    ##  [13]   697  9700   536  1600  2000  2100  1900  5500  2900  1500  3000   933
    ##  [25] 40400  2600  2300 28200  1000  1700  5400 21700  4200  1200  2400  1600
    ##  [37]  1200  4300  3900  2900 10000  4000  1500 34900  2300  1500  1300  2700
    ##  [49] 15700   935  3500  6200  1700  2900   943  7700  1400  1300 65300  1400
    ##  [61]  5200  1400  5400  1100  1500  3000 17500  1200  3200  1200  2800  1400
    ##  [73]  5400  2600  1100  7900  2900  6300  1600  1100  2100  1600  6400  2300
    ##  [85]  1400 17600  4800  2200 10700  6000  1200  1600  2400  7300  1800  1200
    ##  [97]  4600  9800  3000   822

``` r
head(rf_pred, 100)
```

    ##        4       16       29       31       33       38       42       48 
    ## 4552.886 4848.968 4762.546 3252.988 3657.492 4797.955 3742.212 4099.585 
    ##       58       65       66       69       74       91       97      101 
    ## 3562.914 3533.560 3233.211 3768.837 3070.034 3737.843 3416.036 3495.808 
    ##      110      116      125      127      132      135      137      145 
    ## 3409.123 2706.712 3197.832 5145.452 4800.280 1916.104 2575.425 1959.446 
    ##      156      157      159      170      173      176      177      183 
    ## 4019.191 3988.656 4092.161 3225.684 3288.649 2983.057 2443.489 5245.894 
    ##      184      206      212      222      223      224      228      229 
    ## 3393.429 2490.520 2602.104 3497.858 3052.719 3032.468 2402.316 3974.329 
    ##      232      235      237      239      243      244      246      253 
    ## 4837.850 3186.908 2354.584 3490.203 4403.671 2366.684 2377.010 2780.053 
    ##      256      260      269      271      276      281      283      289 
    ## 3516.356 3321.930 3780.210 3199.970 2610.378 4465.010 3445.848 6303.847 
    ##      292      293      305      316      321      322      327      329 
    ## 3670.453 3005.850 4867.032 2851.682 4286.892 4174.115 3464.573 3276.981 
    ##      331      341      345      347      348      356      357      362 
    ## 4944.197 3504.911 6050.659 3308.426 3260.701 3821.139 7596.782 4682.230 
    ##      367      370      374      375      382      383      391      392 
    ## 2274.561 4447.937 2956.841 2681.243 5439.867 4054.205 2788.589 2871.053 
    ##      395      398      405      409      414      415      419      424 
    ## 2845.747 3726.583 3319.912 2167.000 2250.866 3206.670 4074.544 3356.442 
    ##      425      427      432      435      439      442      447      461 
    ## 3862.753 3484.503 3472.981 5125.547 3002.307 3030.680 5148.923 3420.157 
    ##      465      473      475      476 
    ## 3266.588 2708.919 4063.596 4539.182

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
| Linear regression model | 5654.67487707594 | 7749.60704996539 |
| Random forests model    | 8272.74881732589 | 7549.98829220181 |
