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
    ##       5     902    1400    3179    2600  306100

``` r
density_plots_log("shares")
```

![](thursday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](thursday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]  4500   377  7500   441  1300  8100  3800   308  1400   762  1300  1200
    ##  [13]  1900  1500   698  1000   979   835  2400  1400  1100  1300  3500  2500
    ##  [25]   833  2900   744  1600  1100  1200  2000  1400  5700  1800  7200   555
    ##  [37]   902  2300  1200  1400  2100  4400 10700  1000  4100   857   849  1100
    ##  [49]  1700  1900  1000   886  3100  2000  2300  2500  1000   853  1600   850
    ##  [61]  4800   957  1400   886  2300   790  2200  2700   984  1800   413  1400
    ##  [73]  1400   639   336  2900   937   969   694   725  4700  4100  1000  1000
    ##  [85]   589   907  2300  1400  1200  1500  2400  1300  2100   851   854   748
    ##  [97]   607  2900  1100  3100

``` r
head(lr_pred, 100)
```

    ##          3          8         21         22         27         30         32 
    ##  618.69060 -194.09097  114.45724 1680.83228 4118.43411 2109.21304 3942.02557 
    ##         38         39         40         48         49         50         51 
    ##  866.98653 1907.13507  954.76303  568.77676 1312.63684 1017.92866 1174.26657 
    ##         52         56         65         68         75         78         82 
    ##  737.52137 1909.53097 2819.14359 1781.91075   45.01072  708.60092 5339.78617 
    ##         91         93         94         98        100        102        103 
    ## 2199.61972 1149.08824 1551.94704 3269.36900 3247.33542 1651.17303 2435.93001 
    ##        107        117        118        129        135        136        137 
    ## 1253.77076  431.39108 1665.85697 1389.31878 3300.37089 3686.02057 1461.60531 
    ##        149        156        166        182        184        197        203 
    ##  973.79000 1823.16239 3011.26526 1531.19820 1437.66811 1685.03499 2386.75821 
    ##        217        218        239        242        247        248        249 
    ## 2360.08663  711.84009 2581.36082 3496.10841 3417.45277 2317.92078 2116.75706 
    ##        254        258        259        273        277        278        280 
    ## 2245.00247 2474.72687 1929.79736 2566.29073 2228.28130 1968.94774 2788.06935 
    ##        288        289        301        302        316        335        341 
    ## 1527.02850 2124.16042 1264.20561 1612.75016 3228.72830 2254.59636  822.80403 
    ##        343        344        346        360        361        364        375 
    ## 1479.17305 1748.24162 3015.22360 3216.20932 1768.75345 1933.83527 1641.51092 
    ##        382        395        396        397        409        410        419 
    ## 1625.21552 1294.84524 3141.40420 2835.74363 1226.13442 4834.92777  381.40693 
    ##        420        424        429        436        449        452        455 
    ## 2711.63496 2964.18147 1139.20618 2534.18840 2592.84757 3615.06017 1559.63783 
    ##        465        475        477        483        485        487        490 
    ## 2374.90211 4775.63445 3258.32133 2598.97933 3038.72499 2326.42282 2912.78308 
    ##        497        499        501        505        521        523        526 
    ## 3197.96068 2430.62985  480.10915 1370.48797 3710.51028 2653.74367 2932.80087 
    ##        527        530 
    ## 3460.31631 4767.79523

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

    ##   [1]  4500   377  7500   441  1300  8100  3800   308  1400   762  1300  1200
    ##  [13]  1900  1500   698  1000   979   835  2400  1400  1100  1300  3500  2500
    ##  [25]   833  2900   744  1600  1100  1200  2000  1400  5700  1800  7200   555
    ##  [37]   902  2300  1200  1400  2100  4400 10700  1000  4100   857   849  1100
    ##  [49]  1700  1900  1000   886  3100  2000  2300  2500  1000   853  1600   850
    ##  [61]  4800   957  1400   886  2300   790  2200  2700   984  1800   413  1400
    ##  [73]  1400   639   336  2900   937   969   694   725  4700  4100  1000  1000
    ##  [85]   589   907  2300  1400  1200  1500  2400  1300  2100   851   854   748
    ##  [97]   607  2900  1100  3100

``` r
head(rf_pred, 100)
```

    ##         3         8        21        22        27        30        32        38 
    ##  2788.700  3675.410  3944.015  2310.149  6142.732  2927.761  4823.850  3075.594 
    ##        39        40        48        49        50        51        52        56 
    ##  4952.929  3019.506  2073.507  3464.879  4354.567  3188.614  1718.218  6201.454 
    ##        65        68        75        78        82        91        93        94 
    ##  4291.625  4634.491  2279.369  5880.224 10353.832  2793.237  2384.388  2872.790 
    ##        98       100       102       103       107       117       118       129 
    ##  5173.490  4217.990  3323.877  3395.682  9331.607  3509.616  2390.303  2649.047 
    ##       135       136       137       149       156       166       182       184 
    ##  5046.587  2812.455  4989.679  1850.785  3857.154  3867.017  1956.988  3735.601 
    ##       197       203       217       218       239       242       247       248 
    ##  2238.959  2792.032  4764.921  1741.357  3033.527  4456.702  4807.596  7052.796 
    ##       249       254       258       259       273       277       278       280 
    ##  3816.390  3107.787  2755.990  1510.545  4579.965  3070.906  2782.814  4465.606 
    ##       288       289       301       302       316       335       341       343 
    ##  2830.705  5185.884  3304.467  1827.581  3467.055  4411.484  2783.878  1868.011 
    ##       344       346       360       361       364       375       382       395 
    ##  2163.473  5133.765  2326.062  3108.169  1985.935  3773.887  3379.094  3798.566 
    ##       396       397       409       410       419       420       424       429 
    ##  3943.227  2606.562  1392.673  2696.141  2869.038  2946.532  2707.845  1686.380 
    ##       436       449       452       455       465       475       477       483 
    ##  4400.202  3034.805  5617.958  1617.046  5563.940  6492.971  2496.815  2943.534 
    ##       485       487       490       497       499       501       505       521 
    ##  4344.145  5508.795  2351.494  3972.526  2691.553  2028.702  4239.975  9916.934 
    ##       523       526       527       530 
    ##  2428.227  2510.034  2911.236  8308.302

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
| Linear regression model | 9723.6118734044  | 7754.16849274771 |
| Random forests model    | 8867.52144351243 | 7470.98577469827 |
