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

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ##     23.0    887.5   1300.0   3303.4   2600.0 843300.0

``` r
density_plots_log("shares")
```

![](wednesday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](wednesday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]   459  6400   302   575  1800  1600  5600   752 19400  1600  2000   412
    ##  [13]  1100 10400  2200  2100   727  1300  2700  1700   958   504  1300  1600
    ##  [25]  1200  4100  3400   712  2200  3100   832  1200   358  1000   910   909
    ##  [37]   999  2700  1500   837   978  1200   730   556  1700  3900  1800   755
    ##  [49]  1500  1500  1200  3400   990  1200  1700  2300  4300  2400   412   884
    ##  [61]  1500   435   949  3900   399  6900  1900  2000  2200   742  1000  3300
    ##  [73]  2200   914  1600  1700   539  7200  1200   858  1600  6100   564   923
    ##  [85]  1100  2800 15400   602  1400 12500  2700  1600  1500   630   578   639
    ##  [97]  1700   670   726   997

``` r
head(lr_pred, 100)
```

    ##           3           5          13          29          30          31 
    ##  516.232754 -632.308257 1648.673990  412.434269  874.925543  478.833529 
    ##          33          38          39          41          47          48 
    ##  322.178699  408.930319 -577.592908 1760.699420 5467.996308  -11.724018 
    ##          49          56          57          63          64          71 
    ## -754.608037 -291.500575 1475.528933  951.005694 1551.455015 1909.725080 
    ##          76          77          89          93          97         111 
    ## -303.677976 1799.001851  178.161558 1319.883440 1388.280871 2603.581563 
    ##         114         131         135         139         141         143 
    ## 2556.100281 1068.117506 1037.388059 1773.606898 1712.717147  921.760174 
    ##         146         154         157         166         172         180 
    ## 1484.554692 3161.442750 1396.438433 3160.929218 -218.115173  975.371164 
    ##         181         190         205         215         216         218 
    ## 1452.375665 2670.900254  669.946484 1802.061865 2965.520268  657.507326 
    ##         225         231         237         242         245         247 
    ## 2416.980617 1852.156427 2000.085762 1098.883453 1864.332697 1418.235314 
    ##         250         254         265         266         273         286 
    ## 2490.885349  383.200502 3157.049868 3088.766812 3698.242055 2403.732072 
    ##         287         299         302         307         310         317 
    ## 2583.207753 2210.377147 1955.938557 -113.152581 1548.539262 1384.105266 
    ##         318         323         331         334         357         376 
    ## 1685.929050 1173.624811 2970.569001 2593.324620 1905.054302 1591.403756 
    ##         383         386         388         393         403         406 
    ## 3521.457172  990.351979 2869.953549 3153.614049 3048.357578 2755.029523 
    ##         411         415         420         423         424         425 
    ## 2527.864509 3738.350088 2952.744521 2086.137965 2667.606503 3926.587041 
    ##         426         427         430         436         442         447 
    ## 3618.216395 3172.308351 2531.340180 3707.183016 3535.138609 1533.127996 
    ##         454         456         457         462         472         480 
    ## 3183.946705 4158.174872 1805.518714 1578.922862 3777.752094 2668.446964 
    ##         493         496         498         499         502         520 
    ## 2217.089439 1286.485152 1482.531059 3629.837450 2119.927345 3056.849615 
    ##         521         522         525         534 
    ##    4.209802 -924.497359 3400.362214 1817.321528

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

    ##   [1]   459  6400   302   575  1800  1600  5600   752 19400  1600  2000   412
    ##  [13]  1100 10400  2200  2100   727  1300  2700  1700   958   504  1300  1600
    ##  [25]  1200  4100  3400   712  2200  3100   832  1200   358  1000   910   909
    ##  [37]   999  2700  1500   837   978  1200   730   556  1700  3900  1800   755
    ##  [49]  1500  1500  1200  3400   990  1200  1700  2300  4300  2400   412   884
    ##  [61]  1500   435   949  3900   399  6900  1900  2000  2200   742  1000  3300
    ##  [73]  2200   914  1600  1700   539  7200  1200   858  1600  6100   564   923
    ##  [85]  1100  2800 15400   602  1400 12500  2700  1600  1500   630   578   639
    ##  [97]  1700   670   726   997

``` r
head(rf_pred, 100)
```

    ##         3         5        13        29        30        31        33        38 
    ##  2411.225  2363.516  5766.383  4642.334  2256.147  3673.557  2528.584  3212.171 
    ##        39        41        47        48        49        56        57        63 
    ##  4637.774  2178.207  8967.826  2258.764  2930.248  1415.202 22913.900  3146.545 
    ##        64        71        76        77        89        93        97       111 
    ##  3484.574  3139.828  2203.833  2240.749  2729.857  1933.205  6708.828  2515.437 
    ##       114       131       135       139       141       143       146       154 
    ##  2496.930  2723.837  2217.463  2843.871  3688.086  3548.757  2336.487  3252.917 
    ##       157       166       172       180       181       190       205       215 
    ##  3328.437  3126.070  5610.067  1995.074  2906.512  2983.348  1983.804  2455.951 
    ##       216       218       225       231       237       242       245       247 
    ##  3481.762  2522.386  2297.763  2556.921  2276.878  2101.680  2930.681  2181.755 
    ##       250       254       265       266       273       286       287       299 
    ##  2177.123  2160.992  2755.506 18819.966  3326.638  2888.613  2471.438  5668.498 
    ##       302       307       310       317       318       323       331       334 
    ##  3703.638  1867.129  2096.172  3163.871  2369.233  1643.003  4353.001  1969.861 
    ##       357       376       383       386       388       393       403       406 
    ##  2342.439  2510.122  4112.280  1881.268  5345.967  3826.088  4255.454  3825.552 
    ##       411       415       420       423       424       425       426       427 
    ##  2815.461  4307.627  3155.569  3082.227  3179.282 14292.609  6033.402  2626.383 
    ##       430       436       442       447       454       456       457       462 
    ##  2301.536  5392.682  3759.687  2977.029  2524.246  3774.585  2458.694  3500.570 
    ##       472       480       493       496       498       499       502       520 
    ##  4021.584  3738.622  1758.749  2240.668  2485.068  3009.316  2410.153  3929.318 
    ##       521       522       525       534 
    ##  2475.575  2281.355  3380.478  2463.598

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
| Linear regression model | 15868.09289444   | 6829.17191850243 |
| Random forests model    | 9990.51578708213 | 6660.28958768972 |
