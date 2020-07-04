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
    ##      42     897    1300    3202    2500  441000

``` r
density_plots_log("shares")
```

![](tuesday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](tuesday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]   468  4600   442  2200  5900   458   462  2500  1900  1100   787  8900
    ##  [13] 17100  2000   635   902  1000  5100  2500  1800  1300  1500   583  1100
    ##  [25]  1100  1100  1100  1900  5500  1200  4600   689  1500  1200   969   577
    ##  [37]  1400   532  2700   990  1100  1100  2800  1000  6300  1600   895   496
    ##  [49]  2400  1900  6000   401  2100  2000   419  2200  1300  1500  1900   701
    ##  [61]  3600  1700  1000  1800  1100   700   671   676   914   949  1500   596
    ##  [73]  1200  1100   556  8100   816   596  1200  5500  4600   727  3900  1600
    ##  [85]  1800  1300  1300  1400   595  3800  3500  1500  1500  1400  2000   547
    ##  [97]   593   902   754   768

``` r
head(lr_pred, 100)
```

    ##         2         3        20        23        27        28        31        32 
    ## -862.1233  906.9101 1613.2782 1874.2161 2515.8710 1643.6091 1808.0339 2299.1566 
    ##        33        37        40        41        50        61        63        70 
    ## 1174.9854 2921.4491 1402.6968 1966.4595 2037.7192  781.7762 2648.6059 2952.0767 
    ##        73        75        76        83        86        98       105       108 
    ## 2526.2321 1666.0593 1394.6858 2148.2623  529.3534 1849.7826 3315.3018 -453.0452 
    ##       121       126       137       140       141       142       143       149 
    ## 2995.7614  895.7553 2100.8454 2636.0650 1608.0067 2862.9325 1364.9900 1877.5575 
    ##       157       164       166       170       181       191       193       208 
    ##  599.9409 1840.3646  525.5794 1306.6502 3678.1095  791.2747 2311.9773 1841.0074 
    ##       219       221       222       229       236       238       250       252 
    ##  733.4551 1686.8555 2549.7456 1654.1823 1682.2640 1328.0740  934.0388 1475.9588 
    ##       253       254       264       269       277       280       284       288 
    ## 1416.2130 1476.4783 2186.2309  799.2949  459.1312 1533.0710  973.4010 1791.6297 
    ##       292       294       299       308       317       320       321       332 
    ##  758.3207 2725.2138 1461.3231 3467.4161 2595.4792 1850.0814 2190.6870  480.4556 
    ##       335       337       340       346       348       351       355       366 
    ## 1226.0561 -135.7815  731.7922 -120.1550  794.8831 2067.9921  594.4655  803.4585 
    ##       367       378       382       388       395       396       406       422 
    ## 1017.7300  720.5652 1775.9068 2330.3010 2276.4965 3926.3305 2229.2028 1669.4323 
    ##       428       433       434       439       442       448       451       455 
    ## 3217.4137  710.2903 1226.9693 2064.8077 2578.5702 1176.6695 2133.5801 1841.9526 
    ##       457       458       469       474       484       486       487       489 
    ## 3097.8894 1757.3498 2310.9636 1460.8565 1375.5529 3002.3323 2200.7340 2527.6566 
    ##       494       499       510       514 
    ## 2094.3796 2008.8063 2508.8279 1593.6313

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

    ##   [1]   468  4600   442  2200  5900   458   462  2500  1900  1100   787  8900
    ##  [13] 17100  2000   635   902  1000  5100  2500  1800  1300  1500   583  1100
    ##  [25]  1100  1100  1100  1900  5500  1200  4600   689  1500  1200   969   577
    ##  [37]  1400   532  2700   990  1100  1100  2800  1000  6300  1600   895   496
    ##  [49]  2400  1900  6000   401  2100  2000   419  2200  1300  1500  1900   701
    ##  [61]  3600  1700  1000  1800  1100   700   671   676   914   949  1500   596
    ##  [73]  1200  1100   556  8100   816   596  1200  5500  4600   727  3900  1600
    ##  [85]  1800  1300  1300  1400   595  3800  3500  1500  1500  1400  2000   547
    ##  [97]   593   902   754   768

``` r
head(rf_pred, 100)
```

    ##        2        3       20       23       27       28       31       32 
    ## 1953.426 2652.598 3583.250 2891.255 5051.785 2147.864 2913.189 3274.071 
    ##       33       37       40       41       50       61       63       70 
    ## 3010.710 3659.508 2824.902 2562.082 2990.664 2613.241 2803.024 2573.668 
    ##       73       75       76       83       86       98      105      108 
    ## 3814.248 2917.758 1853.777 2866.114 2129.624 4133.275 2799.377 2318.403 
    ##      121      126      137      140      141      142      143      149 
    ## 3638.680 2090.303 2906.100 4725.965 1954.138 4795.223 2270.441 2444.849 
    ##      157      164      166      170      181      191      193      208 
    ## 2669.156 2834.106 2317.952 1592.031 2710.990 2048.879 2646.934 3010.038 
    ##      219      221      222      229      236      238      250      252 
    ## 1997.595 2138.286 3407.572 5067.254 2659.069 2679.451 2136.625 5083.065 
    ##      253      254      264      269      277      280      284      288 
    ## 1719.077 1985.414 2957.223 1622.414 4158.022 3084.703 1572.229 2133.094 
    ##      292      294      299      308      317      320      321      332 
    ## 2419.519 3288.021 2944.763 7827.038 5442.198 2878.137 2447.601 3632.226 
    ##      335      337      340      346      348      351      355      366 
    ## 3011.916 1934.052 2376.484 2157.460 1768.081 3056.839 3591.624 1515.833 
    ##      367      378      382      388      395      396      406      422 
    ## 2113.909 2115.037 3559.730 3169.407 2582.919 4966.914 2127.559 1977.090 
    ##      428      433      434      439      442      448      451      455 
    ## 2617.613 1821.564 1913.106 2759.940 2564.799 1839.703 2548.325 1936.399 
    ##      457      458      469      474      484      486      487      489 
    ## 2414.175 2771.919 2637.106 2334.152 2147.479 3079.485 2922.174 4176.916 
    ##      494      499      510      514 
    ## 3039.507 3706.910 2938.852 2139.235

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
| Linear regression model | 10182.595359327  | 7552.03175827809 |
| Random forests model    | 8596.01601758743 | 7184.53725834568 |
