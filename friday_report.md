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
    ##      22     974    1500    3285    2700  233400

``` r
density_plots_log("shares")
```

![](friday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](friday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]  2500  2900   720  3000  1100   840   658  2300   465  1900  1300  5700
    ##  [13]  8000  3400  2200  2100  2200  1800   930  1500  3200  1400  4800  3300
    ##  [25]  4300  1600   921  1800   931   574  4700  1400  1300  3800  2500  1100
    ##  [37]  1600  7000 40100  1600  1900  1800  4200 17300  1300  4900  1800  4600
    ##  [49]  4000  1500   655  1100  1500  2600   386  4900  1800   755  1500   623
    ##  [61]  1200  6000  1000  1500  1200  3200  1100  1500   532   719  1200  4300
    ##  [73]  1600  1000   855  1200  4200   522  2800  2200  1300  1300  1900  2000
    ##  [85]  3100   837  2900  1400  3400  1400  2900  1300  1800  2500 12500  3700
    ##  [97]  1100  1100  1100   539

``` r
head(lr_pred, 100)
```

    ##        13        20        28        31        37        39        47        50 
    ## 1683.5555 1127.7723  895.3382 2272.9202 2037.9574 2236.9364 1581.8808 1549.6134 
    ##        52        60        62        63        66        78        79        82 
    ## 2250.9423 3976.3301 1534.8615 2867.2830 1962.0540 3494.5323 2032.5689 1765.3449 
    ##        86        90        91        99       109       111       113       117 
    ## 1589.5527 1612.6933 3502.3099 3625.5752 1711.4545 1464.0833 1475.6847 2470.9442 
    ##       121       127       131       132       142       146       147       149 
    ## 2546.8753  659.3785 2340.2672 1992.7245 1327.0744 1051.3842 1861.0767 2487.1002 
    ##       161       163       167       173       179       189       194       206 
    ## 2497.7785 1527.0363 2631.6186 1535.4932 2788.0975 2338.4957 1421.9933 2804.1202 
    ##       214       217       220       223       234       235       243       252 
    ## 2053.7815  430.1258 1275.0202 2615.6878 3424.8058 1713.4865 2141.5308 3117.0957 
    ##       255       256       259       265       266       282       291       293 
    ## 1705.1631 2299.1481 1748.6047 2028.3011 1990.3722 1076.1162 1055.9340  874.0989 
    ##       295       297       315       317       333       335       344       346 
    ## 2351.3578 2818.5604 2231.1950 1143.5728 2451.9765 4029.6645 2833.2515 2347.6239 
    ##       354       355       362       366       370       372       377       397 
    ## 1094.1029 1829.8327 3072.0012 3241.7612 1969.1502 2446.3885  917.5596 1945.7334 
    ##       399       404       415       428       430       431       446       453 
    ## 3459.7616 2528.4852 1889.8104 3365.3448 2235.6335 2457.9629 2782.3578 2094.6535 
    ##       455       465       472       474       475       481       484       497 
    ## 3949.1092 2783.1499 2691.0542 3212.2075 2876.1015 4439.9222 1759.2513 2488.2561 
    ##       500       516       517       529       530       534       537       539 
    ## 4646.9605 2430.7924 4698.8704 3039.9356 3299.8154 3651.1232 4052.1116 4761.3979 
    ##       541       546       547       550 
    ## 3709.2480 2963.1168 3459.7004 2378.9981

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

    ##   [1]  2500  2900   720  3000  1100   840   658  2300   465  1900  1300  5700
    ##  [13]  8000  3400  2200  2100  2200  1800   930  1500  3200  1400  4800  3300
    ##  [25]  4300  1600   921  1800   931   574  4700  1400  1300  3800  2500  1100
    ##  [37]  1600  7000 40100  1600  1900  1800  4200 17300  1300  4900  1800  4600
    ##  [49]  4000  1500   655  1100  1500  2600   386  4900  1800   755  1500   623
    ##  [61]  1200  6000  1000  1500  1200  3200  1100  1500   532   719  1200  4300
    ##  [73]  1600  1000   855  1200  4200   522  2800  2200  1300  1300  1900  2000
    ##  [85]  3100   837  2900  1400  3400  1400  2900  1300  1800  2500 12500  3700
    ##  [97]  1100  1100  1100   539

``` r
head(rf_pred, 100)
```

    ##       13       20       28       31       37       39       47       50 
    ## 4043.584 2874.616 2719.455 2813.202 3310.941 3042.596 3226.641 2440.187 
    ##       52       60       62       63       66       78       79       82 
    ## 1865.543 7336.205 3514.258 3280.863 2933.215 3884.700 2866.609 3095.196 
    ##       86       90       91       99      109      111      113      117 
    ## 3243.456 3634.563 3069.961 3979.361 3378.721 2311.486 2743.378 3153.534 
    ##      121      127      131      132      142      146      147      149 
    ## 4553.617 2312.973 4058.928 3036.650 1960.374 2238.690 3148.643 2726.583 
    ##      161      163      167      173      179      189      194      206 
    ## 2650.084 2785.223 3772.175 2535.561 3459.963 2514.176 2774.447 5412.425 
    ##      214      217      220      223      234      235      243      252 
    ## 1608.493 2356.136 2139.188 2483.968 3025.782 2174.347 2387.401 3682.522 
    ##      255      256      259      265      266      282      291      293 
    ## 1828.405 3175.169 5009.471 2205.561 2437.673 2983.913 3624.265 3461.314 
    ##      295      297      315      317      333      335      344      346 
    ## 5491.185 3372.984 3185.761 2005.951 3750.012 7017.567 4390.993 2854.275 
    ##      354      355      362      366      370      372      377      397 
    ## 2208.862 1889.272 4347.422 7327.077 1937.087 4547.651 5348.928 2000.150 
    ##      399      404      415      428      430      431      446      453 
    ## 3059.373 3633.212 3665.156 3157.716 2807.199 4700.125 4243.504 2256.248 
    ##      455      465      472      474      475      481      484      497 
    ## 4671.657 4138.839 3380.718 2697.933 2960.327 3697.972 2571.367 3217.455 
    ##      500      516      517      529      530      534      537      539 
    ## 8524.478 2344.134 3987.922 3956.545 3837.232 7067.355 3621.353 4764.410 
    ##      541      546      547      550 
    ## 5511.844 2342.401 4367.306 2940.524

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
| Linear regression model | 8050.323145001   | 8104.00595001884 |
| Random forests model    | 8930.28106480468 | 7985.41178421949 |
