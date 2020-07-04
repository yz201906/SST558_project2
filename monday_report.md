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
    ##       1     919    1400    3647    2700  690400

``` r
density_plots_log("shares")
```

![](monday_report_files/figure-gfm/Visualize%20shares-1.png)<!-- -->

``` r
box_plots_log("shares")
```

![](monday_report_files/figure-gfm/Visualize%20shares-2.png)<!-- -->

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

    ##   [1]   711   710   761 17100   598   783  1500   504  2100  1500   400   585
    ##  [13]  1200   909   699  1500  1700  1300  1200  1300  1300   735   544  4000
    ##  [25]  1100   863  1100  1400   971   718  6500   469  2400  1400  1100  6700
    ##  [37]  1700   830  2000   349 30200  1300   804  4400  1200  2400  1500  4800
    ##  [49]   623   963  1900  2000  2500   909  3900  7400  3300  1400   912  1100
    ##  [61]  2100  2100  2300  4700  1500  1100  1400  2000  1200 13000  1800  1200
    ##  [73]  6400  1500  1300 12800   640  1400  1000  5800  2800  1500  1000   768
    ##  [85] 18600  5800  1800  1600  2000  1300   651  1100   495   485  1400  1400
    ##  [97]   494  1200  1200   812

``` r
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

    ##   [1]   711   710   761 17100   598   783  1500   504  2100  1500   400   585
    ##  [13]  1200   909   699  1500  1700  1300  1200  1300  1300   735   544  4000
    ##  [25]  1100   863  1100  1400   971   718  6500   469  2400  1400  1100  6700
    ##  [37]  1700   830  2000   349 30200  1300   804  4400  1200  2400  1500  4800
    ##  [49]   623   963  1900  2000  2500   909  3900  7400  3300  1400   912  1100
    ##  [61]  2100  2100  2300  4700  1500  1100  1400  2000  1200 13000  1800  1200
    ##  [73]  6400  1500  1300 12800   640  1400  1000  5800  2800  1500  1000   768
    ##  [85] 18600  5800  1800  1600  2000  1300   651  1100   495   485  1400  1400
    ##  [97]   494  1200  1200   812

``` r
head(rf_pred, 100)
```

    ##        2       10       15       20       22       26       27       39 
    ## 2023.714 2066.009 4240.983 2798.383 2180.506 2737.563 3028.134 1867.807 
    ##       40       45       53       60       71       86       89      110 
    ## 6551.705 1357.951 1674.112 2825.387 2317.022 2083.332 4077.247 2278.079 
    ##      111      115      118      128      146      148      153      160 
    ## 2366.607 2164.652 5429.314 2306.752 2801.682 3210.852 4016.803 2565.979 
    ##      162      170      171      196      197      204      211      231 
    ## 2651.095 2984.189 2202.334 2859.820 3881.656 3361.946 3198.005 2460.964 
    ##      243      244      245      251      259      264      265      268 
    ## 3002.877 2927.541 2641.666 1590.083 4988.584 2352.952 2021.507 2295.496 
    ##      275      277      280      287      297      298      300      308 
    ## 4890.231 3878.035 4441.767 3825.689 4032.914 2616.142 5956.594 7399.530 
    ##      312      314      316      318      319      330      331      333 
    ## 4323.417 8790.145 4070.354 2599.948 1336.443 2606.571 2972.346 2784.841 
    ##      334      338      341      347      358      359      364      367 
    ## 3931.077 1652.508 2087.288 3064.184 3277.443 3747.162 5101.192 5260.605 
    ##      374      377      381      382      385      388      397      398 
    ## 3687.630 2553.735 3008.834 2938.309 2279.535 6422.996 4153.501 3597.211 
    ##      399      400      404      406      408      424      426      428 
    ## 2651.516 3441.209 2528.969 3271.453 4611.401 1981.663 1568.004 2649.161 
    ##      431      433      446      448      470      473      474      477 
    ## 2449.605 3081.810 6217.808 5595.761 3044.558 3415.292 2863.003 4696.931 
    ##      479      481      484      488      498      500      501      506 
    ## 3393.413 5695.537 2705.918 3339.074 2566.998 5156.895 2673.121 5069.124 
    ##      510      511      521      530 
    ## 2399.893 4824.603 3640.576 4513.879

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
| Linear regression model | 12681.1056179252 | 20165.5965689592 |
| Random forests model    | 20836.5562723611 | 19719.0736386239 |
