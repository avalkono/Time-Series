Times Series HW
================
2024-08-22

HW 0:

``` r
air <- read_csv('https://raw.githubusercontent.com/sjsimmo2/TimeSeries/master/usairlines.csv')
```

    ## Rows: 219 Columns: 3
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (3): Year, Month, Passengers
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
air.ts<- air %>% mutate(date=mdy(paste(Month, "1",Year))) %>%  mutate(Month.ts = yearmonth(date)) %>% as_tsibble(index = Month.ts)
air.ts
```

    ## # A tsibble: 219 x 5 [1M]
    ##     Year Month Passengers date       Month.ts
    ##    <dbl> <dbl>      <dbl> <date>        <mth>
    ##  1  1990     1      34348 1990-01-01 1990 Jan
    ##  2  1990     2      33536 1990-02-01 1990 Feb
    ##  3  1990     3      40578 1990-03-01 1990 Mar
    ##  4  1990     4      38267 1990-04-01 1990 Apr
    ##  5  1990     5      38249 1990-05-01 1990 May
    ##  6  1990     6      40792 1990-06-01 1990 Jun
    ##  7  1990     7      42225 1990-07-01 1990 Jul
    ##  8  1990     8      44943 1990-08-01 1990 Aug
    ##  9  1990     9      35708 1990-09-01 1990 Sep
    ## 10  1990    10      38286 1990-10-01 1990 Oct
    ## # ℹ 209 more rows

``` r
count_gaps(air.ts)
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: .from <mth>, .to <mth>, .n <int>

``` r
autoplot(air.ts, Passengers)
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
energy <- read_csv('energy_F2024.csv', show_col_types = FALSE)
energy <- energy %>% mutate(datetime_beginning_utc = mdy_hm(energy$datetime_beginning_utc), datetime_beginning_ept = mdy_hm(energy$datetime_beginning_ept))
energy <- energy %>% mutate(date = date(energy$datetime_beginning_ept))
energy
```

    ## # A tibble: 52,608 × 9
    ##    datetime_beginning_utc datetime_beginning_ept nerc_region mkt_region zone 
    ##    <dttm>                 <dttm>                 <chr>       <chr>      <chr>
    ##  1 2018-08-01 04:00:00    2018-08-01 00:00:00    RFC         WEST       AEP  
    ##  2 2018-08-01 05:00:00    2018-08-01 01:00:00    RFC         WEST       AEP  
    ##  3 2018-08-01 06:00:00    2018-08-01 02:00:00    RFC         WEST       AEP  
    ##  4 2018-08-01 07:00:00    2018-08-01 03:00:00    RFC         WEST       AEP  
    ##  5 2018-08-01 08:00:00    2018-08-01 04:00:00    RFC         WEST       AEP  
    ##  6 2018-08-01 09:00:00    2018-08-01 05:00:00    RFC         WEST       AEP  
    ##  7 2018-08-01 10:00:00    2018-08-01 06:00:00    RFC         WEST       AEP  
    ##  8 2018-08-01 11:00:00    2018-08-01 07:00:00    RFC         WEST       AEP  
    ##  9 2018-08-01 12:00:00    2018-08-01 08:00:00    RFC         WEST       AEP  
    ## 10 2018-08-01 13:00:00    2018-08-01 09:00:00    RFC         WEST       AEP  
    ## # ℹ 52,598 more rows
    ## # ℹ 4 more variables: load_area <chr>, mw <dbl>, is_verified <lgl>, date <date>

``` r
energy2 <- energy %>% group_by(date) %>% summarise(mw=sum(mw))
energy2 %>% arrange(desc(mw))
```

    ## # A tibble: 2,192 × 2
    ##    date            mw
    ##    <date>       <dbl>
    ##  1 2022-12-24 183480.
    ##  2 2019-01-21 176162.
    ##  3 2024-01-17 170286.
    ##  4 2019-01-31 168222.
    ##  5 2019-01-30 165320.
    ##  6 2024-01-20 162835.
    ##  7 2022-12-23 162513.
    ##  8 2024-01-21 156640.
    ##  9 2019-01-22 155242.
    ## 10 2022-12-25 154010.
    ## # ℹ 2,182 more rows

``` r
max(energy2$mw)
```

    ## [1] 183480.3

``` r
energy.ts <- as_tsibble(energy2, index=date)
energy.ts
```

    ## # A tsibble: 2,192 x 2 [1D]
    ##    date            mw
    ##    <date>       <dbl>
    ##  1 2018-08-01 110120.
    ##  2 2018-08-02 108872.
    ##  3 2018-08-03 106642.
    ##  4 2018-08-04 108985.
    ##  5 2018-08-05 110822.
    ##  6 2018-08-06 119088.
    ##  7 2018-08-07 119968.
    ##  8 2018-08-08 117167.
    ##  9 2018-08-09 118797.
    ## 10 2018-08-10 115601.
    ## # ℹ 2,182 more rows

``` r
count_gaps(energy.ts)
```

    ## # A tibble: 0 × 3
    ## # ℹ 3 variables: .from <date>, .to <date>, .n <int>

HW 1:

``` r
energy <- read_csv('energy_F2024.csv', show_col_types = FALSE)
energy <- energy %>% mutate(datetime_beginning_utc = mdy_hm(energy$datetime_beginning_utc), datetime_beginning_ept = mdy_hm(energy$datetime_beginning_ept))
energy <- energy %>% mutate(date = date(energy$datetime_beginning_ept))
energy2 <- energy %>% group_by(date) %>% summarise(mw=sum(mw))
energy.ts <- as_tsibble(energy2, index=date)
#energy.ts$yr_month = format(energy.ts$date, "%Y-%m")
#energy.ts$ym = ym(energy.ts$yr_month)
energy.ts$yr_month = yearmonth(energy.ts$date)

energy.month = energy.ts[, -1] %>% group_by(yr_month) %>% summarise(mw = mean(mw)) %>% as_tsibble(index=yr_month)
energy.month
```

    ## # A tsibble: 72 x 2 [1M]
    ##    yr_month      mw
    ##       <mth>   <dbl>
    ##  1 2018 Aug 112235.
    ##  2 2018 Sep 106165.
    ##  3 2018 Oct 101049.
    ##  4 2018 Nov 110296.
    ##  5 2018 Dec 119736.
    ##  6 2019 Jan 130367.
    ##  7 2019 Feb 116998.
    ##  8 2019 Mar 111472.
    ##  9 2019 Apr  94396.
    ## 10 2019 May  98615.
    ## # ℹ 62 more rows

``` r
train = energy.month[c(1:48),]
validation = energy.month[c(49:60),]
test = energy.month[c(61:72),]
```

1.  Using the total daily average by month in the training data set,
    what is the median value?

``` r
median(train$mw)
```

    ## [1] 106820.9

2.  Decompose your training data by using the STL decomposition assuming
    there is a monthly seasonality. Describe what you see in your
    decomposition. Describe what you observe for each component (feel
    free to use other plots to help you describe seasonality and trend).
    Which components explain the most variability? Use your Fs and Ft to
    help guide your discussions.

``` r
energy_dcmp <- train |> model(stl = STL(mw))
components(energy_dcmp)
```

    ## # A dable: 48 x 7 [1M]
    ## # Key:     .model [1]
    ## # :        mw = trend + season_year + remainder
    ##    .model yr_month      mw   trend season_year remainder season_adjust
    ##    <chr>     <mth>   <dbl>   <dbl>       <dbl>     <dbl>         <dbl>
    ##  1 stl    2018 Aug 112235. 110900.       3265.    -1930.       108970.
    ##  2 stl    2018 Sep 106165. 110687.      -5064.      542.       111229.
    ##  3 stl    2018 Oct 101049. 110474.     -12118.     2692.       113167.
    ##  4 stl    2018 Nov 110296. 110261.        588.     -553.       109708.
    ##  5 stl    2018 Dec 119736. 110048.       9087.      601.       110648.
    ##  6 stl    2019 Jan 130367. 109834.      19908.      625.       110459.
    ##  7 stl    2019 Feb 116998. 109620.      13303.    -5925.       103695.
    ##  8 stl    2019 Mar 111472. 109380.      -2186.     4278.       113658.
    ##  9 stl    2019 Apr  94396. 109139.     -13828.     -914.       108225.
    ## 10 stl    2019 May  98615. 108898.     -12956.     2673.       111571.
    ## # ℹ 38 more rows

``` r
#decomposition plot
components(energy_dcmp) |> autoplot() + theme_classic()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#decomposition trend overlay
components(energy_dcmp) |> as_tsibble() |> autoplot(mw, colour="gray") + geom_line(aes(y=trend), colour = "#D55E00") + labs( y = "Passengers", title = "Avg Monthly Megawatts with trend overlaid" )
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

``` r
#seasonal plot
train |> gg_season(mw, labels = "both") + labs(y = "MW", title = "Seasonal plot: Avg MW by Month")
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-7-3.png)<!-- -->

``` r
#seasonal subplots
train |> gg_subseries(mw) + labs( y = "MW", title = "Seasonal Subplots: Avg MW by Month" )
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-7-4.png)<!-- --> The
seasonal component has a clear, repetitive pattern over time. Average
daily MW usage appears to peak during January of each year, then goes
down to its lowest usage in April or May, then raises to a smaller peak
in July followed again by lower usage in October. The seasonal strength
value for this data is about 0.91, meaning that there is strong
seasonality in this data, which is clearly shown in the STL
decomposition plot. This can also be confirmed by looking at seasonal
plot, which shows a similar pattern of average daily MW usage in each
month of every year. The trend component shows a slight downward trend
until about mid-2020 where it then changes to a slightly upward trend.
This is also seen in the time plot with trend overlaid, which follows
the height of the peaks in each year. The peaks get smaller following
January 2019, but then get higher again starting in January 2021, which
follows the path of the trend line. The trend strength value is about
0.37, meaning that a trend is present but not very strong, which is seen
in both of these plots. The remainder values fluctuate between -4000 and
4000, which are relatively small since the MW values are typically over
100000. This indicates that the seasonal and trend components describe
the time series well. The seasonal component explains the most
variability.

3.  Create a time plot of the total daily AVERAGE MW by month and upload
    it. Be sure your graph has appropriate axes, labels and title.

``` r
autoplot(train, mw) + labs(y="Megawatts (MW)", x= "Month [1M]", title="Average Megawatts by Month (Aug 2018 - Jul 2022)") + theme(plot.title = element_text(hjust = 0.5, face='bold', size = 15))
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

4.  What is your value for Fs? 0.9141054
5.  What is your value for Ft? 0.3680337

``` r
train |> features(mw, feat_stl)
```

    ## # A tibble: 1 × 9
    ##   trend_strength seasonal_strength_year seasonal_peak_year seasonal_trough_year
    ##            <dbl>                  <dbl>              <dbl>                <dbl>
    ## 1          0.368                  0.914                  6                    9
    ## # ℹ 5 more variables: spikiness <dbl>, linearity <dbl>, curvature <dbl>,
    ## #   stl_e_acf1 <dbl>, stl_e_acf10 <dbl>

HW 2:

``` r
energy <- read_csv('energy_F2024.csv', show_col_types = FALSE)
energy
```

    ## # A tibble: 52,608 × 8
    ##    datetime_beginning_utc datetime_beginning_ept nerc_region mkt_region zone 
    ##    <chr>                  <chr>                  <chr>       <chr>      <chr>
    ##  1 8/1/2018 4:00          8/1/2018 0:00          RFC         WEST       AEP  
    ##  2 8/1/2018 5:00          8/1/2018 1:00          RFC         WEST       AEP  
    ##  3 8/1/2018 6:00          8/1/2018 2:00          RFC         WEST       AEP  
    ##  4 8/1/2018 7:00          8/1/2018 3:00          RFC         WEST       AEP  
    ##  5 8/1/2018 8:00          8/1/2018 4:00          RFC         WEST       AEP  
    ##  6 8/1/2018 9:00          8/1/2018 5:00          RFC         WEST       AEP  
    ##  7 8/1/2018 10:00         8/1/2018 6:00          RFC         WEST       AEP  
    ##  8 8/1/2018 11:00         8/1/2018 7:00          RFC         WEST       AEP  
    ##  9 8/1/2018 12:00         8/1/2018 8:00          RFC         WEST       AEP  
    ## 10 8/1/2018 13:00         8/1/2018 9:00          RFC         WEST       AEP  
    ## # ℹ 52,598 more rows
    ## # ℹ 3 more variables: load_area <chr>, mw <dbl>, is_verified <lgl>

``` r
#Convert to date-time and pull out date only
energy <- energy %>% mutate(datetime_beginning_ept = mdy_hm(energy$datetime_beginning_ept))
energy <- energy %>% mutate(date = date(energy$datetime_beginning_ept))

#Roll up to total daily MW
energy2 <- energy %>% group_by(date) %>% summarise(mw=sum(mw))

#Average MW by month
energy2 <- energy2 %>% mutate(yr_month = yearmonth(energy2$date))
energy2 <- energy2 %>% group_by(yr_month) %>% summarise(mw=mean(mw))
energy2
```

    ## # A tibble: 72 × 2
    ##    yr_month      mw
    ##       <mth>   <dbl>
    ##  1 2018 Aug 112235.
    ##  2 2018 Sep 106165.
    ##  3 2018 Oct 101049.
    ##  4 2018 Nov 110296.
    ##  5 2018 Dec 119736.
    ##  6 2019 Jan 130367.
    ##  7 2019 Feb 116998.
    ##  8 2019 Mar 111472.
    ##  9 2019 Apr  94396.
    ## 10 2019 May  98615.
    ## # ℹ 62 more rows

``` r
#Convert to tsibble
energy.ts <- as_tsibble(energy2, index=yr_month)
energy.ts
```

    ## # A tsibble: 72 x 2 [1M]
    ##    yr_month      mw
    ##       <mth>   <dbl>
    ##  1 2018 Aug 112235.
    ##  2 2018 Sep 106165.
    ##  3 2018 Oct 101049.
    ##  4 2018 Nov 110296.
    ##  5 2018 Dec 119736.
    ##  6 2019 Jan 130367.
    ##  7 2019 Feb 116998.
    ##  8 2019 Mar 111472.
    ##  9 2019 Apr  94396.
    ## 10 2019 May  98615.
    ## # ℹ 62 more rows

``` r
#Create train, validation, and test
train = energy.month[c(1:48),]
validation = energy.month[c(49:60),]
test = energy.month[c(61:72),]
```

Training Dataset Trend (STL decomposition):

``` r
energy_dcmp <- train |> model(stl = STL(mw))
components(energy_dcmp)
```

    ## # A dable: 48 x 7 [1M]
    ## # Key:     .model [1]
    ## # :        mw = trend + season_year + remainder
    ##    .model yr_month      mw   trend season_year remainder season_adjust
    ##    <chr>     <mth>   <dbl>   <dbl>       <dbl>     <dbl>         <dbl>
    ##  1 stl    2018 Aug 112235. 110900.       3265.    -1930.       108970.
    ##  2 stl    2018 Sep 106165. 110687.      -5064.      542.       111229.
    ##  3 stl    2018 Oct 101049. 110474.     -12118.     2692.       113167.
    ##  4 stl    2018 Nov 110296. 110261.        588.     -553.       109708.
    ##  5 stl    2018 Dec 119736. 110048.       9087.      601.       110648.
    ##  6 stl    2019 Jan 130367. 109834.      19908.      625.       110459.
    ##  7 stl    2019 Feb 116998. 109620.      13303.    -5925.       103695.
    ##  8 stl    2019 Mar 111472. 109380.      -2186.     4278.       113658.
    ##  9 stl    2019 Apr  94396. 109139.     -13828.     -914.       108225.
    ## 10 stl    2019 May  98615. 108898.     -12956.     2673.       111571.
    ## # ℹ 38 more rows

``` r
#decomposition plot
components(energy_dcmp) |> autoplot() + theme_classic()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

``` r
#decomposition trend overlay
components(energy_dcmp) |> as_tsibble() |> autoplot(mw, colour="black") + geom_line(aes(y=trend), colour = "#D55E00", size=1) + labs( y = "Megawatts", x="Year Month", title = "Average Megawatts Per Month (Aug 2018 - Jul 2022)", subtitle="Overlaid with STL Decomposition Trend" ) + theme_minimal() + theme(legend.position="none", plot.title = element_text(hjust = 0.5, face = 'bold', size=15), plot.subtitle = element_text(hjust = 0.5, size=13), axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10)))
```

    ## Warning: Using `size` aesthetic for lines was deprecated in ggplot2 3.4.0.
    ## ℹ Please use `linewidth` instead.
    ## This warning is displayed once every 8 hours.
    ## Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    ## generated.

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-11-2.png)<!-- -->

``` r
#ggsave(plot1, file = "trend.png", dpi = 700)
```

Validation Time Plot:

``` r
#Additive
HWadd.energy <- train |> model(ETS(mw ~ error("A") + trend("A") + season("A")))
energy.for.add <- HWadd.energy |> fabletools::forecast(h=length(validation$yr_month))
report(HWadd.energy)
```

    ## Series: mw 
    ## Model: ETS(A,A,A) 
    ##   Smoothing parameters:
    ##     alpha = 0.2182997 
    ##     beta  = 0.0001000047 
    ##     gamma = 0.0001001501 
    ## 
    ##   Initial states:
    ##      l[0]      b[0]     s[0]     s[-1]     s[-2]     s[-3]     s[-4]    s[-5]
    ##  112478.6 -156.3184 6157.426 -4431.856 -12230.83 -13541.76 -1852.746 13602.79
    ##     s[-6]    s[-7]   s[-8]     s[-9]    s[-10]   s[-11]
    ##  20542.35 8285.199 915.735 -13255.04 -5765.835 1574.569
    ## 
    ##   sigma^2:  23117061
    ## 
    ##      AIC     AICc      BIC 
    ## 1014.247 1034.647 1046.058

``` r
energy.for.add |> autoplot(energy.ts, level = 0) + geom_line(aes(y = .fitted), col="#D55E00", data = augment(HWadd.energy)) + labs(y="Megawatts", title="Avg Megawatts per month (add)") + guides(colour = "none") + theme(legend.position="none")
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->

``` r
#Multiplicative
HWmult.energy <- train |> model(ETS(mw ~ error("M") + trend("A") + season("M")))
energy.for.mult <- HWmult.energy |> fabletools::forecast(h=length(validation$yr_month))
report(HWmult.energy)
```

    ## Series: mw 
    ## Model: ETS(M,A,M) 
    ##   Smoothing parameters:
    ##     alpha = 0.167092 
    ##     beta  = 0.00010313 
    ##     gamma = 0.000104043 
    ## 
    ##   Initial states:
    ##      l[0]      b[0]     s[0]   s[-1]     s[-2]     s[-3]     s[-4]    s[-5]
    ##  112789.9 -144.6566 1.050658 0.95321 0.8814424 0.8744815 0.9835806 1.121839
    ##    s[-6]    s[-7]    s[-8]     s[-9]    s[-10]   s[-11]
    ##  1.19441 1.081199 1.003469 0.8835681 0.9475273 1.024614
    ## 
    ##   sigma^2:  0.002
    ## 
    ##      AIC     AICc      BIC 
    ## 1012.913 1033.313 1044.723

``` r
energy.for.mult|> autoplot(energy.ts, level=0) + geom_line(aes(y = .fitted), col="#D55E00", data = augment(HWmult.energy)) + labs(y="Megawatts", title="Avg Megawatts per months (mult)") + guides(colour = "none") + theme(legend.position="none")
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-12-2.png)<!-- -->

``` r
energy_fit <- train |>
  model(
    SES = ETS(mw ~ error("A") + trend("N") + season("N")),
    `Linear` = ETS(mw ~ error("A") + trend("A") + season("N")),
    `Damped Linear` = ETS(mw ~ error("A") + trend("Ad") + season("N")),
    HWAdd = ETS(mw ~ error("A") + trend("A") + season("A")),
    HWMult = ETS(mw ~ error("M") + trend("A") + season("M"))
  )

energy_fc <- energy_fit |>fabletools::forecast(h = length(validation$yr_month))
fabletools::accuracy(energy_fc, energy.ts)
```

    ## # A tibble: 5 × 10
    ##   .model        .type     ME   RMSE    MAE   MPE  MAPE  MASE RMSSE   ACF1
    ##   <chr>         <chr>  <dbl>  <dbl>  <dbl> <dbl> <dbl> <dbl> <dbl>  <dbl>
    ## 1 Damped Linear Test  -7542. 12235. 10387. -8.32 10.7  2.01  1.91   0.468
    ## 2 HWAdd         Test  -2249.  5582.  4069. -2.25  3.79 0.789 0.869 -0.228
    ## 3 HWMult        Test  -2323.  5347.  3971. -2.34  3.72 0.770 0.833 -0.235
    ## 4 Linear        Test   3197. 10021.  8481.  2.31  8.06 1.64  1.56   0.448
    ## 5 SES           Test  -7759. 12383. 10539. -8.53 10.9  2.04  1.93   0.469

``` r
sd(validation$mw)
```

    ## [1] 10079.87

``` r
energy.for.mult <- HWmult.energy |> fabletools::forecast(h=length(validation$yr_month))

energy.for.mult|> autoplot(validation, level=0) + labs(y="Megawatts", title="Actual vs Predicted Average Megawatts (Aug 2022 - Jul 2023)", x= "Year Month") + theme_minimal() + theme(legend.position = 'none', plot.title = element_text(hjust = 0.5, face = 'bold', size=15), axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10))) 
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->

``` r
#ggsave(plot2, file = "forecast.png", width = 7, height = 4, dpi = 700)
```

Final Project:

``` r
energy <- read_csv('energy_F2024.csv', show_col_types = FALSE)
energy
```

    ## # A tibble: 52,608 × 8
    ##    datetime_beginning_utc datetime_beginning_ept nerc_region mkt_region zone 
    ##    <chr>                  <chr>                  <chr>       <chr>      <chr>
    ##  1 8/1/2018 4:00          8/1/2018 0:00          RFC         WEST       AEP  
    ##  2 8/1/2018 5:00          8/1/2018 1:00          RFC         WEST       AEP  
    ##  3 8/1/2018 6:00          8/1/2018 2:00          RFC         WEST       AEP  
    ##  4 8/1/2018 7:00          8/1/2018 3:00          RFC         WEST       AEP  
    ##  5 8/1/2018 8:00          8/1/2018 4:00          RFC         WEST       AEP  
    ##  6 8/1/2018 9:00          8/1/2018 5:00          RFC         WEST       AEP  
    ##  7 8/1/2018 10:00         8/1/2018 6:00          RFC         WEST       AEP  
    ##  8 8/1/2018 11:00         8/1/2018 7:00          RFC         WEST       AEP  
    ##  9 8/1/2018 12:00         8/1/2018 8:00          RFC         WEST       AEP  
    ## 10 8/1/2018 13:00         8/1/2018 9:00          RFC         WEST       AEP  
    ## # ℹ 52,598 more rows
    ## # ℹ 3 more variables: load_area <chr>, mw <dbl>, is_verified <lgl>

``` r
#Convert to date-time and pull out date only
energy <- energy %>% mutate(datetime_beginning_ept = mdy_hm(energy$datetime_beginning_ept))
energy <- energy %>% mutate(date = date(energy$datetime_beginning_ept))

#Roll up to total daily MW
energy2 <- energy %>% group_by(date) %>% summarise(mw=sum(mw))

#Average MW by month
energy2 <- energy2 %>% mutate(yr_month = yearmonth(energy2$date))
energy2 <- energy2 %>% group_by(yr_month) %>% summarise(mw=mean(mw))

#Convert to tsibble
energy.ts <- as_tsibble(energy2, index=yr_month)
energy.ts
```

    ## # A tsibble: 72 x 2 [1M]
    ##    yr_month      mw
    ##       <mth>   <dbl>
    ##  1 2018 Aug 112235.
    ##  2 2018 Sep 106165.
    ##  3 2018 Oct 101049.
    ##  4 2018 Nov 110296.
    ##  5 2018 Dec 119736.
    ##  6 2019 Jan 130367.
    ##  7 2019 Feb 116998.
    ##  8 2019 Mar 111472.
    ##  9 2019 Apr  94396.
    ## 10 2019 May  98615.
    ## # ℹ 62 more rows

``` r
#Create train, validation, and test
train = energy.month[c(1:48),]
validation = energy.month[c(49:60),]
test = energy.month[c(61:72),]
```

To meet the client’s wish for a nonseasonal model, the data was adjusted
to eliminate seasonal effects. To do so, Seasonal and Trend using LOESS
(STL) decomposition was used to extract the seasonal component from the
data. Figure 1 displays the actual MW usage values compared to the
seasonally adjusted values for the training dataset. As displayed in
Figure 1, the seasonally adjusted data does not have the repetitive
seasonal fluctuations seen in the original data.

``` r
#STL decomposition
energy_dcmp <- energy.ts |> model(stl = STL(mw))
components(energy_dcmp)
```

    ## # A dable: 72 x 7 [1M]
    ## # Key:     .model [1]
    ## # :        mw = trend + season_year + remainder
    ##    .model yr_month      mw   trend season_year remainder season_adjust
    ##    <chr>     <mth>   <dbl>   <dbl>       <dbl>     <dbl>         <dbl>
    ##  1 stl    2018 Aug 112235. 111072.       3247.    -2085.       108988.
    ##  2 stl    2018 Sep 106165. 110837.      -5675.     1002.       111839.
    ##  3 stl    2018 Oct 101049. 110602.     -12135.     2582.       113184.
    ##  4 stl    2018 Nov 110296. 110366.        469.     -538.       109828.
    ##  5 stl    2018 Dec 119736. 110131.      10108.     -504.       109627.
    ##  6 stl    2019 Jan 130367. 109897.      19375.     1095.       110991.
    ##  7 stl    2019 Feb 116998. 109662.      11965.    -4629.       105033.
    ##  8 stl    2019 Mar 111472. 109405.      -2065.     4132.       113537.
    ##  9 stl    2019 Apr  94396. 109149.     -13449.    -1303.       107845.
    ## 10 stl    2019 May  98615. 108892.     -12524.     2247.       111139.
    ## # ℹ 62 more rows

``` r
#seasonal adjust data
seasonaladj = components(energy_dcmp) %>% dplyr::select(yr_month, season_adjust, mw) %>% rename('mw'=season_adjust, 'actual' = mw)
seasonaladj = seasonaladj %>% mutate(seasonaldiff = actual-mw)

trainadj = seasonaladj[c(1:48),]
validationadj = seasonaladj[c(49:60),]
testadj = seasonaladj[c(61:72),]

train_dcmp = energy.ts[c(1:48),] |> model(stl = STL(mw))


train_dcmp_long = components(train_dcmp) %>% dplyr::select(yr_month, season_adjust, mw, trend) %>% pivot_longer(cols = c(season_adjust, mw), names_to = "Metric", values_to = "Value")
train_dcmp_long
```

    ## # A tsibble: 96 x 4 [1M]
    ## # Key:       Metric [2]
    ##    yr_month   trend Metric          Value
    ##       <mth>   <dbl> <chr>           <dbl>
    ##  1 2018 Aug 110900. season_adjust 108970.
    ##  2 2018 Aug 110900. mw            112235.
    ##  3 2018 Sep 110687. season_adjust 111229.
    ##  4 2018 Sep 110687. mw            106165.
    ##  5 2018 Oct 110474. season_adjust 113167.
    ##  6 2018 Oct 110474. mw            101049.
    ##  7 2018 Nov 110261. season_adjust 109708.
    ##  8 2018 Nov 110261. mw            110296.
    ##  9 2018 Dec 110048. season_adjust 110648.
    ## 10 2018 Dec 110048. mw            119736.
    ## # ℹ 86 more rows

``` r
ggplot(train_dcmp_long, aes(x=yr_month, y=Value, color=Metric)) + geom_line(size=0.75) +  scale_color_manual(values = c("mw" = "darkgrey", "season_adjust" = "#D55E00"), labels = c("Actual", "Seasonally Adjusted")) + labs( y = "Megawatts (MW)", x= "Year Month", title = "Seasonally Adjusted Data (Aug 2018 - Jul 2022)" ) + theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size=17), axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10)), legend.text = element_text(size = 12), legend.title = element_blank(), legend.position = 'bottom')
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->

``` r
#ggsave(plot2, file = "season.png", width = 7, height = 4, dpi = 700)
```

The seasonally adjusted training data was then used to find the best
performing ARIMA model. The team first found that the series was
stationary using a KPSS unit root test. The autocorrelation and partial
autocorrelation plots both contained a significant spike at the second
lag, indicating the potential need for a MA(2) or AR(2) term. An AR(2),
MA(2), and ARMA(2,2) model were built, of which the AR(2) had the lowest
AICc and thus was chosen as the best performing model. The residuals of
the AR(2) model were found to be white noise using the Ljung-Box test,
and the predicted values were confirmed to be stationary. Figure 2
displays the plot of MW usage forecasted from the AR(2) model versus the
actual data in the validation dataset.

``` r
#Testing if data is stationary
# Perform the KPSS test 
trainadj |> features(mw, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.403      0.0757

``` r
#Perform ndiffs test
trainadj |> features(mw, unitroot_ndiffs)
```

    ## # A tibble: 1 × 1
    ##   ndiffs
    ##    <int>
    ## 1      0

Stationary -\> can perform ARIMA

``` r
#Autocorrelation plot
ggAcf(trainadj$mw,lag=12)
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-19-1.png)<!-- -->

``` r
#Partial autocorrelation plot
ggPacf(trainadj$mw,lag=12) 
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-19-2.png)<!-- --> Try
AR(2), MA(2)

``` r
energy_model <- trainadj %>%
  model(ar2 = ARIMA(mw ~ pdq(2,0,0) + PDQ(0,0,0)),
        ma2 = ARIMA(mw ~ pdq(0,0,2) + PDQ(0,0,0)),
        arma22 = ARIMA(mw ~ pdq(2,0,2) + PDQ(0,0,0)),
        search1 = ARIMA(mw~PDQ(0,0,0)),
        search2 = ARIMA(mw,stepwise = F))

energy_model2<-as.data.frame(energy_model)
t(energy_model2)
```

    ##         [,1]                
    ## ar2     ARIMA(2,0,0) w/ mean
    ## ma2     ARIMA(0,0,2) w/ mean
    ## arma22  ARIMA(2,0,2) w/ mean
    ## search1 ARIMA(0,0,2) w/ mean
    ## search2 ARIMA(2,0,0) w/ mean

``` r
glance(energy_model) %>% arrange(AICc) %>% dplyr::select(.model:BIC)
```

    ## # A tibble: 5 × 6
    ##   .model     sigma2 log_lik   AIC  AICc   BIC
    ##   <chr>       <dbl>   <dbl> <dbl> <dbl> <dbl>
    ## 1 ar2     14649334.   -463.  934.  934.  941.
    ## 2 search2 14649334.   -463.  934.  934.  941.
    ## 3 ma2     14988390.   -463.  935.  936.  942.
    ## 4 search1 14988390.   -463.  935.  936.  942.
    ## 5 arma22  15230592.   -463.  937.  939.  948.

``` r
#AR(2) model
energy_model %>% dplyr::select(ar2) %>% gg_tsresiduals()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-21-1.png)<!-- -->

``` r
energy_model %>% dplyr::select(ar2) %>% residuals() %>% ggPacf()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-21-2.png)<!-- -->

``` r
augment(energy_model) %>% filter(.model=='ar2') %>% features(.innov,ljung_box, lag=10, dof = 2)
```

    ## # A tibble: 1 × 3
    ##   .model lb_stat lb_pvalue
    ##   <chr>    <dbl>     <dbl>
    ## 1 ar2       3.04     0.932

``` r
#MA(2) model
energy_model %>% dplyr::select(ma2) %>% gg_tsresiduals()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-21-3.png)<!-- -->

``` r
energy_model %>% dplyr::select(ma2) %>% residuals() %>% ggPacf()
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-21-4.png)<!-- -->

``` r
augment(energy_model) %>% filter(.model=='ma2') %>% features(.innov,ljung_box, lag=10, dof = 2)
```

    ## # A tibble: 1 × 3
    ##   .model lb_stat lb_pvalue
    ##   <chr>    <dbl>     <dbl>
    ## 1 ma2       3.37     0.909

``` r
#AR(2) adding back seasonality
energy_for_ar<-energy_model %>% dplyr::select(ar2) %>% fabletools::forecast(h=length(validationadj$yr_month))
energy_for_ar_seas <- energy_for_ar %>% mutate(.mean = .mean + validationadj$seasonaldiff)
ar_df = data.frame(
  yr_month = energy_for_ar_seas$yr_month,
  Actual = validationadj$actual,
  Predicted = energy_for_ar_seas$.mean
)

ar_data_long <- ar_df%>%
  pivot_longer(cols = c(Actual, Predicted), names_to = "Metric", values_to = "Value")

#MA(2) adding back seasonality
energy_for_ma<-energy_model %>% dplyr::select(ma2) %>% fabletools::forecast(h=length(validationadj$yr_month))
energy_for_ma_seas <- energy_for_ma %>% mutate(.mean = .mean + validationadj$seasonaldiff)
ma_df = data.frame(
  yr_month = energy_for_ma_seas$yr_month,
  Actual = validationadj$actual,
  Predicted = energy_for_ma_seas$.mean
)

ma_data_long <- ma_df%>%
  pivot_longer(cols = c(Actual, Predicted), names_to = "Metric", values_to = "Value")

#AR(2) plot
ggplot(ar_data_long, aes(x=yr_month, y=Value, color=Metric)) + geom_line(size=0.75) + 
    labs(y="Megawatts (MW)", title="Actual vs Predicted Average MW", x= "Year Month", subtitle = "Forecasted with AR(2) Model") + 
    scale_color_manual(values = c("Actual" = "darkblue", "Predicted" = "red")) + 
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size=17), axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), plot.subtitle = element_text(hjust = 0.5, size=13), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10)), legend.text = element_text(size = 12), legend.title = element_blank(), legend.position = 'bottom')
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-22-1.png)<!-- -->

``` r
#MA(2) plot
ggplot(ma_data_long, aes(x=yr_month, y=Value, color=Metric)) + geom_line(size=0.75) + 
    labs(y="Megawatts (MW)", title="Actual vs Predicted Average MW - MA(2)", x= "Year Month") + 
    scale_color_manual(values = c("Actual" = "darkblue", "Predicted" = "red")) + 
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size=17), axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10)), legend.text = element_text(size = 12), legend.title = element_blank(), legend.position = 'bottom')
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-22-2.png)<!-- -->

``` r
#ggsave(plot2, file = "forecast.png", width = 7, height = 5, dpi = 700)
```

``` r
#White noise test
ar_df$residuals = ar_df$Actual - ar_df$Predicted
Box.test(ar_df$residuals, lag = 2, type = "Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  ar_df$residuals
    ## X-squared = 2.3877, df = 2, p-value = 0.3031

``` r
ma_df$residuals = ma_df$Actual - ma_df$Predicted
Box.test(ma_df$residuals, lag = 2, type = "Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  ma_df$residuals
    ## X-squared = 2.2024, df = 2, p-value = 0.3325

``` r
#Stationary test
as_tsibble(ar_df) |> features(Predicted, unitroot_kpss)
```

    ## Using `yr_month` as index variable.

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1    0.0901         0.1

``` r
as_tsibble(ma_df) |> features(Predicted, unitroot_kpss)
```

    ## Using `yr_month` as index variable.

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1    0.0897         0.1

Accuracy Statistics:

``` r
#AR(2) accuracy statistics
energy_resid_ar<-validationadj$actual-energy_for_ar$.mean
MAPE<-mean(abs(energy_resid_ar/validationadj$actual))
MAE<-mean(abs(energy_resid_ar))
MAPE
```

    ## [1] 0.08583508

``` r
MAE
```

    ## [1] 8420.05

``` r
#MA(2) accuracy statistics
energy_resid_ma<-validationadj$actual-energy_for_ma$.mean
MAPE<-mean(abs(energy_resid_ma/validationadj$actual))
MAE<-mean(abs(energy_resid_ma))
MAPE
```

    ## [1] 0.08515919

``` r
MAE
```

    ## [1] 8363.53

``` r
sd(validationadj$actual)
```

    ## [1] 10079.87

``` r
fabletools::accuracy(energy_for_ar$.mean, validationadj$actual)
```

    ##                 ME     RMSE     MAE       MPE     MAPE
    ## Test set -4326.426 10527.78 8420.05 -5.137337 8.583508

``` r
fabletools::accuracy(energy_for_ma$.mean, validationadj$actual)
```

    ##                 ME     RMSE     MAE       MPE     MAPE
    ## Test set -4195.268 10433.62 8363.53 -5.003218 8.515919

A second method that the team used was to build a Bayesian Structural
Time Series (BSTS) with a trigonometric seasonal component. This model
used sine and cosine trigonometric functions to model the periodic
fluctuations in the data, effectively capturing seasonal patterns. The
residuals of the BSTS model were found to be white noise using the
Ljung-Box test, and the predicted values were confirmed to be
stationary.

``` r
#Testing if data is stationary
# Perform the KPSS test 
train |> features(mw, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1     0.109         0.1

``` r
#Perform ndiffs test
train |> features(mw, unitroot_ndiffs)
```

    ## # A tibble: 1 × 1
    ##   ndiffs
    ##    <int>
    ## 1      0

``` r
library(bsts)
```

    ## Loading required package: BoomSpikeSlab

    ## Loading required package: Boom

    ## 
    ## Attaching package: 'Boom'

    ## The following object is masked from 'package:stats':
    ## 
    ##     rWishart

    ## 
    ## Attaching package: 'BoomSpikeSlab'

    ## The following object is masked from 'package:stats':
    ## 
    ##     knots

    ## Loading required package: xts

    ## 
    ## ######################### Warning from 'xts' package ##########################
    ## #                                                                             #
    ## # The dplyr lag() function breaks how base R's lag() function is supposed to  #
    ## # work, which breaks lag(my_xts). Calls to lag(my_xts) that you type or       #
    ## # source() into this session won't work correctly.                            #
    ## #                                                                             #
    ## # Use stats::lag() to make sure you're not using dplyr::lag(), or you can add #
    ## # conflictRules('dplyr', exclude = 'lag') to your .Rprofile to stop           #
    ## # dplyr from breaking base R's lag() function.                                #
    ## #                                                                             #
    ## # Code in packages is not affected. It's protected by R's namespace mechanism #
    ## # Set `options(xts.warn_dplyr_breaks_lag = FALSE)` to suppress this warning.  #
    ## #                                                                             #
    ## ###############################################################################

    ## 
    ## Attaching package: 'xts'

    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     first, last

    ## 
    ## Attaching package: 'bsts'

    ## The following object is masked from 'package:BoomSpikeSlab':
    ## 
    ##     SuggestBurn

``` r
energy.bsts = log(train$mw)

model_components=list()
model_components = AddLocalLevel(model_components, y = energy.bsts)
model_components=AddTrig(model_components, y = energy.bsts, period = 12,frequencies = 1:3)
fit.season=bsts(energy.bsts, model_components, niter = 2000)
```

    ## =-=-=-=-= Iteration 0 Tue Oct 29 16:25:42 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 200 Tue Oct 29 16:25:42 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 400 Tue Oct 29 16:25:42 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 600 Tue Oct 29 16:25:42 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 800 Tue Oct 29 16:25:42 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 1000 Tue Oct 29 16:25:43 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 1200 Tue Oct 29 16:25:43 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 1400 Tue Oct 29 16:25:43 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 1600 Tue Oct 29 16:25:43 2024 =-=-=-=-=
    ## =-=-=-=-= Iteration 1800 Tue Oct 29 16:25:43 2024 =-=-=-=-=

``` r
plot(fit.season$final.state[,2],type='l')
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-26-1.png)<!-- -->

``` r
pred.season<-predict(fit.season,burn = 500,horizon = 24)
plot(pred.season)
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-26-2.png)<!-- -->

``` r
pred.season$interval
```

    ##           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]
    ## 2.5%  11.50232 11.40619 11.37405 11.45301 11.57560 11.63688 11.58704 11.44815
    ## 97.5% 11.69664 11.61132 11.56805 11.64714 11.78344 11.83757 11.79880 11.65880
    ##           [,9]    [,10]    [,11]    [,12]    [,13]    [,14]   [,15]    [,16]
    ## 2.5%  11.33322 11.32705 11.41651 11.50848 11.48006 11.39260 11.3550 11.42655
    ## 97.5% 11.55088 11.54513 11.64633 11.73318 11.71531 11.63595 11.5876 11.67769
    ##          [,17]   [,18]    [,19]    [,20]    [,21]    [,22]    [,23]    [,24]
    ## 2.5%  11.54956 11.6175 11.55944 11.41763 11.30504 11.30898 11.40560 11.49548
    ## 97.5% 11.79441 11.8635 11.81362 11.68448 11.57433 11.56562 11.66229 11.76604

``` r
forecast.bsts <- predict(fit.season, burn = 500, horizon = 12)
forecasted_values <- exp(forecast.bsts$mean)
validation$predicted = forecasted_values
validation$actual = validation$mw
```

``` r
validation_long <- validation %>%
  pivot_longer(cols = c(actual, predicted), names_to = "Metric", values_to = "Value")

#Fitted vs predicted plot
ggplot(validation_long, aes(x=yr_month, y=Value, color=Metric)) + geom_line(size=0.75) + 
    labs(y="Megawatts (MW)", title="Actual vs Predicted Average MW", x= "Year Month", subtitle = "Forecasted with BSTS Model") + 
    scale_color_manual(values = c("actual" = "darkblue", "predicted" = "red"), label = c("Actual", "Predicted")) + 
    theme_minimal() + theme(plot.title = element_text(hjust = 0.5, face = 'bold', size=17), plot.subtitle = element_text(hjust = 0.5, size=13),  axis.title = element_text(size=13, face='bold'), axis.text = element_text(color='black', size=10), axis.title.x = element_text(margin = margin(t=10)), axis.title.y = element_text(margin = margin(r=10)), legend.text = element_text(size = 12), legend.title = element_blank(), legend.position = 'bottom')
```

![](TimeSeriesHW_files/figure-gfm/unnamed-chunk-28-1.png)<!-- -->

``` r
#Accuracy measures
energy_resid_bsts<-validation$actual-validation$predicted
MAPE<-mean(abs(energy_resid_bsts/validation$actual))
MAE<-mean(abs(energy_resid_bsts))
MAPE
```

    ## [1] 0.04694907

``` r
MAE
```

    ## [1] 4859.022

``` r
sd(validation$actual)
```

    ## [1] 10079.87

``` r
#ggsave(plot2, file = "forecast2.png", width = 7, height = 5, dpi = 700)
```

``` r
#White noise test
validation$residuals = validation$actual - validation$predicted
Box.test(validation$residuals, lag = 2, type = "Ljung-Box")
```

    ## 
    ##  Box-Ljung test
    ## 
    ## data:  validation$residuals
    ## X-squared = 4.5371, df = 2, p-value = 0.1035

``` r
#Stationary test
validation |> features(predicted, unitroot_kpss)
```

    ## # A tibble: 1 × 2
    ##   kpss_stat kpss_pvalue
    ##       <dbl>       <dbl>
    ## 1    0.0944         0.1
