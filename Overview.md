# RELATIONSHIP BETWEEN EXTERNAL FACTORS AND AGRICULTURAL INDICES IN 4 COUNTRIES 

Arman Zhussipbek 2023-01-17

This document presents a concise analytical summary of the links between external climate factors and agriculture. The focus is on the impact on general indices like crop, food, livestock and integrated agricultural productivity. Majority of analytics is founded on information from Food and Agriculture Organization with other datasources listed in attached application. The external factors considered are related to hydrological, chemical, thermal properties of certain geographical areals which in this case are separate state entities.

Analysis was processed on 4 countries in 2 separate geographical areas.

![](For%20Quarto%20Markdown/Map1.jpeg)

As example of analytics, agricultural data for Colombia (South Africa) can be shown:

``` r
plot(c2d1$Year, c1d1$Value, xlab="Year", ylab="Agriculture production relative to 2014-2016 mean (%)")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-2-1.png)

``` r
plot(c2d2$Year, c1d2$Value, xlab="Year", ylab="Crop production relative to 2014-2016 mean (%)")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-2-2.png)

``` r
plot(c2d4$Year, c1d4$Value, xlab="Year", ylab="Total food production relative to 2014-2016 mean (%)" )
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-2-3.png)

``` r
plot(c2d5$Year, c1d5$Value, xlab="Year", ylab="Livestock relative to 2014-2016 mean (%)")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-2-4.png)

As for outside factors, climate indices were used (related to hydrology, pollution and mean temperature). This data was taken from different websites which specialized in providing data

Example of climate trends for Iran:

``` r
plot(fao_clim_Iran$Year, fao_clim_Iran$Value, xlab="Year", ylab="Mean temperature change")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-3-1.png)

``` r
plot(wb_clim_Iran$Year, wb_clim_Iran$Value, xlab="Year", ylab="PM2.5 (mg/m^3)")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-3-2.png)

``` r
plot(SPEI_Iran$Year, SPEI_Iran$SPEI_12, xlab="Year", ylab="SPEI")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-3-3.png)

In application, you will be able to further see that correlations between outside factors and agriculture were assessed. Example of Colombia (here red lines represent linear fit while green quadratic fits):

``` r
plot(c2p1$Value.y, c2p1$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Agriculture production relative to 2014-2016 mean (%)")
abline(lin212, col = "red")
y <- predict(qua212, newdata = data.frame(x = c2p1$Value.y))
lines(sort(c2p1$Value.y), y[order(c2p1$Value.y)], col = "green")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-4-1.png)

``` r
plot(c2p2$Value.y, c2p2$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Crop production relative to 2014-2016 mean (%)")
abline(lin222, col = "red")
y <- predict(qua222, newdata = data.frame(x = c2p2$Value.y))
lines(sort(c2p2$Value.y), y[order(c2p2$Value.y)], col = "green")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-4-2.png)

``` r
plot(c2p4$Value.y, c2p4$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Total food production relative to 2014-2016 mean (%)" )
abline(lin242, col = "red")
y <- predict(qua242, newdata = data.frame(x = c2p4$Value.y))
lines(sort(c2p4$Value.y), y[order(c2p4$Value.y)], col = "green")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-4-3.png)

``` r
plot(c2p5$Value.y, c2p5$Value.x, xlab="PM 2.5 (mean annual exposure)", ylab="Livestock relative to 2014-2016 mean (%)")
abline(lin252, col = "red")
y <- predict(qua252, newdata = data.frame(x = c2p5$Value.y))
lines(sort(c2p5$Value.y), y[order(c2p5$Value.y)], col = "green")
```

![](Export_as_doc_files/figure-commonmark/unnamed-chunk-4-4.png)

Further, you are able to look at statistics of each model. Example of Bolivia:

``` r
databaseB = data.frame(
        Relationship_between_indices = c("Total agricultural production vs average temperature change",
                                         "Crop production vs average temperature change",
                                         "Livestock production vs average temperature change",
                                         "Total food production vs average temperature change"),
        LM_R2_adjusted = c(
          glance(lin413)$adj.r.squared,
          glance(lin423)$adj.r.squared,
          glance(lin443)$adj.r.squared,
          glance(lin453)$adj.r.squared
        ),
        LM_P_value = c(
          glance(lin413)$p.value,
          glance(lin423)$p.value,
          glance(lin443)$p.value,
          glance(lin453)$p.value
        ),
        LM_standard_error = c(
          glance(lin413)$sigma,
          glance(lin423)$sigma,
          glance(lin443)$sigma,
          glance(lin453)$sigma
        ),
        QM_R2_adjusted = c(
          glance(qua413)$adj.r.squared,
          glance(qua423)$adj.r.squared,
          glance(qua443)$adj.r.squared,
          glance(qua453)$adj.r.squared
        ),
        QM_P_value = c(
          glance(qua413)$p.value,
          glance(qua423)$p.value,
          glance(qua443)$p.value,
          glance(qua453)$p.value
        ),
        QM_standard_error = c(
          glance(qua413)$sigma,
          glance(qua423)$sigma,
          glance(qua443)$sigma,
          glance(qua453)$sigma
        ),
        stringsAsFactors = FALSE)

colnames(databaseB) <- c("Index relationship","LM (R^2 - adjusted)", "LM (P-value)", "LM (standard error)", "QM (R^2 - adj.)", "QM (P-value)", "QM (standard error)")

kable(databaseB)
```

| Index relationship                                          | LM (R\^2 - adjusted) | LM (P-value) | LM (standard error) | QM (R\^2 - adj.) | QM (P-value) | QM (standard error) |
|:----------------|---------:|---------:|---------:|---------:|---------:|---------:|
| Total agricultural production vs average temperature change |            0.5135753 |            0 |           0.2830253 |        0.5061328 |            0 |           0.2851823 |
| Crop production vs average temperature change               |            0.4890273 |            0 |           0.2900790 |        0.4804081 |            0 |           0.2925153 |
| Livestock production vs average temperature change          |            0.5254003 |            0 |           0.2795640 |        0.5218755 |            0 |           0.2806002 |
| Total food production vs average temperature change         |            0.5064557 |            0 |           0.2850891 |        0.5030616 |            0 |           0.2860676 |

All sources of used information (in both - “Shiny” app and this document) are listed in “References” tab of application.
