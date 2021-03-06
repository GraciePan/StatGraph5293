Continuous Variables, pt. 2
================

# Weekly Savings

``` r
library(tidyverse)
set.seed(5702)
WeeklySavings <- tibble(A = rexp(n = 52, rate = .015)) %>% 
  mutate(B = rnorm(n = 52, mean = median(A) - 2,
                    sd = 50),
         C = ifelse(test = B > median(B) + 10, 
                     yes = B * 10, no = B),
         D = ifelse(test = B < median(B) - 10,
                     yes = B - 100, no = B)) 
WeeklySavings <- WeeklySavings %>% 
  dplyr::mutate_all(sort)

head(WeeklySavings)
```

    ## # A tibble: 6 x 4
    ##       A     B     C     D
    ##   <dbl> <dbl> <dbl> <dbl>
    ## 1 0.909 -95.2 -95.2 -195.
    ## 2 1.50  -75.1 -75.1 -175.
    ## 3 2.02  -61.8 -61.8 -162.
    ## 4 2.40  -46.2 -46.2 -146.
    ## 5 3.27  -39.8 -39.8 -140.
    ## 6 4.77  -37.6 -37.6 -138.

``` r
#DT::datatable(WeeklySavings, options = list(paging=F)) %>% DT::formatCurrency(1:4)
```

# Histograms

``` r
tidySavings <- gather(WeeklySavings, person, amount)
ggplot(tidySavings, aes(amount)) + geom_histogram(fill = "blue") + facet_wrap(~person)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

# Boxplots

``` r
boxplot(WeeklySavings, horizontal = TRUE, las = 1)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />

# Boxplot (Person “D”)

``` r
fivenumnames <- c("min", "lower-hinge", "median", "upper-hinge", "max")
D <- WeeklySavings$D
fivenum(D) %>% set_names(fivenumnames)
```

    ##         min lower-hinge      median upper-hinge         max 
    ##      -195.2      -100.5        33.9        57.7       163.1

``` r
boxplot(D, horizontal = TRUE, ylim=c(-250, 200))
text(fivenum(D)[c(1,3,5)], 1.25, round(fivenum(D)[c(1,3,5)],1), col = "red")
text(fivenum(D)[c(2,4)], .75, round(fivenum(D),1)[c(2,4)], col = "red")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-4-1.png" style="display: block; margin: auto;" />

# Boxplot with outliers (Person “C”)

``` r
C <- WeeklySavings$C
fivenum(C) %>% set_names(fivenumnames)
```

    ##         min lower-hinge      median upper-hinge         max 
    ##     -95.249      -0.473      33.889     577.408    1631.089

``` r
boxplot(C, horizontal = TRUE)
text(median(C), 1.25, round(median(C),1), col = "red")
text(fivenum(C)[c(2,4)], .75, round(fivenum(C),1)[c(2,4)], col = "red")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-5-1.png" style="display: block; margin: auto;" />

# What does it take to be an outlier?

# What does it take to be an outlier?

<img src="boyfriend.png" style="width: 800px;"/>

<https://www.explainxkcd.com/wiki/index.php/539:_Boyfriend>

# What does it take to be an outlier?

``` r
df <- read_csv("BoxOfficeMojo2019Jan0406.csv")
df$`Weekend Gross` <- df$`Weekend Gross`/1000000
```

``` r
b <- boxplot(df$`Weekend Gross`, 
        horizontal = TRUE, axes = F,
        ylim = c(0, 35), lty = "solid", 
        lwd = 2, border = "blue")
mtext("Weekend Box Office Gross, Top 20",
      side = 3, line = -1, cex = 1.5, 
      font = 2)
mtext("Jan 4-6, 2019",
      side = 3, line = -3, cex = 1.2)
mtext("in millions of $", side = 1, 
      line = .5, cex = 1.2)
axis(1, 0:35, labels = NA, line = -2)
axis(1, seq(0, 35, 5), lwd = 2, line = -2)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-7-1.png" style="display: block; margin: auto;" />

``` r
# Outlier = "Aquaman" $31 million
```

Source: <http://www.boxofficemojo.com/weekend/chart/>

# What does it take to be an outlier?

``` r
b <- boxplot(df$`Weekend Gross`, 
        horizontal = TRUE, axes = F,
        ylim = c(0, 35), lty = "solid", 
        lwd = 2, border = "blue")
mtext("Weekend Box Office Gross, Top 20",
      side = 3, line = -1, cex = 1.5, 
      font = 2)
mtext("Jan 4-6, 2019",
      side = 3, line = -3, cex = 1.2)
mtext("in millions of $", side = 1, 
      line = .5, cex = 1.2)
axis(1, 0:35, labels = NA, line = -2)
axis(1, seq(0, 35, 5), lwd = 2, line = -2)
outliers <- which(df$`Weekend Gross` %in% b$out)
text(b$out, 1.1, df$Title[outliers], col = "red")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-8-1.png" style="display: block; margin: auto;" />

``` r
# Outlier = "Aquaman"
```

# What does it take to be an outlier?

<center>

<img src = "dollarruler1.png" width = "600" />

</center>

“H-spread” or fourth spread (upper hinge - lower hinge)

# What does it take to be an outlier?

<center>

<img src = "dollarruler2.png" width = "600" />

</center>

fences:

1.5 x hinge spread above upper-hinge

1.5 x hinge spread below lower-hinge

# Fences

``` r
b <- boxplot(df$`Weekend Gross`, 
        horizontal = TRUE, axes = F,
        ylim = c(-10, 20), lty = "solid", 
        lwd = 2, border = "blue")
f <- fivenum(df$`Weekend Gross`)
fences <- c(f[2],f[4]) + c(-1,1)*1.5*(f[4]-f[2])
abline(v = fences, col = "green", lwd = 2)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-9-1.png" style="display: block; margin: auto;" />

fences:

1.5 x hinge spread above upper-hinge

1.5 x hinge spread below lower-hinge

# Tukey’s original boxplot

``` r
b <- boxplot(df$`Weekend Gross`, axes = F,
        ylim = c(-30, 45), lty = "solid", 
        lwd = 2, border = "blue")
innerfences <- c(f[2],f[4]) + c(-1,1)*1.5*(f[4]-f[2])
outerfences <- c(f[2],f[4]) + c(-1,1)*3*(f[4]-f[2])
abline(h = innerfences, col = "green", lwd = 2)
text(1, innerfences[1]+1.5, "inner fence (1.5 times hinge-spread from hinge)", col = "green")
abline(h = outerfences, col = "darkgreen", lwd = 2)
text(1, outerfences[1]+1.5, "outer fence (3 times hinge-spread from hinge)", col = "darkgreen")
diff <- outerfences[2] - innerfences[2]
text(1, diff/2 + innerfences[2]+2, "outside values", col = "blue")
text(1, diff/2 + outerfences[2], "far out values", col = "blue")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-10-1.png" style="display: block; margin: auto;" />

# Quartiles

``` r
boxoffice
```

    ##  [1]  0.703  0.923  1.005  1.168  1.609  1.808  1.843  1.903  2.147  2.368
    ## [11]  3.303  4.674  4.755  5.735  9.110 13.127 13.203 15.861 18.238 31.003

``` r
fivenum(boxoffice) %>% set_names(fivenumnames)
```

    ##         min lower-hinge      median upper-hinge         max 
    ##       0.703       1.709       2.835      11.118      31.003

``` r
quantile(boxoffice)
```

    ##     0%    25%    50%    75%   100% 
    ##  0.703  1.758  2.835 10.114 31.003

See: ?quantile for different methods

Sometimes boxplots are drawn using the IQR (interquartile range) instead
of hinge spread

# base R vs. ggplot2

``` r
library(mlbench)
data(Ozone)
boxplot(V5 ~ V3, data = Ozone)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-12-1.png" style="display: block; margin: auto;" />

``` r
ggplot(Ozone, aes(V3, V5)) + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-12-2.png" style="display: block; margin: auto;" />

# Box plot stats

``` r
# base R
boxplot.stats(df$`Weekend Gross`)
```

    ## $stats
    ## [1]  0.703  1.709  2.835 11.118 18.238
    ## 
    ## $n
    ## [1] 20
    ## 
    ## $conf
    ## [1] -0.489  6.160
    ## 
    ## $out
    ## [1] 31

``` r
# ggplot2
g <- ggplot(df, aes(1, `Weekend Gross`)) + geom_boxplot()
ggplot_build(g)$data[[1]]
```

    ##    ymin lower middle upper ymax outliers notchupper notchlower x PANEL
    ## 1 0.703  1.76   2.84  10.1 18.2       31       5.79     -0.117 1     1
    ##   group ymin_final ymax_final  xmin xmax xid newx new_width weight colour
    ## 1    -1      0.703         31 0.625 1.38   1    1      0.75      1 grey20
    ##    fill size alpha shape linetype
    ## 1 white  0.5    NA    19    solid

# Multiple box plots

``` r
library(ggplot2)
world <- read.csv("countries2012.csv")
ggplot(world, aes(x = CONTINENT, y = TFR)) + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/multboxplots-1.png" style="display: block; margin: auto;" />

# Multiple box plots

``` r
ggplot(world, aes(x = CONTINENT, y = TFR)) + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-14-1.png" style="display: block; margin: auto;" />

``` r
world %>% filter (TFR > 5) %>% filter(CONTINENT == "Asia") %>% select(COUNTRY, CONTINENT, TFR)
```

    ##       COUNTRY CONTINENT  TFR
    ## 1 Afghanistan      Asia 5.27
    ## 2 Timor-Leste      Asia 5.30

``` r
world %>% filter (TFR < 2) %>% filter(CONTINENT == "Oceania") %>% select(COUNTRY, CONTINENT, TFR)
```

    ##     COUNTRY CONTINENT  TFR
    ## 1 Australia   Oceania 1.92

# Reorder by median

``` r
g1 <- ggplot(world, aes(x = reorder(CONTINENT, -TFR, median),
                        y = TFR)) 
g1 + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-15-1.png" style="display: block; margin: auto;" />

# Reorder by maximum value

``` r
ggplot(world, aes(x = reorder(CONTINENT, TFR, max), 
                  y = TFR)) + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-16-1.png" style="display: block; margin: auto;" />

# Reorder by standard deviation

``` r
ggplot(world, aes(x = reorder(CONTINENT, TFR, sd), 
                  y = TFR)) + geom_boxplot()
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-17-1.png" style="display: block; margin: auto;" />

# Add overall median line

``` r
g1 + geom_boxplot() + 
  geom_hline(yintercept = median(world$TFR), color = "red")
```

<img src="BoxplotsPlus-code_files/figure-gfm/withmedian-1.png" style="display: block; margin: auto;" />

# Variable width box plots

``` r
g2 <- g1 + geom_boxplot(varwidth = TRUE)
g2
```

<img src="BoxplotsPlus-code_files/figure-gfm/varwidth-1.png" style="display: block; margin: auto;" />

# Add continent country count

``` r
library(dplyr)
tfrorderdesc <- world %>% group_by(CONTINENT) %>%
  summarize(count = n(), median = median(TFR)) %>% arrange(-median)
g2 + annotate("text", x=1:6, y = 8, 
             label = tfrorderdesc$count, color = "blue",
             size = 6) +
    ggtitle("count:") + theme_grey(14) +
    theme(plot.title = element_text(color = "blue"))
```

<img src="BoxplotsPlus-code_files/figure-gfm/varwidthnum-1.png" style="display: block; margin: auto;" />

# Horizontal boxplot

``` r
gb <- ggplot(world, aes(x = reorder(CONTINENT, TFR, median),
                        y = TFR)) + 
  geom_boxplot(varwidth = TRUE) +
  coord_flip() + theme_grey(14)
gb
```

<img src="BoxplotsPlus-code_files/figure-gfm/varwidthflip-1.png" style="display: block; margin: auto;" />

# Not for discrete data

``` r
library(likert)
data("pisaitems")
p <- pisaitems[1:100, 2:7] %>% 
  dplyr::mutate_all(as.integer) %>% 
  dplyr::filter(complete.cases(.))

boxplot(p, las = 1, main = "PISA data (scale: 1 - 4)",
        border = "blue")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-18-1.png" style="display: block; margin: auto;" />

Source: R likert::pisaitems dataset

# Multiple density histograms, ordered by median

``` r
gh <- ggplot(world, aes(x = TFR, y = ..density..)) + 
    geom_histogram(color = "blue", fill = "lightblue") +
facet_wrap(~reorder(CONTINENT, -TFR, median), nrow = 6, strip.position = "top") +
  theme(strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"))
gh
```

<img src="BoxplotsPlus-code_files/figure-gfm/dh-1.png" style="display: block; margin: auto;" />

# Boxplots vs. histograms

``` r
library(gridExtra)
grid.arrange(gb, gh, nrow = 1)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-19-1.png" style="display: block; margin: auto;" />

# Frequency polygon

``` r
ggplot(world, aes(x = GDP)) + 
  geom_histogram(fill = "white", color = "lightblue") +
  geom_freqpoly() + theme_grey(14)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-20-1.png" style="display: block; margin: auto;" />

# Density histogram

``` r
g <- ggplot(world, aes(x = GDP, y = ..density..)) + 
  geom_histogram(fill = "white", color = "lightblue")
g
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-21-1.png" style="display: block; margin: auto;" />

# Density curve

``` r
g + geom_density(color = "red")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-22-1.png" style="display: block; margin: auto;" />

# Density curve

``` r
g + geom_density(color = "red", adjust = .5) + ggtitle("adjust = .5")
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-23-1.png" style="display: block; margin: auto;" />

# Density curve: varying smoothing bandwidths

``` r
g + geom_density(color = "red", bw = 2000) +
  geom_density(color = "blue", bw  = 4000) +
  geom_density(color = "green", bw = 16000) +
  geom_density(color = "purple", bw = 1000)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-24-1.png" style="display: block; margin: auto;" />

# Density curve: varying smoothing bandwidths (`ggvis`)

``` r
library(ggvis)
world %>% ggvis(~GDP) %>%
    layer_densities(adjust = input_slider(.1, 5,
                                          label = "bandwidth"))
```

See also: <http://ggvis.rstudio.com/0.1/quick-examples.html#histograms>

# Density curves

``` r
library(dplyr)
america <- world %>% filter(grepl("America", CONTINENT))
ggplot(america, aes(x = GDP, color = CONTINENT, fill = CONTINENT)) + 
  geom_density(alpha = .2)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-26-1.png" style="display: block; margin: auto;" />

# Density curves

``` r
library(dplyr)
america <- world %>% filter(grepl("America", CONTINENT))
ggplot(america, aes(x = GDP, color = CONTINENT, fill = CONTINENT)) + 
  geom_density(alpha = .2, adjust = .5)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-27-1.png" style="display: block; margin: auto;" />

# Violin plots

``` r
ggplot(world, aes(x = CONTINENT,
                  y = GDP)) +
    geom_violin() + 
    ggtitle("Gross Domestic Product") +
    coord_flip() + theme_grey(14)
```

<img src="BoxplotsPlus-code_files/figure-gfm/violin-1.png" style="display: block; margin: auto;" />

# Violin plots, change bandwidth

``` r
ggplot(world, aes(x = CONTINENT,
                  y = GDP)) +
    geom_violin(adjust = 6) + 
    ggtitle("Gross Domestic Product") +
    coord_flip() + theme_grey(14)
```

<img src="BoxplotsPlus-code_files/figure-gfm/violin2-1.png" style="display: block; margin: auto;" />

# Violin plots, ordered by median

``` r
ggplot(world, aes(x = reorder(CONTINENT, GDP, median),
                  y = GDP)) +
    geom_violin() + 
    ggtitle("Gross Domestic Product") +
    coord_flip() + theme_grey(14)
```

<img src="BoxplotsPlus-code_files/figure-gfm/violin3-1.png" style="display: block; margin: auto;" />

# Box plot vs. violin plot

``` r
library(gridExtra)
g1 <- ggplot(world, aes(x = factor(1), y = TFR)) +
    geom_boxplot() + xlab("") + coord_flip()
g2 <- ggplot(world, aes(x = factor(1), y = TFR)) + 
    geom_violin() + xlab("") + coord_flip()
grid.arrange(g1, g2, ncol = 1)
```

<img src="BoxplotsPlus-code_files/figure-gfm/bvs-1.png" style="display: block; margin: auto;" />

# Ridgeline plot

<img src = "leisuretime.jpg" width = "400"/>

Source: <https://eagereyes.org/blog/2017/joy-plots>

Additional
resources:

<http://blog.revolutionanalytics.com/2017/07/joyplots.html>

<https://blogs.scientificamerican.com/sa-visual/pop-culture-pulsar-origin-story-of-joy-division-s-unknown-pleasures-album-cover-video/>

# Ridgeline plot inspiration

Jocelyn Bell discovers first radio pulsars, 1967

<img src = "pulsar.jpg" width = "400"/>

# Ridgeline plot

``` r
library(ggridges)
gr <- ggplot(world, aes(x = GDP, y = reorder(CONTINENT, -GDP,
                                       median))) + 
  geom_density_ridges(fill = "blue", alpha = .5)
gr
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-28-1.png" style="display: block; margin: auto;" />

# Ridgeline plot, change scale

``` r
ggplot(world, aes(x = GDP, 
                        y = reorder(CONTINENT, -GDP, median))) + 
  geom_density_ridges(fill = "blue", alpha = .5, scale = 1)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-29-1.png" style="display: block; margin: auto;" />

# Histogram vs. ridgeline

``` r
grid.arrange(gh, gr, nrow = 1)
```

<img src="BoxplotsPlus-code_files/figure-gfm/unnamed-chunk-30-1.png" style="display: block; margin: auto;" />

# Ridgeline vs. boxplot

<img src = "lenkieferboxplots.jpg" width = "800"/>

Source: <https://twitter.com/lenkiefer/status/916823350726610946>

# `ggridge` package

**CRAN** <https://CRAN.R-project.org/package=ggridges>

**Github** <https://github.com/clauswilke/ggridges>

**Package vignette(s)**
<https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html>

<https://cran.r-project.org/web/packages/ggridges/vignettes/gallery.html>

**Package manual**
<https://cran.r-project.org/web/packages/ggridges/ggridges.pdf>
