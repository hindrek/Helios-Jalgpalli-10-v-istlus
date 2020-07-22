Jalgpalli 10-võistlus 2017
================
Hindrek Teder
2017-06-13

## Sissejuhatus

Helios 2005 grupi jalgpalli 10-võistlus toimus Piiri treeninglaagris.
Kokku osales 19 mängijat.

## Teegid

``` r
library(readxl)
library(skimr)
library(tidyverse)
library(scales)
```

## Andmed

``` r
tulemused <- read_excel("jalgpalli 10-võistlus 2017.xlsx")
skim(tulemused)
```

|                                                  |           |
| :----------------------------------------------- | :-------- |
| Name                                             | tulemused |
| Number of rows                                   | 19        |
| Number of columns                                | 11        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_   |           |
| Column type frequency:                           |           |
| character                                        | 1         |
| numeric                                          | 10        |
| \_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_\_ |           |
| Group variables                                  | None      |

Data summary

**Variable type: character**

| skim\_variable | n\_missing | complete\_rate | min | max | empty | n\_unique | whitespace |
| :------------- | ---------: | -------------: | --: | --: | ----: | --------: | ---------: |
| nimi           |          0 |              1 |  10 |  17 |     0 |        19 |          0 |

**Variable type: numeric**

| skim\_variable    | n\_missing | complete\_rate |  mean |    sd |    p0 |   p25 |   p50 |   p75 |  p100 | hist  |
| :---------------- | ---------: | -------------: | ----: | ----: | ----: | ----: | ----: | ----: | ----: | :---- |
| jalaga kõksimine  |          0 |           1.00 | 16.89 | 20.58 |  2.00 |  6.50 | 11.00 | 15.00 | 92.00 | ▇▂▁▁▁ |
| kauguslöömine     |          0 |           1.00 |  9.74 |  5.29 |  1.00 |  5.50 | 10.00 | 14.00 | 18.00 | ▇▆▇▇▇ |
| tribling          |          2 |           0.89 | 21.20 |  3.60 | 17.90 | 19.28 | 19.67 | 20.90 | 28.30 | ▇▃▁▁▂ |
| peaga kõksimine   |          0 |           1.00 |  4.89 |  3.11 |  2.00 |  3.00 |  4.00 |  5.00 | 16.00 | ▇▅▁▁▁ |
| audivise          |          0 |           1.00 | 10.00 |  5.63 |  1.00 |  5.50 | 10.00 | 14.50 | 19.00 | ▇▇▆▇▇ |
| kõrguslöömine     |          2 |           0.89 |  3.29 |  0.29 |  2.90 |  3.03 |  3.34 |  3.52 |  3.86 | ▇▅▆▃▅ |
| latti löömine     |          1 |           0.95 |  0.78 |  0.81 |  0.00 |  0.00 |  1.00 |  1.00 |  2.00 | ▇▁▆▁▃ |
| palliga jooksmine |          0 |           1.00 | 12.53 |  1.19 | 10.75 | 11.95 | 12.31 | 12.90 | 15.22 | ▅▇▇▁▂ |
| penalt            |          0 |           1.00 |  1.11 |  0.81 |  0.00 |  1.00 |  1.00 |  1.50 |  3.00 | ▃▇▁▃▁ |
| täpsuslöömine     |          0 |           1.00 |  1.42 |  0.90 |  0.00 |  1.00 |  1.00 |  2.00 |  3.00 | ▃▇▁▇▂ |

## Punktiarvestus

Iga ala eest oli võimalik teenida 1 kuni 10 punkti vastavalt tulemusele.
Ala tegemata jätmise korral punkte ei antud. Seega oli 10 ala pealt
võimalik kokku teenida maksimaalselt 100 punkti (10 alavõitu).

``` r
punktid <- tulemused %>% 
  pivot_longer(-nimi, names_to = "ala", values_to = "tulemus") %>% 
  mutate(punktid = if_else(ala %in% c("tribling", "palliga_jooksmine"), -tulemus, tulemus)) %>% 
  group_by(ala) %>% 
  mutate(punktid = rescale(punktid, c(1, 10))) %>% 
  ungroup()
```

## Alavõitjad

``` r
punktid %>% 
  group_by(ala) %>% 
  filter(punktid == max(punktid, na.rm = T)) %>% 
  select(ala, nimi, tulemus) %>% 
  arrange(ala)
```

<div class="kable-table">

| ala               | nimi              | tulemus |
| :---------------- | :---------------- | ------: |
| audivise          | Raido Villiko     |   19.00 |
| jalaga kõksimine  | Mihkel Sepp       |   92.00 |
| kauguslöömine     | Ramon Põldaru     |   18.00 |
| kõrguslöömine     | Ramon Põldaru     |    3.86 |
| latti löömine     | Kaarel Kaasik     |    2.00 |
| latti löömine     | Siim Oskar Liivla |    2.00 |
| latti löömine     | Gabriel Soome     |    2.00 |
| latti löömine     | Jan-Erik Kärner   |    2.00 |
| palliga jooksmine | Marcello Širikov  |   15.22 |
| peaga kõksimine   | Ramon Põldaru     |   16.00 |
| penalt            | Ramon Põldaru     |    3.00 |
| tribling          | Ramon Põldaru     |   17.90 |
| täpsuslöömine     | Randes Kanter     |    3.00 |
| täpsuslöömine     | Jasper Kanter     |    3.00 |

</div>

## Kokkuvõte

``` r
kokku <- punktid %>% 
  group_by(nimi) %>% 
  summarise(punktid = sum(punktid, na.rm = T)) %>% 
  arrange(desc(punktid)) %>% 
  rowid_to_column(var = "koht")
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
kokku
```

<div class="kable-table">

| koht | nimi              |  punktid |
| ---: | :---------------- | -------: |
|    1 | Ramon Põldaru     | 72.37383 |
|    2 | Mihkel Sepp       | 64.64553 |
|    3 | Raido Villiko     | 60.74807 |
|    4 | Gabriel Soome     | 57.02286 |
|    5 | Matis Rein        | 52.73407 |
|    6 | Siim Oskar Liivla | 51.69809 |
|    7 | Marcus Mosin      | 51.35825 |
|    8 | Rasmus Lodi       | 48.00777 |
|    9 | Karl Trahv        | 46.31515 |
|   10 | Jasper Kanter     | 46.13190 |
|   11 | Kaarel Kaasik     | 42.56865 |
|   12 | Hans Herzmann     | 42.13419 |
|   13 | Jan-Erik Kärner   | 38.16685 |
|   14 | Roland Kruus      | 37.55444 |
|   15 | Randes Kanter     | 36.74695 |
|   16 | Kristjan Rüüson   | 33.48077 |
|   17 | Marcus Tambets    | 30.54796 |
|   18 | Marcello Širikov  | 28.28654 |
|   19 | Robin Melk        | 23.13905 |

</div>

``` r
kokku %>% 
  ggplot(aes(x = punktid, y = reorder(nimi, punktid))) +
  geom_col() +
  labs(y = "nimi")
```

![](jalgpalli-10-võistlus-2017_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
