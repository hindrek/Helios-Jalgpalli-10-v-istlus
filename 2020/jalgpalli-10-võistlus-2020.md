Jalgpalli 10-võistlus 2020
================
Hindrek Teder
2020-07-18

## Sissejuhatus

Helios U17 grupi jalgpalli 10-võistlus toimus Piiri treeninglaagris.
Kokku osales 12 mängijat.

## Teegid

``` r
library(readxl)
library(janitor)
library(skimr)
library(tidyverse)
library(scales)
```

## Andmed

``` r
tulemused <- read_excel("jalgpalli 10-võistlus 2020.xlsx",
                        .name_repair = make_clean_names)
skim(tulemused)
```

|                                                  |           |
| :----------------------------------------------- | :-------- |
| Name                                             | tulemused |
| Number of rows                                   | 12        |
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
| nimi           |          0 |              1 |   9 |  15 |     0 |        12 |          0 |

**Variable type: numeric**

| skim\_variable     | n\_missing | complete\_rate |  mean |    sd |    p0 |   p25 |   p50 |   p75 |   p100 | hist  |
| :----------------- | ---------: | -------------: | ----: | ----: | ----: | ----: | ----: | ----: | -----: | :---- |
| jalaga\_koksimine  |          0 |              1 | 39.08 | 51.88 |  3.00 |  6.25 | 17.50 | 45.25 | 164.00 | ▇▁▁▁▁ |
| kaugusloomine      |          0 |              1 |  6.25 |  3.28 |  1.00 |  3.75 |  6.50 |  9.00 |  11.00 | ▇▅▅▇▅ |
| tribling           |          0 |              1 | 14.53 |  1.16 | 12.79 | 13.66 | 14.62 | 15.19 |  16.14 | ▇▂▇▅▇ |
| peaga\_koksimine   |          0 |              1 |  7.17 |  6.29 |  3.00 |  3.75 |  4.50 |  8.00 |  25.00 | ▇▂▁▁▁ |
| audivise           |          0 |              1 |  5.67 |  3.37 |  1.00 |  2.75 |  5.50 |  8.25 |  11.00 | ▇▃▃▃▃ |
| korgusloomine      |          0 |              1 |  4.06 |  0.25 |  3.56 |  3.83 |  4.18 |  4.26 |   4.27 | ▁▃▁▁▇ |
| latti\_loomine     |          0 |              1 |  0.25 |  0.45 |  0.00 |  0.00 |  0.00 |  0.25 |   1.00 | ▇▁▁▁▂ |
| palliga\_jooksmine |          0 |              1 | 12.98 |  1.06 | 11.83 | 12.16 | 12.53 | 13.97 |  14.75 | ▇▃▂▂▅ |
| penalt             |          0 |              1 |  1.42 |  1.08 |  0.00 |  0.75 |  1.50 |  2.00 |   3.00 | ▆▆▁▇▃ |
| tapsusloomine      |          0 |              1 |  2.00 |  1.04 |  0.00 |  1.75 |  2.00 |  2.25 |   4.00 | ▁▂▇▂▁ |

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
  select(ala, tulemus, nimi) %>% 
  arrange(ala)
```

<div class="kable-table">

| ala                | tulemus | nimi            |
| :----------------- | ------: | :-------------- |
| audivise           |  11.000 | Karl Kurs       |
| jalaga\_koksimine  | 164.000 | Hugo Salmar     |
| kaugusloomine      |  11.000 | Henry Tikk      |
| korgusloomine      |   4.266 | Henri Muug      |
| latti\_loomine     |   1.000 | Kaspar Rohtmets |
| latti\_loomine     |   1.000 | Kristjan Rüüson |
| latti\_loomine     |   1.000 | Ranel Lillemets |
| palliga\_jooksmine |  11.832 | Henri Muug      |
| peaga\_koksimine   |  25.000 | Henri Muug      |
| penalt             |   3.000 | Henry Tikk      |
| penalt             |   3.000 | Hugo Salmar     |
| tapsusloomine      |   4.000 | Hugo Salmar     |
| tribling           |  12.792 | Henri Muug      |

</div>

## Kokkuvõte

``` r
kokku <- punktid %>% 
  group_by(nimi) %>% 
  summarise(kokku = sum(punktid, na.rm = T)) %>% 
  arrange(desc(kokku))
```

    ## `summarise()` ungrouping output (override with `.groups` argument)

``` r
kokku
```

<div class="kable-table">

| nimi            |    kokku |
| :-------------- | -------: |
| Hugo Salmar     | 79.37494 |
| Henri Muug      | 73.08851 |
| Ranel Lillemets | 61.29353 |
| Kaspar Rohtmets | 55.83771 |
| Kristo Vaks     | 53.50956 |
| Karl Kurs       | 51.69425 |
| Henry Tikk      | 50.36067 |
| Markus Kirsel   | 41.01890 |
| Joonas Haug     | 37.75335 |
| Raul Augla      | 35.71357 |
| Kristjan Rüüson | 32.56337 |
| Roland Kruus    | 25.33693 |

</div>

``` r
kokku %>% 
  ggplot(aes(x = kokku, y = reorder(nimi, kokku))) +
  geom_col() +
  labs(y = "nimi")
```

![](jalgpalli-10-võistlus-2020_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
