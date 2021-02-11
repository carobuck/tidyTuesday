Lifetime Wealth
================
Caro Buck
2/9/2021

## Lifetime Wealth + Race

For this week’s \#TidyTuesday, I’m trying something new by using a
Markdown doc to create my visualization. This week’s data looks at
wealth inequities, especially by race, with data from the [Urban
Institute](https://apps.urban.org/features/wealth-inequality-charts/)
and US Census. I’m going to look at differences in retirement amounts,
by race and year, with a custom/DIY ggplot2 theme.

Other things I learned about rendering markdowns in github; need to add
a special output to make it render properly in github.
([Ref](https://stackoverflow.com/questions/39814916/how-can-i-see-output-of-rmd-in-github))

## Reading in the Data + Loading Libraries

``` r
library(tidyverse)
retirement <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-09/retirement.csv')
```

Let’s get a glimpse of the data for what we’ll be working with. How many
races? How many years?

``` r
retirement %>%
  glimpse()
```

    ## Rows: 30
    ## Columns: 3
    ## $ year       <dbl> 1989, 1989, 1989, 1992, 1992, 1992, 1995, 1995, 1995,…
    ## $ race       <chr> "White", "Black", "Hispanic", "White", "Black", "Hisp…
    ## $ retirement <dbl> 32649.430, 5954.398, 7121.722, 36637.760, 7798.197, 5…

``` r
retirement %>% count(race)
```

    ## # A tibble: 3 x 2
    ##   race         n
    ##   <chr>    <int>
    ## 1 Black       10
    ## 2 Hispanic    10
    ## 3 White       10

``` r
retirement %>% summary()
```

    ##       year          race             retirement    
    ##  Min.   :1989   Length:30          Min.   :  5249  
    ##  1st Qu.:1995   Class :character   1st Qu.: 15021  
    ##  Median :2002   Mode  :character   Median : 21809  
    ##  Mean   :2002                      Mean   : 41411  
    ##  3rd Qu.:2010                      3rd Qu.: 44936  
    ##  Max.   :2016                      Max.   :157884

## Now let’s get to plotting!

First, just a basic ggplot2 to see what we’re working with:

``` r
retirement %>%
  ggplot(aes(x=year,y=retirement,color = race)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x='Year',y='Retirement Savings',color = 'Race',
       title = 'Average Retirement Savings by Race over Time',
       caption = 'Data sourced from the Urban Institute')
```

![](20210209_lifetimeWealth_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

I think we can make this a *little* more exciting visually; let’s make a
custom ggplot2 theme. I’ll start with the theme\_bw(), since I tend to
like that one and use it a lot for my plots.

Some of the helpful links for reference:
[this](https://joeystanley.com/blog/custom-themes-in-ggplot2) and
[this](https://www.andrewheiss.com/blog/2017/09/27/working-with-r-cairo-graphics-custom-fonts-and-ggplot/#:~:text=R%20and%20ggplot%20can%20create,font%20family%20for%20the%20plot).

Also, [this](https://github.com/hrbrmstr/hrbrthemes) is good reference
for some pre-built themes from the hrbrthemes R package (if I’m looking
for some variety in life).

What fonts are actually available on my computer? After some
exploration, let’s use Copperplate for this exercise.

``` r
library(systemfonts)
system_fonts()

theme_bougie <- function () { 
    theme_bw(base_size=12, base_family="Copperplate") %+replace% 
        theme(
            panel.background = element_rect(fill = 'darkseagreen1'),
            panel.border = element_rect(fill=NA,color = 'purple'),
            panel.grid = element_line(color = 'darkturquoise'),
            legend.position = 'bottom',
            legend.background = element_rect(fill = 'orange')
        )
}
```

Now let’s see how that theme looks on our chart:

``` r
retirement %>%
  ggplot(aes(x=year,y=retirement,color = race)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels = scales::dollar) +
  labs(x='Year',y='Retirement Savings',color = 'Race',
       title = 'Average Retirement Savings by Race over Time',
       caption = 'Data sourced from the Urban Institute') +
  theme_bougie() +
  ggsave('lifeWealth.png')
```

    ## Saving 7 x 5 in image

![](20210209_lifetimeWealth_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

Ha! So it’s not the most beautiful (or necessarily color accessible),
but it was fun to make something a little different and learn a new
technique!
